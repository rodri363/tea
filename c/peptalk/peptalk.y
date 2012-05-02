%{

/* \file 
This is the main grammar for parsing the SQL-style input files to the edit
system. The grammar is at the top of the file, and you can see that
certain patterns trigger certain C functions, which appear after the
grammar.

I will assume you have had a look at the user-side documentation,
included in the R manual pages. 

There are about four types of operation:

--setting a key/value pair, which is put into the keys table. There are two forms:
    key:value
    #and
    key{
        value1
        value2
    }

--Declaring a new variable, via the "fields" group.
    For each new variable, I create a new table with a single column,
    where both the table and the column have the variable name. I fill
    the table with the valid values specified on the command line.

--Adding a consistency check
    The hard part here is that I need to work out what is a variable,
    and call the vars using the table.var form (where the table and var
    name are identical, as above.

--A pre-edit, of the form A => B (a subset of consistency checks. This implies two steps:
	* Rewrite this as update table set B were A.
	* Adding A as a standard old edit.
	The only annoying part here is storing A for later use.

The big magic trick to the setup is that each declaration produces one table
for one variable. Let the declarations say 
|x 1-2
|y 1-5; 
then we'll produce two new tables: one named x, whose sole column will be named x, and which will have two rows; one table named y, with a column named y, with values 1 to five.

A query over a set of variables will output the cross product of all combinations---that's just how SQL works, and we can use that to produce a comprehensive list of variables that pass/fail an edit.

Say the user adds the constraint
x >= y.

We'll convert that to 
select *
from x, y
where x.x >= y.y

which will produce the output

x y
---
1 1
2 1
2 2

and that's (almost) exactly the form we need to feed into the edit system. 

We keep a list of variables that have been used so far. That list includes a marker for the last query that used the variable, because we need to add a variable to the list of tables in the from clause exactly once for each query.


But wait---let's make this more complicated: recodes are artificial variables generated by
the user, which will exist in a view but not the original table or declarations. This gets
done after the parsing is done, and besides, we don't want to impose an order on the
segments of the config. 

So: the file is parsed in two passes.
Pass zero:   key table filled
            variables declared

then, recodes generate more variables, and the view table is set

Pass one:   generate edits

The functions here are all run by the yyparse() function. To see the context in which that lives, see bridge.c

*/

//int yydebug=1;

#include <stdlib.h>
#include <apop.h>
//#include "../tea.h"
#include "tea.h"
#include "discrete.h"
#include <string.h>
#define YYSTYPE char*
#define YYMAXDEPTH 100000
    YYSTYPE last_value = 0;
extern int yylex(void);
int yyerror(const char *s) ;
void add_keyval(char *, char*);
void add_var(char *var, int, char);
void add_to_num_list_seq(char *min, char*max);
void add_to_num_list(char *v);
void extend_key(char *in);
void reduce_key();
void store_right(char*); 
void add_check(char *);
void moreblob(char **, char *, char *);

used_var_t *used_vars;
edit_t *edit_list;
char *var_list, *current_key, parsed_type;
int val_count, preed2;
apop_data *pre_edits;
extern int pass, has_edits, file_read;
extern char  *nan_marker;

%}
//tokens delivered from lex
%token SPACE NUMBER DECLARATION THEN EOL TEXT ',' ':' OPENKEY CLOSEKEY '$' '-' '*' TYPE

%%
list: list item 
    | item
    ;

item: keyval
	| keygroup    
    | EOL
    | SPACE
    | declaration     /*flex eats the { }s for the declaration group*/
    | error
    ;

keyval: blob ':' blob  {extend_key($1); add_keyval(current_key, $3); reduce_key($1);} ;

keygroup: blob OPENKEY {extend_key($1);} keylist CLOSEKEY {reduce_key($1);} ;

keylist: keylist keylist_item 
       | keylist_item
       ;

keylist_item: preedit 
           | keyval
           | keygroup
           | EOL
           | error   {warning("Trouble parsing an item [$1] in the list of fields", $1);}
		   | blob { if (apop_strcmp(current_key, "checks")) {add_check($1);}
                   else add_keyval(current_key, $1);}
           ;

preedit  : blob {add_check($1); preed2=1;} THEN blob {store_right($4);preed2=0;} EOL ;

declaration: DECLARATION TEXT optionalcolon TYPE {add_var($2,0, parsed_type);} numlist
										{add_to_num_list(nan_marker ? strdup(nan_marker): NULL);}
           | DECLARATION TEXT optionalcolon {add_var($2,0, 'c');} numlist
										{add_to_num_list(nan_marker ? strdup(nan_marker): NULL);}
           | DECLARATION 
           ;

optionalcolon: ':'
             | optionalcolon SPACE
             |
             ;

numlist : num_item
        | numlist ',' num_item
        |
        ;

num_item : NUMBER '-' NUMBER  {add_to_num_list_seq($1, $3);}
         | NUMBER             {add_to_num_list($1);}
         | TEXT               {add_to_num_list($1);}
         | '*'               {add_to_num_list("*");}
         |'$' NUMBER           {used_vars[total_var_ct-1].weight = atof($2);}
         | error   			{warning("Error in numeric list around [%s].", $1);}
         ;

blob : blob blob_elmt {moreblob(&$$,$1,$2);}
     | blob_elmt {moreblob(&$$,"",$1);}
     | error
     ;

blob_elmt : TEXT 
         | NUMBER
         | ','   {$$=","}
         | '*'   {$$="*"}
         | '-'   {$$="-"}
         | '$'   {$$="$"}
         | SPACE {$$=" "}
         ;
%%

#include <apop.h>
#include <string.h>
void xprintf(char **q, char *format, ...); //impute/parse_sql.c
#define XN(in) ((in) ? (in) : "")          //same.

void extend_q(char **, char*, char*);

int total_var_ct, total_option_ct, *optionct, query_ct;
char *current_var, *datatab, *database, *nan_marker=NULL, *preed, *last_preed;
char *tag = NULL;
double *costs;

//Declarations. Create a new table for each variable,
//then just read in values as given.

char * strip(const char *in){
  static char    stripregex[1000] = "";
  char    w[]     = "[:space:]\n";
  apop_data *d;
  	if (!strlen(stripregex)){
      sprintf(stripregex, "[%s\"]*([^%s]|[^%s]+.*[^%s]+)[%s\"]*", w,w,w,w,w);
    }
	if (!apop_regex(in, stripregex, &d))
        return NULL;
	char* out= strdup(d->text[0][0]);
    apop_data_free(d);
    return out;
}

static void set_database(char *dbname){  //only on pass 0.
    if (verbose) printf("opening database %s.\n", dbname);
    Apop_assert(!apop_db_open(dbname), "failed to open database %s", dbname);
    database= strdup(dbname);
    apop_table_exists("keys", 'd');
    apop_table_exists("variables", 'd');
	begin_transaction();  //the commit is in bridge.c:read_spec.
    apop_query("create table keys (key, tag, count, value); "
               "create table variables (name);");
}

void add_keyval(char *key, char *val){ 
    if (pass != 0)
        return;
	char *skey = strip(key);
	char *sval = strip(val);
    if (!sval) return;
	if (pass==0 && apop_strcmp(skey, "database"))
		set_database(sval);
	Apop_assert(database, "The first item in your config file needs to be \"database:db_file_name.db\".");
	apop_query("insert into keys values (\"%s\", \"%s\", %i, \"%s\")", skey, XN(tag), ++val_count, sval);
	free(skey); free(sval);
}

void add_var(char *var, int is_recode, char type){
    if (pass !=0 && !is_recode)
        return;
	/* set current_var
		create the table in the db
		set default cost
		prep the optionct list
		add it all to used_vars
	*/
    if (is_recode) //recodes may have already been declared.
        for (int i=0; i< total_var_ct; i++)
            if (apop_strcmp(var, used_vars[i].name)) return;
    free(current_var);
	if (verbose)
	    printf("A var: %s (type %s)\n", var, type=='n' ? "no type" : type=='i' ? "integer" : "unknown");
    current_var = strdup(var);
    if (type!='r'){
        apop_table_exists(current_var, 'd');
		/*ahh here it is, table was always text*/
		if(type=='i')
	        apop_query("create table %s (%s integer); create index indx%svar on %s(%s)",
                                    var, var,var,var,var);
		else
	        apop_query("create table %s (%s text); create index indx%svar on %s(%s)",
                                    var, var,var,var,var);
        apop_query("insert into variables values ('%s')", var);
        optionct = realloc(optionct, sizeof(int)*(total_var_ct+1));
        optionct[total_var_ct] = 0;
    }
    costs = realloc(costs, sizeof(double)*(total_var_ct+1));
	costs[total_var_ct] = 1;
	total_var_ct++;
	used_vars = realloc(used_vars, sizeof(used_var_t)*(total_var_ct));
	used_vars[total_var_ct-1] = (used_var_t) {.name=strdup(var), .weight=1, .last_query=-1, .type=type};
}


/* 
one variable, multiple values
		case var
		when a1 then val
		when a2 then val2
		else valn
		end

general version
		case 
		when var=a1 then val
		when var2=a2 then val2
		else valn
		end

*/

char *make_case_list(char *in){
    //take in 2, 3,4-8 ,10, 12 and spit out where 2 or 3 or 4 or 5 or 6 ... then val
	apop_data *d;
	apop_regex(in, "([^,]+)(,|$)", &d);
	char *out = NULL;
	for (int j=0; j < d->textsize[0]; j++) {
		apop_data *splitme=NULL;
		if (apop_regex(d->text[j][0], "^[ \t]*([0-9]+)[ \t]*-[ \t]*([0-9]+)[ \t]*$", &splitme))
			for (int i=atoi(splitme->text[0][0]); i<=atoi(splitme->text[0][1]); i++)
				xprintf(&out, "%s%s %i", XN(out), out ? " or ":"", i);
		else 
			xprintf(&out, "%s%s %s", XN(out), out ? " or ":"", d->text[j][0]);
	} 
	apop_data_free(d);
	return out;
}

void add_to_num_list(char *v){
    if (pass !=0) return;
    Apop_assert_c(!(parsed_type=='r' && (v && strlen(strip(v))>0)), , 1,
         "I ignore ranges for real variables. Please add limits in the check{} section.");
    if (parsed_type=='r') return; //no warning--just go.
	if (apop_strcmp(v, "*")){
		text_in();
		apop_data *invalues = apop_query_to_text("select distinct %s from %s", current_var, datatab);
		for (int i=0; i< invalues->textsize[0]; i++)
			add_to_num_list(invalues->text[i][0]);
		apop_data_free(invalues);
	}
	int already_have = 
			(v && apop_query_to_float("select count(*) from %s where %s='%s'", 
												current_var, current_var, v))
			||
			(!v && apop_query_to_float("select count(*) from %s where %s is null", 
													current_var, current_var));
    //if not already an option, add it in:
    if(!already_have){
		if (!v) apop_query("insert into %s values (NULL);", current_var);
		else {
			char *tail;
			if (parsed_type == 'i') {
                int val = strtod(v, &tail);
            }
			if (parsed_type != 'i' || *tail != '\0'){ //then this can't be parsed cleanly as a number
				apop_query("insert into %s values ('%s');", current_var, v);
                if (parsed_type == 'i' && *tail != '\0')
                    Apop_notify(0, "The variable %s is set to integer type, but I can't "
                                "cleanly parse %s as a number. Please fix the value or add"
                                "a type specifier of 'cat'.", current_var, v);
            } else
				apop_query("insert into %s values (%s);", current_var, v);
		}
        optionct[total_var_ct-1]++;
        total_option_ct ++;
    }
	free(v);
}

void add_to_num_list_seq(char *min, char*max){
    if (pass !=0) return;
    Apop_assert_c(parsed_type!='r', , 1,
         "I ignore ranges for real variables. Please add limits in the check{} section.");
    for (int i = atoi(min); i<=atoi(max);i++){
        apop_query("insert into %s values (%i);", current_var, i),
		optionct[total_var_ct-1]++,
        total_option_ct ++;
    }
	free(min);
	free(max);
}

//Queries.

void moreblob(char **out, char* so_far, char *more){
    //if (pass==0 && !apop_strcmp(current_key, "checks")) {
    if (!apop_strcmp(current_key, "checks") || preed2) {
		asprintf(out, "%s%s", XN(so_far), more);
        return;
    }
    if(pass==1 && apop_strcmp(current_key, "checks")){
        /*If you're here, you're in query mode and extending a query.
        Queries are mostly just read in verbatim, but if we find a
        variable X, then we have to do three things: 
        add the table X to the from clause of the query, 
        check that the X table exists, and 
        rewrite X as X.X in the where clause that the user gave us.  */
        int is_used=0;
        int is_in_list=0;

        //First, handle the edit_list, which stores the edit w/o modification.
        if (!edit_list) {
            edit_list = malloc(sizeof(edit_t));
            edit_list[0] = (edit_t){};
        }	
        if (!edit_list[query_ct].clause){ //always allocate one unit ahead, so this test will work.
            edit_list = realloc(edit_list, sizeof(edit_t)* (query_ct+2));
            edit_list[query_ct+1] = (edit_t){};
        }
        edit_t *el = &edit_list[query_ct]; //Just an alias.
        xprintf(&el->clause, "%s%s", XN(el->clause), more);

        //OK, now take care of the variable lists.
        for (int i=0; i< total_var_ct; i++)
            if (used_vars && !strcasecmp(more, used_vars[i].name)){
                is_in_list++;
                if (used_vars[i].last_query == query_ct)
                    is_used++;
                used_vars[i].last_query = query_ct;

                //if not in the vars_used list, add it on
                int already_used =0;
                for (int j=0; j< el->var_ct; j++)
                    if (apop_strcmp(more, el->vars_used[j].name)){
                        already_used++;
                        break;
                    }
                if (!already_used){
                    el->vars_used = realloc(el->vars_used, sizeof(used_var_t)*++el->var_ct);
                    el->vars_used[el->var_ct-1] = used_vars[i];
                }
            }

        //Finally, extend the query
        xprintf(&preed, "%s%s", XN(preed), more); //needed for pre-edits; harmless elsewhere
        if (!is_in_list){ //then it's not a variable.
            asprintf(out, "%s%s", XN(so_far), more);
            return;
        }
        /* \key{input/output table} The table that the raw data is written to. Also used 
            as a reference for some later keys, like the recodes. */
        if (!is_used){ //it's a var, but I haven't added it to the list for this q.
            if (!var_list)
                var_list = strdup(more);
            else
                asprintf(&var_list, "%s, %s", more, var_list);
            if (!datatab)
                datatab = get_key_word("input","output table");
            apop_assert(datatab, "I need the name of the data table so I can set up the recodes."
                                 "Put an 'output table' key in the input segment of the spec.");
            if (!apop_table_exists(more))
                apop_query( "create table %s as "
                            "select distinct %s.rowid as %s from %s;"
                            , more, more, more, datatab);
        }
        asprintf(out, " %s %s.%s ", XN(so_far), more, more);
    }
}

void extend_key(char *key_in){
    char *inkey = strip(key_in);
    char* open_brace_posn = strchr(inkey, '[');
    char* end_brace_posn = strchr(inkey, ']');
    if (open_brace_posn && end_brace_posn){
        *end_brace_posn = '\0';  //cut off the inkey at the end brace.
        tag= strip(open_brace_posn+1);//copy everything after the open-brace (minus surrounding whitespace)
        *open_brace_posn = '\0'; //cut off the inkey at the open brace.
    }
    //Push a key tag on the stack
    if (!current_key)
        current_key = strip(inkey);
    else 
        xprintf(&current_key, "%s/%s", current_key, strip(inkey));
}

void reduce_key(char *in){
    //Pop a key tag off the stack
    Apop_assert(current_key, "The {curly braces} don't seem to match. Please "
            "check your last few changes.");
    int i;
    for (i=strlen(current_key); i > -1; i--)
        if (current_key[i] == '/') break;
    if (i == -1){
        free(current_key);
        current_key = NULL;
    }
    else current_key[i]= '\0'; //crop the string to that point.

    char *inkey = strip(in);
    if (inkey){
        char* open_brace_posn = strchr(inkey, '[');
        if (open_brace_posn) tag[0]='\0'; //I'm assuming a single tag at a time.
    }   
}


void one_recode_to_string(char const *thisrc, char **clauses, int *is_formula, int *has_else, int doedits){
    apop_data *one_rc = NULL;
    apop_regex(thisrc, "[ \t]*(([^|]|\\|\\|)+)[ \t]*(\\||$)", &one_rc);//break into delimited parts
    if (!one_rc){ //Then it's not of the form "something | [...]", so treat as a formula.
        (*is_formula)++;
        *clauses = strdup(thisrc);
        return;
    } //else:
    if (*one_rc->textsize==2){
        //the "category | condition" version
        xprintf(clauses, "%s when %s then '%s'\n", XN(*clauses), one_rc->text[1][0], strip(one_rc->text[0][0]));
    } else if (*one_rc->textsize==1){ 
        //the default category.
        apop_assert(!*has_else, "You have a recode with two default categories.");
        xprintf(clauses, "%s else '%s'\n", 
            *clauses ? *clauses : "when 0 then 0", strip(one_rc->text[0][0]) );
        (*has_else)++;
    } else
        Apop_assert(0, "This line doesn't parse right as a recode: %s", thisrc);
    if (doedits) add_to_num_list(strip(one_rc->text[0][0]));
    apop_data_free(one_rc);
}

/* \key database The database to use for all of this. It must be the first thing on your line.
 
I need it to know where to write all the keys to come.

\key id Provides a column in the data set that provides a unique identifier for each
observation.
Some procedure need such a column; e.g., multiple imputation will store imputations in a
table separate from the main dataset, and will require a means of putting imputations in
their proper place. Other elements of TEA, like flagging for disclosure avoidance, use the
same identifier.


\key recodes New variables that are deterministic functions of the existing data sets.
There are two forms, one aimed at recodes that indicate a list of categories, and one
aimed at recodes that are a direct calculation from the existing fields.
For example (using a popular rule that you shouldn't date anybody who is younger than
(your age)/2 +7),

\begin{lstlisting}[language=]
recodes { 
    pants {
        yes | leg_count = 2
        no  |                   #Always include one blank default category at the end.
    }

    youngest_date {
        age/2 + 7
    }
}
\end{lstlisting}

You may chain recode groups, meaning that recodes may be based on previous recodes. Tagged
recode groups are done in the sequence in which they appear in the file. [Because the
order of the file determines order of execution, the tags you assign are irrelevant, but
I still need distinct tags to keep the groups distinct in my bookkeeping.]

\begin{lstlisting}
recodes [first] {
    youngest_date: (age/7) +7        #for one-line expressions, you can use a colon.
    oldest_date: (age -7) *2
}

recodes [second] {
    age_gap {
        yes | spouse_age > youngest_date && spouse_age < oldest_date
        no  | 
    }

}
\end{lstlisting}

If you have edits based on a formula, then I'm not smart enough to set up the edit table
from just the recode formula. Please add the new field and its valid values in the \c
fields section, as with the usual variables.

If you have edits based on category-style recodes, I auto-declare those, because the
recode can only take on the values that you wrote down here.

\key {group recodes} Much like recodes (qv), but for variables set within a group, like
eldest in household.
For example,
\begin{lstlisting}[language=]
group recodes { 
    group id column: hh_id
    eldest: max(age)
    youngest: min(age)
    household_size: count(*)
    total_income: sum(income)
    mean_income: avg(income)
}
\end{lstlisting}
*/

void recodes(char **key, char** tag, char **outstring, char **intab){
// All this function does is produce a query string. text_in/recodes then runs the query.
// **key may be "recodes" or "group recodes".
    apop_data *all_recodes;
    *outstring=NULL;
    if(tag && strlen(*tag)>0)
        all_recodes = apop_query_to_text("select distinct key from keys where "
                "(key like '%s%%' and key not like '%s%%no checks')"
                " and tag ='%s'",*key,*key, XN(tag[0]));
    else
        all_recodes = apop_query_to_text("select distinct key from keys where "
                "(key like '%s%%' and key not like '%s%%no checks')"
                " order by tag", *key, *key);
    if (!all_recodes || !all_recodes->textsize[0]) return;
    for(int i=0; i < all_recodes->textsize[0]; i++) // "recodes/var1" ==> "var1"
        all_recodes->text[i][0] += strlen(*key)+1;
    if (verbose) apop_data_show(all_recodes);

    int new_vars = apop_query_to_float("select count(*) from (select distinct key from keys where "
                                "(key like '%s%%' and key not like '%s%%no checks') %s%s%s "
                                , *key, *key, tag ? "and tag='":"", tag ? *tag:"", tag ? "')": ")");
    apop_data *editcheck = apop_query_to_text("select value from keys where "
                "key like '%s%%no checks' %s%s%s"
                , *key, tag ? "and tag='":"", tag ? *tag:"", tag ? "'": " ");

    if (!new_vars) return; //No recodes for the given **key.
    for (int i=0; i < new_vars; i++){
        char *varname = all_recodes->text[i][0]; //just an alias
        char comma = ' ';
        apop_data *recode_list = get_key_text(*key, varname);
        Apop_assert(recode_list && recode_list->textsize[0],
                        "%s looks like a recode field, but I can't parse "
                        "its recodes. Please check on this.", varname);
        
        int doedits = 0;
        if (editcheck)
            for (int e=0; doedits && e<editcheck->textsize[0]; e++)
                if (apop_regex(editcheck->text[e][0], varname))
                    doedits= 1;

        char *clauses= NULL;
        int has_else = 0, is_formula=0;
        if (doedits) add_var(varname, 1, 'n');
        if (verbose)
            printf("%s recode list size: %i\n",varname,recode_list->textsize[0]);
        for (int j=0; j < recode_list->textsize[0]; j++)
            one_recode_to_string(*recode_list->text[j], &clauses, 
                                   &is_formula, &has_else, doedits);

        //clauses is now the core of a variable definition. Wrap it and add it to the list.
        if (!is_formula && !has_else) asprintf(&clauses, "%s else null\n", clauses);
        comma = ',';		
        if (is_formula)
            xprintf(outstring, "%s%c %s as %s\n", XN(*outstring), comma, strip(clauses), varname);
        else
            xprintf(outstring, "%s%c case %s end as %s\n", XN(*outstring), comma, strip(clauses), varname);
    }
    if (verbose) printf("recode string: %s\n", *outstring);
    apop_data_free(editcheck);
}

/* This function is for post-parsing. it actually executes the list of pre-edits generated
by store_right below.  */

void do_preedits(char **datatab){
    //takes in pointer-to-string for compatibility with R.
	if (pre_edits)
		for (int i=0; i < pre_edits->textsize[0]; i++)
			apop_query("update %s %s", *datatab, pre_edits->text[i][0]);
}

/* I store a list of pre-edits in preedits, visible anywhere discrete.h is present.
   They're read here, and then executed using do_preedits.  */
void store_right(char *fix){ 
    if (pass !=1) return;
    int ts = pre_edits ? pre_edits->textsize[0] : 0;
    pre_edits = apop_text_alloc(pre_edits, ts+1, 1);
    apop_text_add(pre_edits,ts,0, " set %s where %s;", fix, last_preed);
    free(last_preed);
}

apop_data *ud_queries = NULL; //ud=user-defined.
extern int explicit_ct;

/* All the extend_q work above produces a list of variables (and thus
a list of tables, since we're naming tables and variables identically),
and a where clause. Here, we join those into a proper query, and run.
*/
void add_check(char *this_query){
    if (pass !=1 || !this_query || !strip(this_query)) 
        return;  //only after declaration pass; no blanks
    has_edits=1;
	apop_assert(var_list, 
				"The query \n%s\ndoesn't use any declared variables, which is a problem. "
                "Please check your typing/syntax, and make sure you've declared the variables you'd "
                "like me to check.\n", this_query);
    char *select = NULL;

	//select part1.rowid as part1, var2.rowid as var2 ... 
	char comma = ' ';
    for (int i=0; i< edit_list[query_ct].var_ct; i++){
		xprintf(&select, "%s%c %s.rowid as %s", XN(select), comma,
				edit_list[query_ct].vars_used[i].name, edit_list[query_ct].vars_used[i].name);
		comma =',';
	}

	ud_queries = apop_text_alloc(ud_queries, (ud_queries ?  ud_queries->textsize[0] : 0) +1, 1);
	apop_text_add(ud_queries, ud_queries->textsize[0]-1, 0, "select distinct %s from %s where %s", 
							select, var_list, this_query);
    if (preed) {
        last_preed = strdup(preed);
        preed[0]='\0';
    }
    free(var_list); var_list=NULL;
    query_ct++;
    explicit_ct++;
}
