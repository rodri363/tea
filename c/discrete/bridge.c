/** \file 

At one point, this was the bridge to the FORTRAN 77 code that was the basis of the
edit system, but now that that part has been entirely converted to C, it is more of an
eccentric list of interface functions. The conversion from SQL to the em matrix is here.

Further, the C code is intended for use as itself or via the R code. This also explains
the awkward interface functions, because R can only pass int*s, double*s, or char**s.

As of 4 May 2010, the main() function has been removed---it works only via PEP's config files.
*/

#include <apop.h>
#include "tea.h"
#include "internal.h"

int edit_ct, nflds, errorcount, verbose, run_number, explicit_ct;
int *find_b, *find_e;
FILE *yyin;
int total_option_ct, total_var_ct, *optionct;
char *database;
apop_data *settings_table, *ud_queries;

void implicit_edits();
void db_to_em();
int yyparse();

int lineno = 1;
int pass = 0; //The config file has to be scanned multiple times.
int has_edits = 0; //incremented in peptalk.y if there's a checks{} group in the config.
char *fname = "-stdin-";

int yyerror(const char *s) { Apop_assert_c(0, 0, 0, "%s(%d): %s\n",fname,lineno,s); }

void get_costs(double * restrict out){ 
    for (size_t i=0; i< total_var_ct; i++) 
        out[i]= used_vars[i].weight; 
}

/* This file does most of the initial processing of a table. There are many steps.
Following recipe format, I'll list the ingredients, then give the steps.

* a table in the db for every variable and each recode. I currently never delete
		them, so they get to live there as cruft. These tables convert from
		ri=rowid in database to ext=external, (or ud=user defined)

* A C struct with info on every edit, named ud_explicits. It's an array of apop_data sets.

* A C struct with info on every variable used, used_vars

* A query to generate a view, based on the input table and the recode info.

* The input table, a copy of the input table, and a working view of the copy


--read the declarations, to generate a list of interesting variables.
	--each variable gets its own table in the database. I currently never delete
		them, so they get to live there as cruft. These tables convert from
		ri=rowid in database to ext=external, (or ud=user defined)

--get a table of recodes. 
	--Recodes are in the key table right now, and it's just a parsing problem to 
				turn them into clauses for SQL.
	--add each recode to the list of interesting variables.

--get the list of consistency checks. This is ud_explicits. That is, these are 

--generate the em table via the list of consistency checks. The jewel there is
db_to_em, with its several supporting functions; see the notes below.

-----Actually read the input file. This may be already in the db, or it may be
read via text file. It happens on demand---the first time that a procedure requires
knowing the data, the read-in happens. This is often the recode step.
*/


/**
Each query produces a (apop_data) table of not-OK values, in the
 to_sql function. We add that to the list of tables from user-defined edit rules,
 ud_explicits (explicits in user-designated, not rowid, notation). Having
 generated the list, the function below gets called by (formerly) FORTRAN to supply
 edits, item by item.

 As an extra detail, we can squash a table like
 1 2
 1 3
 8 2
 8 5

 down to

 1 (2,3)
 8 (2,5)
 */

int pull_index(char const *in_name){
    for (int i =0; i< total_var_ct; i++)
        if (!strcasecmp(used_vars[i].name, in_name))
            return i;
    return -1;
}

/* If only one item changed from the last edit, and it's the same item that's been changing lately,
we can merge this edit into the last run.  In terms of the little machine below, that just means not 
switching to the 's' state and continuing in state 'e'.
This is never called when current_row==0.
*/
char is_new_edit(apop_data *explicits, int current_row){
    static int last_changed = -1;
	for (int i=0; i< explicits->textsize[1]; i++)
		if (!apop_strcmp(explicits->text[current_row][i], explicits->text[current_row-1][i])){
            if (last_changed == -1)
                last_changed = i;
            else if (last_changed != i){
                    last_changed = -1;
                    return 's';  //diff in the wrong place. Start over.
                }
	}
	return 'e';
}

apop_data *edit_grid; //generated here; useed by consistency_check.

		
/* At this point, we have a list of tables representing each edit.

   This little machine will do one of these things:
   --Bump forward to start a new edit    = 's'
   --Declare another field in the edit   = 'd'
   --Give another value in this edit     = 'e'

This replaces rdin3 in gen_ed52.f
*/
void db_to_em(void){
    if (!ud_queries) return;

	//This needs to be rewritten for ud_queries
    /*if (!ud_explicits){
        if (has_edits) printf("All edits gave null results, so I have no failed edits.\n");
        return;
    }*/
    int current_row = 0;
    int current_col = 0;
    int current_explicit = 0;
    static char next_phase = 's';
    int em_i = 0, field; 
	char *ud_val;
    apop_data *d=NULL;
	while(1){
        //d = ud_explicits[current_explicit];
        if (next_phase == 's'){ //starting a new edit
			if (!d) d = apop_query_to_text("%s", ud_queries->text[current_explicit][0]);
            em_i++;         //add a row to edit_grid->matrix.
            if (!edit_grid) edit_grid = apop_data_alloc();
            edit_grid->vector = apop_vector_realloc(edit_grid->vector, em_i);
            edit_grid->matrix = apop_matrix_realloc(edit_grid->matrix, em_i, total_option_ct);
            Apop_row(edit_grid, em_i-1, a_row)
            gsl_vector_set_all(a_row, -1); //first posn of a field==-1 ==> ignore.
            if (verbose) printf("Next edit.\n");

            //We're only doing integer and text edits. If there's a real variable anywhere
            //along the row, then we'll use the sql-based edit system to make it work. 
            for (int i=0; i< edit_list[current_explicit].var_ct; i++)
                if (edit_list[current_explicit].vars_used[i].type =='r'){
                    apop_text_alloc(edit_grid, edit_grid->textsize[0]+1, GSL_MAX(1, edit_grid->textsize[1]));
                    apop_text_add(edit_grid, edit_grid->textsize[0]-1, 0, edit_list[current_explicit].clause);
                    next_phase='s';
                    gsl_vector_set(edit_grid->vector, em_i-1, 2); //2==use the SQL-based edit system
                    goto Edited; //a use of goto! This is where we've finished an edit and are stepping forward.
                }
            //else, standard discrete-indexed matrix
            next_phase = 'd';
            current_col = 0;
            gsl_vector_set(edit_grid->vector, em_i-1, 1); //1==plain old discrete-valued constraint.
        }
        if (next_phase == 'd'){ //declarations
            if (!d){  //this is a blank; continue to next.
                next_phase='s';
                apop_text_alloc(edit_grid, edit_grid->textsize[0]+1, GSL_MAX(1, edit_grid->textsize[1]));
                apop_text_add(edit_grid, edit_grid->textsize[0]-1, 0, edit_list[current_explicit].clause);
                current_explicit ++;
                if (current_explicit == explicit_ct) {
                    if (verbose) 
                        printf("Done.\n");
                    edit_ct = em_i;
                    return;
                }
                continue;
            } //else:
            field = pull_index(d->names->text[current_col]);
            current_col++;
            if (current_col == d->textsize[1]){
                next_phase = 'e'; 
                current_col = 0;
            }
            if (verbose) printf("Declare.\tfield %i\n", field);
            //set this field's set of bits to false
            for(int  kk = find_b[field]-1; kk< find_e[field]; kk++)
              gsl_matrix_set(edit_grid->matrix, em_i-1, kk, 0);
        }
        if (next_phase == 'e'){ //Else, an edit.
            field = pull_index(d->names->text[current_col]);
            ud_val = d->text[current_row][current_col];
            if (verbose) printf("Add edit.\tfield %i\t val %s\n", field, ud_val);
			/*int ri_position = ri_from_ext(d->names->text[current_col], ud_val);
            gsl_matrix_set(edit_grid->matrix, em_i-1,find_b[field]-1+ri_position-1, 1); */
            gsl_matrix_set(edit_grid->matrix, em_i-1,find_b[field]-1+atoi(ud_val)-1, 1);

            int k, already_recorded = 0;
            for (k=0; em_i <= edit_grid->textsize[0] && k < edit_grid->textsize[1]; k++)
                if (apop_strcmp(edit_list[current_explicit].clause, edit_grid->text[em_i-1][k])){
                    already_recorded++;
                    break;
                }
            if (!already_recorded){
                apop_text_alloc(edit_grid, GSL_MAX(edit_grid->textsize[0], em_i),
                                           GSL_MAX(edit_grid->textsize[1], k+1));
                edit_grid->text[em_i-1][k] = strdup(edit_list[current_explicit].clause);
            }
            
            Edited:
            current_col++;//bump forward by col, row, and/or table
            if (!d || current_col == d->textsize[1]){ //the !d occurs when you jump from the above goto
                current_col = 0;
                current_row++;
                if (!d || current_row == d->textsize[0]) {
                    //apop_data_free(ud_explicits[current_explicit]);
                    apop_data_free(d);
                    current_row = 0;
                    next_phase = 's';
                    current_explicit ++;
                    if (current_explicit == explicit_ct) {
                        if (verbose) 
                            printf("Done.\n");
                        edit_ct = em_i;
                        /*if (!apop_table_exists("edit_grid"))
                            apop_data_print(edit_grid, .output_file="edit_grid", .output_type='d');*/
                        return;
                    }
                } else
                    next_phase = is_new_edit(d, current_row);
            } 
        }
    }
}


/* \page Lib Library versions of the routine.

If you are using this system as a library inside a larger program, then these functions
will help. #include "tea.h" in your code, and see tea.h for overview documentation.
*/

/** Give me a key, in either one or two parts, and I'll give you a double in
   return. If the key is not found, return \c GSL_NAN (test via \c gsl_isnan()).
*/
double get_key_float(char const *part1, char const * part2){
     char *p = NULL;
     if (part1 && part2)
         asprintf(&p, "%s/%s", part1, part2);
    double out = apop_query_to_float("select value from keys where key like '%s' order by count", 
                            p ? p : part1 ? part1 : part2);
    if (p) free(p);
    return out;
}

/** Give me a key, in either one or two parts, and I'll give you an apop_data
 * set with a text element filled with your data. Access via 
   \c returned_data->text[0][0]
   \c returned_data->text[1][0], ....
   If the key is not found, return \c NULL.
*/
apop_data* get_key_text(char const *part1, char const *part2){
    apop_data* out = apop_query_to_text("select value from keys where "
									    "key like '%s%s%s' order by count",
										XN(part1),
										(part1&&part2)?"/":"",
										XN(part2));
    return out;
}

apop_data* get_key_text_tagged(char const *part1, char const *part2, char const *tag){
    if (!tag || !strlen(tag)) return get_key_text(part1,part2);
    apop_data* out = apop_query_to_text("select value from keys where "
									    "key like '%s%s%s' and tag like '%%%s%%'",
										XN(part1),
										(part1&&part2)?"/":"",
										XN(part2), XN(tag));
    return out;
}

/** Give me a key in two parts and I'll give you a char* with your data.
   If the key is not found, return \c NULL.
*/
char* get_key_word(char const *part1, char const *part2){
    apop_data* almost_out = get_key_text(part1, part2);
    char *out=NULL;
    if (almost_out)
        out = strdup (almost_out->text[0][0]);
    apop_data_free(almost_out);
    return out;
}

char* get_key_word_tagged(char const *part1, char const *part2, char const *tag){
    if (!tag || !strlen(tag)) return get_key_word(part1,part2);
    apop_data* almost_out = get_key_text_tagged(part1, part2, tag);
    char *out=NULL;
    if (almost_out)
        out = strdup (almost_out->text[0][0]);
    apop_data_free(almost_out);
    return out;
}

int breakdown(char *inlist, double **outlist){
    char *saveptr;
    int ct=0;
    *outlist = NULL;
    if (!inlist)
        return 0;
    for (char* str1 = inlist; ; str1 = NULL) {
        char *token = strtok_r(str1, ",;\t ", &saveptr);
        if (token == NULL)
            break;
        *outlist = realloc(*outlist, sizeof(double)*++ct);
        (*outlist)[ct-1] = atof(token);
    }
    free(inlist);
    return ct;
}

int get_key_float_list(char *part1, char * part2, double **outlist){
    return breakdown(get_key_word(part1, part2), outlist);
}

int get_key_float_list_tagged(char *part1, char * part2, char *tag, double **outlist){
    if (!tag || !strlen(tag)) return get_key_float_list(part1, part2, outlist);
    return breakdown(get_key_word_tagged(part1, part2, tag), outlist);
}

apop_data* get_sub_key(char const *part1){
    apop_data* out = apop_query_to_text("select distinct key from keys where key like '%s/%%' order by count", part1);
	if (out)
		for(int i=0; i < out->textsize[0]; i++)//shift past the input key and slash.
			out->text[i][0] += strlen(part1)+1; //leaks; don't care.
    return out;
}

int vcount;

void add_key_text(char const *group, char const *key, char const *value){
    apop_query("insert into keys values('%s/%s', '', %i, '%s')", group, key, vcount++, value);
}

void set_key_text(char const *group, char const *key, char const *value){
    apop_query("delete from keys where key='%s/%s'", group, key);
    add_key_text(group, key, value);
}

void set_key_text_for_R(char **group, char **key, char **value){
    apop_query("delete from keys where key='%s/%s'", *group, *key);
    add_key_text(*group, *key, *value);
}

void start_over(){ //Reset everything in case this wasn't the first call
    extern int file_read, impute_is_prepped;
    extern apop_data *pre_edits;
    free(edit_list); 
    reset_ri_ext_table();
    apop_data_free(edit_grid);
    apop_data_free(pre_edits);
    edit_list = NULL;
    for (int i=0; i< total_var_ct; i++)
        free(used_vars[i].name);
    free(used_vars);
    apop_data_free(ud_queries);
    used_vars = NULL;
    fname = "-stdin-";
    lineno = 1;
    pass =
    nflds =
    edit_ct =
    query_ct =
    has_edits =
    file_read =
    errorcount =
    explicit_ct =
    total_var_ct =
    total_option_ct =
    impute_is_prepped = 0;
}

void setup_findxbe(){
    find_b[0] = 1;
    find_e[0] = optionct[0];
    for(int i =1; i< total_var_ct; i++){
        find_b[i] = find_e[i-1]+1;
        find_e[i] = find_e[i-1]+optionct[i];
    }
}

void init_edit_list(){
	if (nflds){
		find_b = malloc(sizeof(int)*nflds);
		find_e = malloc(sizeof(int)*nflds);
        setup_findxbe();
        db_to_em();
        implicit_edits();
	}
    if (edit_list) {
        apop_assert(database, "Please declare a database using 'database: your_db'.");
        apop_table_exists("editinfo", 'd');
        apop_query("create table editinfo (row, edit, infotype, val1, val2);");
        if (!apop_table_exists("alternatives"))
            apop_query("create table alternatives (run, id, field, value);");
        apop_query("delete from alternatives where run = %i", run_number);
    }
}

void verbosity(){ verbose = 1-verbose;}

void read_spec(char **infile, char **dbname_out){
    start_over();
    fname = strdup(*infile);
    yyin = fopen (fname, "r");
    apop_assert(yyin, "Trouble opening spec file %s.", fname);
    pass=0;
    yyparse();  //fill keys table

    apop_data *recode_tags = apop_query_to_text("select distinct tag from keys "
            " where key like 'recode%%' or key like 'group recodes%%' order by count");
    if (recode_tags){
        if (recode_tags->textsize[0]==1) make_recode_view(NULL, (char*[]){"both"});
        else for (int i=0; i< *recode_tags->textsize; i++){
            Apop_assert(
                    !make_recode_view(recode_tags->text[i], //pointer to list of char*s.
                        ( (i==0) ? (char*[]){"first"}
                        : (i==*recode_tags->textsize-1) ? (char*[]){"last"}
                        : (char*[]){"middle"})),
                    "Error in recode production.");
        }
        apop_data_free(recode_tags);
    }

    pass++;
    rewind(yyin); //go back to position zero in the config file
	lineno=1;
    yyparse();   //use the parser to assemble edits
    //Apop_assert(!errorcount, "%i errors. Please fix and re-run.\n", errorcount);
    nflds = total_var_ct;
	dbname_out[0] = strdup(database);
    join_tables();
	commit_transaction();
}

void get_key_count_for_R(char **group,  char **key, char **tag, int *out, int *is_sub){
	apop_data *dout = *is_sub 
                        ? get_sub_key(group ? *group : NULL)
                        : get_key_text_tagged(group ? *group : NULL, *key, (tag ? *tag:NULL));
	if (!dout){
	    *out = 0;
	    return;
	}
    *out = dout->textsize[0];
    apop_data_free(dout);
}                                                             

void get_key_text_for_R(char **group, char **key, char **tag, char **out, int *is_sub){
	apop_data *dout;
	if(*is_sub)
    	    dout = get_sub_key(*group);
	else
	    dout = get_key_text_tagged(*group, *key, (tag ? *tag:NULL));
	if (!dout)
	    return;
    for (int i=0; i< dout->textsize[0]; i++)
        out[i] = strdup(dout->text[i][0]);
    //apop_data_free(dout);
} 

int transacting;
void begin_transaction(){
    if (!transacting) apop_query("begin;");
    transacting++;
}

void commit_transaction(){
    if(transacting > 0) transacting--;
    if (!transacting) apop_query("commit;");
}
