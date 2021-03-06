/** \file 

At one point, this was the bridge to the FORTRAN 77 code that was the basis of the
edit system, but now that that part has been entirely converted to C, it is more of an
eccentric list of interface functions. The conversion from SQL to the em matrix is here.

Further, the C code is intended for use as itself or via the R code. This also explains
the awkward interface functions, because R can only pass int*s, double*s, or char**s.

As of 4 May 2010, the main() function has been removed---it works only via PEP's config files.
*/

#include <stdbool.h>
#include "internal.h"

extern int speer_(void);

void generate_indices(char const *);

int edit_ct, errorcount, verbose, run_number, explicit_ct;
int *find_b, *find_e;
FILE *yyin;
int total_var_ct, *optionct;
char *database;
apop_data *settings_table, *ud_queries;
int max_lev_distance = 2;
int num_typos;
bool CTea=false; //Set to true iff using as a library without R.

apop_data *edit_grid;
edit_t **edit_grid_to_list;

/* The implicit edit code has been removed---it never worked. The last edition that had it was 
git commit 51e31ffeeb100fb8a30fcbe303739b43a459fd59
git-svn-id: svn+ssh://svn.r-forge.r-project.org/svnroot/tea@107 edb9625f-4e0d-4859-8d74-9fd3b1da38cb

get_key_float_list and get_key_float_list_tagged also made their last appearance there.
*/

int yyparse();

int lineno = 1;
int pass = 0; //The config file has to be scanned multiple times.
int has_edits = 0; //incremented in peptalk.y if there's a checks{} group in the config.
char *fname = "-stdin-";

/* This is what the parser calls on error. */
int yyerror(const char *s) { Tea_stopif(1, return 0, 0, "%s(%d): %s\n",fname,lineno,s); }

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

static int pull_index(char const *in_name){
    int intct=0;
    for (int i =0; used_vars[i].name; i++){
        if (!strcasecmp(used_vars[i].name, in_name))
            return intct;
        if (used_vars[i].type!='r') intct++; //reals don't enter the edit grid.
    }
    return -1;
}

/* If only one item changed from the last edit, and it's the same item that's been changing lately,
we can merge this edit into the last run.  In terms of the little machine below, that just means not 
switching to the 's' state and continuing in state 'e'.
This is never called when current_row==0.
*/
static char is_new_edit(apop_data *explicits, int current_row){
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

static void grow_grid(apop_data *edit_grid, int *em_i, int toc){
    (*em_i)++;         //add a row to edit_grid->matrix.
    edit_grid->vector = apop_vector_realloc(edit_grid->vector, *em_i);
    edit_grid->matrix = apop_matrix_realloc(edit_grid->matrix, *em_i, toc);
    edit_grid_to_list = realloc(edit_grid_to_list, sizeof(*edit_grid)* *em_i);
}

/* At this point, we have a list of tables representing each edit, in ud_queries.

   This little machine will do one of these things:
   --Bump forward to start a new edit    = 's'
   --Declare another field in the edit   = 'd'
   --Give another value in this edit     = 'e'

This replaces rdin3 in gen_ed52.f
*/
void db_to_em(void){
    if (!ud_queries) return;
    int current_row = 0;
    int current_col = 0;
    int current_explicit = 0;
    static char next_phase = 's';
    int em_i = 0; 
	char *ud_val;
    apop_data *d=NULL;

    int total_option_ct = 0;
    for (int i=0; i< total_var_ct; i++) total_option_ct +=optionct[i];

	while(1){
        //d = ud_explicits[current_explicit];
        if (next_phase == 's'){ //starting a new edit
            if (!edit_grid) {
                edit_grid = apop_data_alloc();
                free(edit_grid_to_list);
                edit_grid_to_list = NULL;
            }
            //We're only doing integer and text edits. If there's a real variable anywhere
            //along the row, then we'll use the sql-based edit system to make it work. 
            //Also, the DISCRETE system is ill-suited for handling "if var is null"
            //conditions. The reader is welcome to modify it to suit; meanwhile use SQL.
            for (int i=0; i< edit_list[current_explicit].var_ct; i++)
                if (edit_list[current_explicit].vars_used[i].type =='r'){
                    apop_text_alloc(edit_grid, edit_grid->textsize[0]+1, GSL_MAX(1, edit_grid->textsize[1]));
                    apop_text_add(edit_grid, edit_grid->textsize[0]-1, 0, edit_list[current_explicit].clause);
                    next_phase='s';
                    grow_grid(edit_grid, &em_i, total_option_ct);
                    gsl_vector_set(edit_grid->vector, em_i-1, 2); //2==use the SQL-based edit system
                    edit_grid_to_list[em_i-1] = edit_list + current_explicit;
                    goto Edited; //We've finished an edit and are stepping forward.
                }
            //else, standard discrete-indexed matrix

			if (!d) d = apop_query_to_text("%s", ud_queries->text[current_explicit][0]);
            Tea_stopif(d && d->error, return, 0, "query error setting up edit grid; edits after this one won't happen.");
            grow_grid(edit_grid, &em_i, total_option_ct);
            gsl_vector *a_row = Apop_rv(edit_grid, em_i-1);
            gsl_vector_set_all(a_row, -1); //first posn of a field==-1 ==> ignore.
            edit_grid_to_list[em_i-1] = edit_list + current_explicit;
            if (verbose) printf("Next edit.\n");

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
            int field = pull_index(d->names->text[current_col]);
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
            int field = pull_index(d->names->text[current_col]);
            ud_val = d->text[current_row][current_col];
            if (verbose) printf("Add edit.\tfield %i\t val %s\n", field, ud_val);
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
            current_col++; //bump forward by col, row, and/or table
            if (!d || current_col == d->textsize[1]){ //the !d may occur when you jump from the above goto
                current_col = 0;
                current_row++;
                if (!d || current_row == d->textsize[0]) {
                    //apop_data_free(ud_explicits[current_explicit]);
                    apop_data_free(d);
                    current_row = 0;
                    next_phase = 's';
                    current_explicit ++;
                    if (current_explicit == explicit_ct) {
                        if (verbose) printf("Done.\n");
                        edit_ct = em_i;
                        /*if (!apop_table_exists("edit_grid"))
                            apop_data_print(edit_grid, .output_name="edit_grid", .output_type='d');*/
                        return;
                    }
                } else next_phase = is_new_edit(d, current_row);
            } 
        }
    }
}


///// Here are functions to get keys from the key/value table generated from parsing the config.

/** Give me a key, in either one or two parts, and I'll give you a double in
   return. If the key is not found, return \c GSL_NAN (test via \c gsl_isnan()).
*/
double get_key_float(char const *part1, char const * part2){
     char *p = NULL;
     if (part1 && part2) Asprintf(&p, "%s/%s", part1, part2)
    double out = apop_query_to_float("select value from keys where key like '%s' order by count", 
                            p ? p : part1 ? part1 : part2);
    if (p) free(p);
    return out;
}

double get_key_float_tagged(char const *part1, char const * part2, char const *tag){
    if (!tag || !strlen(tag)) return get_key_float(part1,part2);
    return apop_query_to_float("select value from keys where "
									    "key like '%s%s%s' and tag ='%s'",
										XN(part1),
										(part1 && strlen(part1)&&part2 && strlen(part2))?"/":"",
										XN(part2), XN(tag));
}

/** Give me a key, in either one or two parts, and I'll give you an apop_data
set with a text element filled with your data. Access via 
   \c returned_data->text[0][0]
   \c returned_data->text[1][0], ....
   \c apop_data_free(returned_data);

\return  If the key is not found, return \c NULL.
*/
apop_data* get_key_text(char const *part1, char const *part2){
    return apop_query_to_text("select value from keys where "
									    "key like '%s%s%s' order by count",
										XN(part1),
										(part1 && strlen(part1)&&part2 && strlen(part2))?"/":"",
										XN(part2));
}

apop_data* get_key_text_tagged(char const *part1, char const *part2, char const *tag){
    if (!tag || !strlen(tag)) return get_key_text(part1,part2);
    return apop_query_to_text("select value from keys where "
									    "key like '%s%s%s' and tag like '%%%s%%'",
										XN(part1),
										(part1 && strlen(part1)&&part2 && strlen(part2))?"/":"",
										XN(part2), XN(tag));
}

/** Give me a key in two parts and I'll give you a char* with your data.
   If the key is not found, return \c NULL.
*/
char* get_key_word(char const *part1, char const *part2){
    apop_data* almost_out = get_key_text(part1, part2);
    char *out=NULL;
    if (almost_out) out = strdup (almost_out->text[0][0]);
    apop_data_free(almost_out);
    return out;
}

char* get_key_word_tagged(char const *part1, char const *part2, char const *tag){
    if (!tag || !strlen(tag)) return get_key_word(part1, part2);
    apop_data* almost_out = get_key_text_tagged(part1, part2, tag);
    char *out=NULL;
    if (almost_out) out = strdup (almost_out->text[0][0]);
    apop_data_free(almost_out);
    return out;
}

static apop_data* get_sub_key(char const *part1,char const *part2){
    apop_data* out = apop_query_to_text("select distinct key from keys where key like '%s%s%s/%%' order by count", XN(part1), (part1 && strlen(part1) && part2 && strlen(part2)) ? "/":"", XN(part2));
	if (out)
		for(int i=0; i < out->textsize[0]; i++)//shift past the input key and slash.
			out->text[i][0] += strlen(part1)+ (part2 ? strlen(part2)+1:0) + 1; //leaks; don't care.
    return out;
}

int vcount;

static void add_key_text(char const *group, char const *key, char const *value){
    apop_query("insert into keys values('%s%s%s', '', %i, '%s')", XN(group), 
                                (group && key) ? "/": "", XN(key), vcount++, value);
}

void set_key_text(char const *group, char const *key, char const *value){
    apop_query("delete from keys where key='%s%s%s'", XN(group),
                                (group && key) ? "/": "", XN(key));
    add_key_text(group, key, value);
}

void set_key_text_for_R(char **group, char **key, char **value){
    apop_query("delete from keys where key='%s%s%s'", XN(*group), 
                                (*group && *key) ? "/": "", XN(*key));
    add_key_text(*group, *key, *value);
}

void start_over(){ //Reset everything in case this wasn't the first call
    extern int file_read, impute_is_prepped;
    free(edit_list); 
    reset_ri_ext_table();
    apop_data_free(edit_grid);
    edit_list = NULL;
    if (used_vars){
        for (int i=0; used_vars[i].name; i++)
            free(used_vars[i].name);
        free(used_vars);
    }
    apop_data_free(ud_queries);
    in_out_tab_reset();
    used_vars = NULL;
    fname = "-stdin-";
    lineno = 1;
    pass = 0;
    edit_ct = 0;
    query_ct = 0;
    has_edits = 0;
    file_read = 0;
    errorcount = 0;
    explicit_ct = 0;
    total_var_ct = 0;
    impute_is_prepped = 0;
    num_typos = 0;
}

void setup_findxbe(){
    //first, compress the optionct, which currently has zeros for real vars.
    int last_nonzero =-1;
    for (int i=0; i< total_var_ct; i++)
        if (used_vars[i].type!='r')
            optionct[++last_nonzero] = optionct[i];
    for (int i=++last_nonzero; i<total_var_ct; i++)
            optionct[i] = 0;

    find_b = malloc(sizeof(int)*total_var_ct);
    find_e = malloc(sizeof(int)*total_var_ct);
    find_b[0] = 1;
    find_e[0] = optionct[0];
    for(int i =1; i< total_var_ct; i++){
        find_b[i] = find_e[i-1]+1;
        find_e[i] = find_e[i-1]+optionct[i];
    }
}

void init_edit_list(){
	if (total_var_ct){
        setup_findxbe();
        db_to_em();
	}
}


/* TeaKey(verbose, <<<Specifies whether the user would like TEA's behind-the-scenes work to be printed to screen when functions are executed. It is irrelevant what value you assign to verbose, assigning it any value is enough to turn it on.>>>)
 */
void verbosity(int *verbosityLevel){ verbose = *verbosityLevel;}

void read_spec(char **infile, char **dbname_out){
    start_over();
    fname = strdup(*infile);
    yyin = fopen (fname, "r");
    Tea_stopif(!yyin, return, 0, "Trouble opening spec file %s.", fname);
    pass=0;
    begin_transaction();
    Tea_stopif(yyparse(), return, 0, "TEA was unable to read your spec file. This is most likely"
            " due to the fact that you didn't specify a database at the header of the file.");
        //fill keys table. Note that yyparse() returns 0 when parsing is successful.
    
    if(get_key_word("verbose", NULL) != NULL) verbose = 2;
    num_typos = check_levenshtein_distances(max_lev_distance);

    // SPEER bounds generating routine
    char *bfld_exits = get_key_word("SPEERparams", "BFLD");
    if( bfld_exits ) {
        int gr = genbnds_();
        if (gr==0) speer_();
    }

    //Generating indices for ID
    apop_data *tags=apop_query_to_text("%s", "select distinct tag from keys");
    if (tags){
        for (int i=0; i< *tags->textsize;i++){
            if (apop_query_to_float("select count(*) from keys where "
                            "tag='%s' and key like 'input/%%'", *tags->text[i]))
                    generate_indices(*tags->text[i]);
            in_out_row_add(*tags->text[i]);
        }
        in_out_recode_fix();
        apop_data_free(tags);
    }

    do_recodes();

    pass++;
    rewind(yyin); //go back to position zero in the config file
	lineno=1;
    yyparse();   //use the parser to assemble edits
    //Apop_assert(!errorcount, "%i errors. Please fix and re-run.\n", errorcount);
	dbname_out[0] = strdup(database);
    commit_transaction();
}

/** Used in /tea/c/tests/levenshtein_test.c to test check_levenshtein_distances()
  */
int get_num_typos(){ return num_typos; }

void get_key_count_for_R(char **group,  char **key, char **tag, int *out, int *is_sub){
	apop_data *dout = *is_sub 
                        ? get_sub_key(group ? *group : NULL, key ? *key : NULL)
                        : get_key_text_tagged(group ? *group : NULL, key ? *key : NULL, (tag ? *tag:NULL));
	if (!dout){
	    *out = 0;
	    return;
	}
    *out = dout->textsize[0];
    //apop_data_free(dout);
}                                                             

void get_key_text_for_R(char **group, char **key, char **tag, char **out, int *is_sub){
	apop_data *dout = (*is_sub)
    	                ? get_sub_key(*group, key ? *key : NULL)
	                    : get_key_text_tagged(*group, key ? *key :NULL, (tag ? *tag:NULL));
	if (!dout) return;
    for (int i=0; i< dout->textsize[0]; i++)
        out[i] = strdup(dout->text[i][0]);
    //apop_data_free(dout);
} 

int transacting;
void begin_transaction(){
    if (transacting == 0) apop_query("begin;");
    transacting++;
}

void commit_transaction(){
    bool started_in_transaction = (transacting > 0);
    if (transacting > 0) transacting--;
    if (transacting==0 && started_in_transaction)
        apop_query("commit;");
}

//Do we need to use the edit check interface, and if so, what is the column type?
//type = '\0' means not in the index of variables to check.
char get_coltype(char const* invar){
    if (!used_vars) return '\0';
    for (int v=0; used_vars[v].name; v++)
        if (!strcasecmp(invar, used_vars[v].name))
            return used_vars[v].type;
    return '\0';
}


//For opening a database connection from R
void db_open(char **in){apop_db_close(); apop_db_open(*in);}

void qxprintf(char **q, char *format, ...){
    va_list ap;
    char *r = *q;
    va_start(ap, format);
    Tea_stopif(vasprintf(q, format, ap)==-1, , 0, "Trouble writing to a string.");
    va_end(ap);
    free(r);
}
