#define _GNU_SOURCE
#include <time.h>
#include "internal.h"
#include <setjmp.h>
#include <stdbool.h>

void init_edit_list();
void xprintf(char **q, char *format, ...); //in ../impute/parse_sql.c

/*   TeaKEY(checks, <<<Where the user specifies on which variables she would like to perform consistency checks. The parameters for the variables declared in checks are obtained from input/types.>>>)
 */

/** DISCRETE, by Winkler et al, had the goal of finding the smallest change on the
  smallest subset of the records that would pass all edits. Our goal is to report to the
  caller all records that fail any edit, and all possible values that could be
  imputed---a sort of maximal edit set.  Thus the rewrite. 
*/

static int prune_edits(apop_data *d, int row, int col, void *us){ //callback for the fn below
    bool *usable_sql = us;
    return usable_sql[row];
}

/* We may be given a subset of the complete record; don't check edits that mention a field 
   that was not provided in the subset.
   Also, if (field is in promised_nan list + record has a NULL here + no preedit) => skip.
*/
static void check_for_all_vars(bool *usable_sql, char ** const restrict*oext_values,
                                                        bool const *promised_nans){
    for (int r=0; r< edit_grid->vector->size; r++){
        if(apop_data_get(edit_grid, r, -1) != 2) {
            usable_sql[r] = false;
            continue; 
        }
        usable_sql[r] = true;
        for (int i=0; i< edit_grid_to_list[r]->var_ct; i++){
            bool found=false;
            for (int j=0; j< total_var_ct; j++){
                if (*oext_values[j] && **oext_values[j]=='\0') continue;
                if (!strcasecmp(edit_grid_to_list[r]->vars_used[i].name, used_vars[j].name)){
                    found=true;
                    if (promised_nans[j] && !*oext_values[j] && !edit_grid_to_list[i]->pre_edit)
                        usable_sql[r]=false;
                    break;
                }
            }
            if (!found || !usable_sql[r]) {
                usable_sql[r] = false;
                break;
            }
        }
    }
}

//""  =this field doesn't enter our edit checks.
//NULL=use for checks, and val is null
static bool use_elmt (char const *str){ return !str || *str!='\0'; }

static void sqlify(char ** const restrict* oext_values){
    /*qstring will hold a query to generate a sample table, like "create table tea_test(a,
    b); insert into tea_test values('left', 'right');"*/
    char *qstring, *insert, comma=' ';
    Asprintf(&qstring, "create table tea_test(");
    Asprintf(&insert, "insert into tea_test values(");
    for (int i=0; i< total_var_ct; i++){
        if (!use_elmt(*oext_values[i])) continue;
        if (!*oext_values[i]) 
            xprintf(&insert, "%s%c NULL ", insert, comma);
        else 
            xprintf(&insert, "%s%c'%s' ", insert, comma, *oext_values[i]);
        xprintf(&qstring, "%s%c'%s' ", qstring, comma, used_vars[i].name);
        comma=',';
    }
    xprintf(&qstring, "%s numeric); %s);", qstring, insert);
    apop_query("%s", qstring);
    free(insert);
    free(qstring);
}


/* Up until revision 3b72363, there was a lot of verbiage to check whether a preedit
   actually changed anything. We now assume that they do. The only action that happens 
   after a preedit is that the entire consistency check reruns, so if somebody wrote
   a preedit that somehow leaves the record unchanged, we only lose a few cycles. */

static bool run_preedit(char *** oext_values, char const *preed){
    apop_query("update tea_test %s", preed);
    apop_data *newvals = apop_query_to_text("select * from tea_test");

printf("> > %s\n", preed);
apop_data_print(newvals);
printf("< <\n");

    int ctr=0; //order is the same as oext_values; just have to find the entering fields.
    for (int octr=0; octr< total_var_ct; octr++){
        if (!use_elmt(*oext_values[octr])) continue;

        //Found the right field. Write it if it changed.
        char *postval = newvals->text[0][ctr];
        char *preval = *oext_values[octr];
        bool postval_is_null = !strcmp(postval, apop_opts.nan_string);
        if ((preval && !strcmp(preval, postval)) 
            ||(!preval && postval_is_null)){
                ctr++;
                continue; //no change.
        }
printf("%s: [%s] →→ [%s]\n", used_vars[ctr].name, XN(preval), XN(postval));
        if (postval_is_null) *oext_values[octr] = NULL;
        else                 Asprintf(oext_values[octr], postval);
        ctr++;
    }

    apop_data_free(newvals);
    return true; //assume something changed; see notes above.
}

//call iff there are SQL edits (or preedits) to be checked.
static int check_a_record_sql(char *** oext_values, int **ofailures,
                         int *last_run_preedit,
                         int preedit_to_run,
                         bool *gotta_start_over, bool const *promised_nans
                         ){
    int out = 0;
    bool usable_sql[edit_grid->vector->size];
    check_for_all_vars(usable_sql, oext_values, promised_nans);
    begin_transaction();
    if (!apop_table_exists("tea_test")) sqlify(oext_values); //else, we're redoing after a preedit.

    //TO DO: ingomring promised_nans is a bug 
    if (!ofailures && *last_run_preedit==INT_MAX){   //just want pass-fail ==> run a single yes/no query
        char *q = apop_text_paste(edit_grid, .between=") or (", .after=")",
              .before= "select count(*) from tea_test where (", .prune=prune_edits,
              .prune_parameter=usable_sql);
        if (strlen(q) != strlen("select count(*) from tea_test where ()"))//any relevant sql?
            out += apop_query_to_float("%s", q);
        free(q);
    } else {
        bool pre_edits_changed_something = false;
        for (int i=0; i< edit_grid->vector->size; i++){
            if (preedit_to_run==i) break; //discrete preedit; no earlier sql preedit found.

            char *has_preed = edit_grid_to_list[i]->pre_edit;

            edit_t *this_list_item = edit_grid_to_list[i];
            if (!usable_sql[i]) continue;

            int fails = usable_sql[i] && apop_query_to_float("select count(*) from tea_test where (%s)",
                                                                *edit_grid->text[i]);
            if (fails){
                if (ofailures)
                    for (int i=0; i<total_var_ct; i++){
                        if (!use_elmt(*oext_values[i])) continue;
                        for (int j=0; j< this_list_item->var_ct; j++)//is this var. used in this edit?
                            if (!strcasecmp(this_list_item->vars_used[j].name, used_vars[i].name))
                                {(*ofailures[i])++; break;}
                    }
                if (has_preed &&  i > *last_run_preedit) {preedit_to_run=i; break;}
            }
        }
    }

    if (preedit_to_run >=0){
        run_preedit(oext_values, edit_grid_to_list[preedit_to_run]->pre_edit);
        *last_run_preedit = preedit_to_run;
        *gotta_start_over = true;
    }

    apop_table_exists("tea_test", 'd');
    commit_transaction();
    return out;
}

/* A field where all elements in em are T isn't really a part of the edit---it's just a placeholder. */
static int entering (int const row, int const rec){
    for (int i = find_b[rec]-1; i< find_e[rec]; i++)
        if (!gsl_matrix_get(edit_grid->matrix, row, i))
            return 1;
    return 0;
}

static void report_failure(int i, int const *rowfailures){
    if (verbose){
        printf("Failed edits:\n");
        for (int m=0; m< edit_grid->textsize[1]; m++)
            if (strlen(edit_grid->text[i][m]))
                printf("\t%s\n", edit_grid->text[i][m]);
        printf("Because:\n");
        for (int m=0; m< total_var_ct; m++)
            if (rowfailures[m])
                printf("\t%s = %s\n", used_vars[m].name,
                  ext_from_ri(used_vars[m].name, rowfailures[m]));
        printf("\n");
    }
}

/** Check each edit for a failure, meaning every T in the record matches a T in the edit (i.e.
  a row in the em matrix).

   Once we find a record T that matches an edit F (we verify that the record does 
not have the same value as the edit in that column), we know that the record 
passes the edit; move on to the next.

  If the input array *failures is NULL, then we're in brief mode: first time we find a
  failed edit, then exit reporting non-passing.

  If failures is an array to be filled (of size nsize: the number of fields in
  em), then check all edits, and mark down every field involved in any failure.
  There is one slot per field (i.e., it is unrelated to any subset of records
  requested by the user).

  Some fields are currently NULL because we promise to impute them later. If a 
  promised field is NULL, and there is no preedit associated, then skip that edit.
*/
static int check_a_record_discrete(int const * restrict row,  int **failures,
                    bool *has_sql_edits, int *preedit_to_run, int last_run_preedit,
                    bool const *promised_nans){
    int rowfailures[total_var_ct];
    int out = 0;
    if (failures)   //is this initialization necessary?
        for (int i=0; i< total_var_ct; i++) 
            if (*failures[i]!=-1) *failures[i]=0;

    if (!edit_grid) return 0;

    for (int i=0; i < edit_grid->matrix->size1; i++){
        if (gsl_vector_get(edit_grid->vector, i) == 2) {//this row has real values in it; skip.
            //I should check that all of the variables used in the query are in the list
            //sent to consistency_check here, to eliminate some redundant calls to check_a_record_sql.
            *has_sql_edits = true; 
            continue;
        }
        int rowfailed = 1; //each row is guilty until proven innocent.
        int has_actual_failed_fields=0;
        char *has_preed = edit_grid_to_list[i]->pre_edit;
        memset(rowfailures, 0, sizeof(int)*total_var_ct);
        int rec = 0;
        for (int j =0; j < edit_grid->matrix->size2; j++){
            if (row[j] == -1){
                if (gsl_matrix_get(edit_grid->matrix, i, j)==-1)
                    j = find_e[rec]-1;  //this is a dummy record, so fast-forward to end of record.
                else { //then we can't test this edit: a needed field is missing from row[].
                    rowfailed = 0;
                    break;
                }
                //else, row[j]==-1 && edit grid has a -1
            } else if (row[j]){
                if (j==find_e[rec]-1 //this field value==NULL; see the declaration parser in peptalk.y
                        && promised_nans[rec] && !has_preed){
                    rowfailed = 0;
                    break;
                }
                if (gsl_matrix_get(edit_grid->matrix, i, j)) {
                    if (entering(i, rec)){
                        rowfailures[rec]= j-find_b[rec]+2;//use the record value as a marker.
                        has_actual_failed_fields++;
                    }
                } else {
                    rowfailed = 0;
                    break; //out of the j-loop.
                }
				j = find_e[rec]-1;//only one true val per record, so fast-forward to end of record.
            }
            if (find_e[rec]-1==j) //at end of record?
                rec++;
        }
        if (rowfailed && has_actual_failed_fields){
            out = 1;
            if (failures){
                for(int k= 0; k < total_var_ct; k++)
                    *failures[k] += !!rowfailures[k]; //adds one or zero.
                report_failure(i, rowfailures);
            }
            if (has_preed && i > last_run_preedit){ //making fwd progress on preedits
                *preedit_to_run = i; //don't run it until you know no SQL preedits precede this.
                return out;
            } 
        }
    }
    return out;
}

/* There had been a clever recursive routine to generate a list of all possible
   alternatives. In every application we have, we've found that it's easier to just
   re-draw until we have a passing entry than to exhaustively generate the list of
   possiblities.  It last appeared in commit c11ad596. */


// Generate a record in DISCRETE's preferred format for the discrete-valued fields,
// and a query for the real-valued.
static void fill_a_record(int *record, int record_width, char ** const restrict *oext_values, long int id){
    for (int rctr=0; rctr < record_width; rctr++)
        record[rctr]=-1;   //-1 == ignore-this-field marker
    int rctr=0; //i is the index for oext_values or used_vars; rctr for record.
    for (int i=0; i < total_var_ct; i++){
        if (!*oext_values[i] || 
                (*oext_values[i] && **oext_values[i]=='\0') ||used_vars[i].type=='r')
            continue;
        int ri_position = ri_from_ext(used_vars[i].name, *oext_values[i]);
        if (ri_position == -100) continue;  //This variable wasn't declared ==> can't be in an edit.
        for(int  kk = find_b[rctr]-1; kk< find_e[rctr]; kk++)
            record[kk] = 0;
        Tea_stopif(ri_position == -1 , return, 0, "I couldn't find the value %s in your "
                "declarations for the variable %s. Please remove the error from the data or "
                "add that value to the declaration, then restart the program so I can rebuild "
                "some internal data structures.", *oext_values[i], used_vars[i].name);
        int bit = find_b[rctr]-1 + ri_position-1;
        Tea_stopif(bit >= record_width || bit < 0, return, 0,
                    "About to shift position %i in a record, but there "
                    "are only %i entries.", bit, record_width);
        record[bit] = 1;
        rctr++;
    }
    if (verbose){
        printf("record %li:\n", id);
        for (int rctr=0; rctr< record_width; rctr++)
            printf("%i\t", record[rctr]);
        printf("\n");
    } 
}

//A lengthy assertion checking that failed_fields and fails_edits are in sync.
static void do_fields_and_fails_agree(int **ofailed_fields, int fails_edits, int total_var_ct){
    int total_fails = 0;
    for (int k=0; k < total_var_ct; k++)
        total_fails += *ofailed_fields[k];
    assert((total_fails !=0 && fails_edits) || (total_fails==0 && !fails_edits));
}


/* see tea.h for documentation.

BUG: R won't let you pass a long int from R to C. ID numbers are often long
ints. Probably the only solution is to pass the id as a string. This only affects an
output when verbose is on, not internal processing.
*/
int consistency_check(char ***oext_values, char const *what_you_want, 
			long int id, int **ofailed_fields, bool do_preedits){

    if (!edit_grid) init_edit_list();
    if (!edit_grid || !edit_grid->matrix) return 0; //then there are no edits.

    int width = edit_grid->matrix->size2;
	Tea_stopif(!width, return 0, 1, "zero edit grid; returning zero failures.");
    int record[width];
    bool gotta_start_over = true;
    int last_run_preed = do_preedits ? -1 : INT_MAX;
    int fail_count;
    bool pf = !strcmp(what_you_want, "passfail");
    bool has_sql_edits = false;
    bool promised_nans[total_var_ct];memset(promised_nans, 0, sizeof(bool)*total_var_ct);  //TO DO
    while (gotta_start_over){
        gotta_start_over = false;
        int preed_to_run = -1;
        fill_a_record(record, width, oext_values, id);
        fail_count = check_a_record_discrete(record, ofailed_fields,
                                          &has_sql_edits, &preed_to_run, last_run_preed, promised_nans);
        if (pf && fail_count && !do_preedits)
            return 1;
        if (has_sql_edits||(preed_to_run>=0 && preed_to_run < INT_MAX)) 
            fail_count += check_a_record_sql(oext_values, ofailed_fields,
                                                 &last_run_preed, preed_to_run, &gotta_start_over, promised_nans);
    }
    Tea_stopif(gotta_start_over, return 1, 0,
                "Over 100 pre-edits made. I am probably stuck in a loop.")

    if (!pf) do_fields_and_fails_agree(ofailed_fields, fail_count, total_var_ct);

    for (int i=0; i< total_var_ct; i++)
        printf("%s\t", XN(*oext_values[i]));
    printf("\n");
    return fail_count;
}

/* Take in a data set; check each row. Return a table of the same size, with each cell
   indicating the number of edit failures for the corresponding cell in the original data.
*/
apop_data *checkData(apop_data *data, bool do_preedits){
	Tea_stopif(!data, return NULL, 1, "NULL data; returning NULL failure set.");
    //copy field names from the input data.
	int nvars = data->names->colct + data->names->textct;
	Tea_stopif(!nvars, return NULL, 1, "Zero columns in data; returning NULL failure set.");
	char *fields[nvars];
    memcpy(fields, data->names->col, sizeof(char*)*data->names->colct);
    memcpy(&fields[data->names->colct], data->names->text, sizeof(char*)*data->names->textct);

	//now that we have the variables, we can call consistency_check for each row
	long int id=1;
	int nrow = data->matrix ? data->matrix->size1: *data->textsize;
	int failed_fields[nvars];
	char *vals[nvars];
	apop_data *failCount = apop_data_calloc(nrow,nvars);
    apop_name_stack(failCount->names, data->names, 'c', 'c');
    apop_name_stack(failCount->names, data->names, 'c', 't');

    //sort filed names to ordered by external value lists.
    //these are pointers to failed_fields and vals, and so follow along as those change.
    char **oext_values[total_var_ct];
    for (int i=0; i<total_var_ct; i++)
        oext_values[i] = calloc(1, sizeof(char*));
    int *ofailed_fields[total_var_ct];
    memset(failed_fields, 0, nvars*sizeof(int));
    order_things_int(failed_fields, fields, nvars, ofailed_fields);

	for(int idx=0; idx<nrow; idx++){
		for(int jdx=0; jdx<nvars; jdx++){
		   if(data->matrix && jdx < data->matrix->size2){
                double v = apop_data_get(data,idx,jdx);
                if (isnan(v)) Asprintf(&vals[jdx], "%s", apop_opts.nan_string)
                else          Asprintf(&vals[jdx], "%g", v)
            }
			else vals[jdx] = data->text[idx][jdx - data->names->colct];
		}
        memset(failed_fields, 0, nvars*sizeof(int));
        order_things(vals, fields, nvars, oext_values); //has to be here for NaN-handling.

        consistency_check(oext_values, "failed fields", idx, ofailed_fields, do_preedits);


		//insert failure counts
		for(int jdx=0; jdx < nvars; jdx++)
			apop_data_set(failCount,.row=idx,.col=jdx,.val=failed_fields[jdx]);
	}
apop_data_show(data);
	return failCount;
}
