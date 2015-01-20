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

static void check_for_all_vars(bool *usable_sql, char * const restrict*oext_values){
    for (int r=0; r< edit_grid->vector->size; r++){
        if(apop_data_get(edit_grid, r, -1) != 2) {
            usable_sql[r] = false;
            continue; 
        }
        usable_sql[r] = true;
        for (int i=0; i< edit_grid_to_list[r]->var_ct; i++){
            bool found=false;
            for (int j=0; j< total_var_ct; j++){
                if (oext_values[j] && *oext_values[j]=='\0') continue;
                if (!strcasecmp(edit_grid_to_list[r]->vars_used[i].name, used_vars[j].name))
                        {found=true; break;}
            }
            if (!found) {
                usable_sql[r] = false;
                break;
            }
        }
    }
}

static void sqlify(char * const restrict* oext_values){
    /*qstring will hold a query to generate a sample table, like "create table tea_test(a,
    b); insert into tea_test values('left', 'right');"*/
    char *qstring, *insert, comma=' ';
    Asprintf(&qstring, "create table tea_test(");
    Asprintf(&insert, "insert into tea_test values(");
    for (int i=0; i< total_var_ct; i++){
        if (!oext_values[i]) 
            xprintf(&insert, "%s%c NULL ", insert, comma);
        else if (*oext_values[i]=='\0') 
            continue;
        else 
            xprintf(&insert, "%s%c'%s' ", insert, comma, oext_values[i]);
        xprintf(&qstring, "%s%c'%s' ", qstring, comma, used_vars[i].name);
        comma=',';
    }
    xprintf(&qstring, "%s numeric); %s);", qstring, insert);
    apop_query("%s", qstring);
    free(insert);
    free(qstring);
}

/* If the input preedit changed something, then both oext_values here and tea_test in the
   db will be different. Output answers the question `did the preedit change anything?' */
static bool run_preedits(char ** oext_values, char const *preed){
    bool out=false;
    apop_query("update tea_test %s", preed);
    apop_data *newvals = apop_query_to_text("select * from tea_test");

    int ctr=0; //order is the same as oext_values; just have to find the entering fields.
    for (int octr=0; octr< total_var_ct; octr++){
        if (oext_values[octr] && *oext_values[octr]=='\0') continue;

        //Found the right field. Did it change?
        char *postval = newvals->text[0][ctr];
        char *preval = oext_values[octr];
        bool postval_is_null = !strcmp(postval, apop_opts.nan_string);
        if ((preval && !strcmp(preval, postval)) 
            ||(!preval && postval_is_null))
                continue; //no change.

        out=true;
        if (!postval_is_null)
            Asprintf(oext_values+octr, postval)
        else
            oext_values[octr] = NULL;
        ctr++;
    }

    apop_data_free(newvals);
    return out;
}

//call iff there are SQL edits (or preedits) to be checked.
static int check_a_record_sql(char ** oext_values, int **ofailures,
                         int wanted_preed,  //if the discrete edits found a failure, its row number is here; else -1.
                         bool *gotta_start_over
                         ){
    int out = 0;
    bool usable_sql[edit_grid->vector->size];
    check_for_all_vars(usable_sql, oext_values);
    begin_transaction();
    sqlify(oext_values);
    if (!ofailures && wanted_preed<0){   //just want pass-fail ==> run a single yes/no query
        char *q = apop_text_paste(edit_grid, .between=") or (", .after=")",
              .before= "select count(*) from tea_test where (", .prune=prune_edits,
              .prune_parameter=usable_sql);
        if (strlen(q) != strlen("select count(*) from tea_test where ()"))//any relevant sql?
            out += apop_query_to_float("%s", q);
        free(q);
    } else {
        edit_t *last_list_item = NULL;
        bool pre_edits_changed_something = false;
        for (int i=0; i< edit_grid->vector->size; i++){
            if (last_list_item==edit_grid_to_list[i] && wanted_preed!=i) continue;
            last_list_item=edit_grid_to_list[i];
            if (!usable_sql[i] && wanted_preed<0) continue;
            int fails = usable_sql[i] && apop_query_to_float("select count(*) from tea_test where (%s)",
                                                                *edit_grid->text[i]);
            if (fails || wanted_preed==i){
                char *preed = edit_grid_to_list[i]->pre_edit;
                if (preed)
                    pre_edits_changed_something = run_preedits(oext_values, preed);
                out+=fails;
if(pre_edits_changed_something) {
    *gotta_start_over=true;
    return 1;
}
                if (ofailures)
                    for (int i=0; i<total_var_ct; i++){
                        if (oext_values[i] && *oext_values[i]=='\0') continue; //skip non-entering.
                        for (int j=0; j< last_list_item->var_ct; j++)//is this var. used in this edit?
                            if (!strcasecmp(last_list_item->vars_used[j].name, used_vars[i].name))
                                {(*ofailures[i])++; break;}
                    }
            }
            if (pre_edits_changed_something) break;
        }
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

  If the input array failures is NULL, then we're in brief mode: first time we find a
  failed edit, then exit reporting non-passing.

  If failures is an array to be filled (of size nsize: the number of fields in
  em), then check all edits, and mark down every field involved in any failure.
  There is one slot per field (i.e., it is unrelated to any subset of records
  requested by the user).
  */
static int check_a_record_discrete(int const * restrict row,  int **failures,
                   int rownumber, bool *has_sql_edits, int *run_this_preedit){
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
            if (*run_this_preedit){ //then we will be applying the preedits
                *run_this_preedit=i;
                if (!failures) goto done;
            } 
        }
    }
    done:
    return out;
}

/* There had been a clever recursive routine to generate a list of all possible
   alternatives. In every application we have, we've found that it's easier to just
   re-draw until we have a passing entry than to exhaustively generate the list of
   possiblities.  It last appeared in commit c11ad596. */


// Generate a record in DISCRETE's preferred format for the discrete-valued fields,
// and a query for the real-valued.
static void fill_a_record(int *record, int record_width, char * const restrict *oext_values, int id){
    for (int rctr=0; rctr < record_width; rctr++)
        record[rctr]=-1;   //-1 == ignore-this-field marker
    int rctr=0; //i is the index for oext_values or used_vars; rctr for record.
    for (int i=0; i < total_var_ct; i++){
        if (!oext_values[i] || 
                (oext_values[i] && *oext_values[i]=='\0') ||used_vars[i].type=='r')
            continue;
        int ri_position = ri_from_ext(used_vars[i].name, oext_values[i]);
        if (ri_position == -100) continue;  //This variable wasn't declared ==> can't be in an edit.
        for(int  kk = find_b[rctr]-1; kk< find_e[rctr]; kk++)
            record[kk] = 0;
        Tea_stopif(ri_position == -1 , return, 0, "I couldn't find the value %s in your "
                "declarations for the variable %s. Please remove the error from the data or "
                "add that value to the declaration, then restart the program so I can rebuild "
                "some internal data structures.", oext_values[i], used_vars[i].name);
        int bit = find_b[rctr]-1 + ri_position-1;
        Tea_stopif(bit >= record_width || bit < 0, return, 0,
                    "About to shift position %i in a record, but there "
                    "are only %i entries.", bit, record_width);
        record[bit] = 1;
        rctr++;
    }
    if (verbose){
        printf("record %i:\n", id);
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

int cc2(char * *oext_values, char const *const *what_you_want, 
			int const *id, int **ofailed_fields, bool do_preedits, int recursion_count){

    if (!edit_grid) init_edit_list();
    if (!edit_grid || !edit_grid->matrix) return 0; //then there are no edits.

    int width = edit_grid->matrix->size2;
	Tea_stopif(!width, return 0, 1, "zero edit grid; returning zero failures.");
    int record[width];
	fill_a_record(record, width, oext_values, *id);
    bool has_sql_edits = false, gotta_start_over=false;
int wanted_preed=1000000;
    bool pf = !strcmp(what_you_want[0], "passfail");
    int fail_count = check_a_record_discrete(record, ofailed_fields, (pf ? 0:*id),
                              &has_sql_edits, &wanted_preed);
    if (!do_preedits) wanted_preed=-1;//don't apply preedit, even if a failure was found.
    fail_count += (has_sql_edits||wanted_preed>=0) 
                       && check_a_record_sql(oext_values, ofailed_fields, wanted_preed, &gotta_start_over);
    if (gotta_start_over) {
        Tea_stopif(recursion_count>100, return 1, 0,
                "Over 100 pre-edits made. I am probably stuck in a loop.")
        return cc2(oext_values, what_you_want, id, ofailed_fields, do_preedits, recursion_count++);
    }

    if (!pf) do_fields_and_fails_agree(ofailed_fields, fail_count, total_var_ct);

    return fail_count;
}


/* see tea.h for documentation.

This function takes in a data set with an arbitrarily-ordered list of records. cc2,
above, takes in a list that exactly matches the order of the edit matrix. It simplifies
this file a lot, and perhaps we can some day deprecate this function in favor of
that one.
*/
int consistency_check(char * const *record_names, char * const *ud_values, 
			int const *record_size, char const *const *what_you_want, 
			int const *id, int *failed_fields, char * restrict *ud_post_preedit){
	Tea_stopif(*record_size <= 0, return 0, 1, "zero record size; returning zero failures.");
    char *oext_values[total_var_ct];
    int *ofailed_fields[total_var_ct];
    order_things(ud_values, record_names, *record_size, oext_values);
    if (failed_fields) order_things_int(failed_fields, record_names, *record_size, ofailed_fields);
    return cc2(oext_values, what_you_want, id, (failed_fields? ofailed_fields : NULL), !!ud_post_preedit, 0);
}

apop_data *checkData(apop_data *data){
    //copy field names from the input data.
	int nvars = data->names->colct + data->names->textct;
	char *fields[nvars];
    memcpy(fields, data->names->col, sizeof(char*)*data->names->colct);
    memcpy(&fields[data->names->colct], data->names->text, sizeof(char*)*data->names->textct);

	//now that we have the variables, we can call consistency_check for each row
	int id=1;
	int nrow = data->matrix ? data->matrix->size1: *data->textsize;
	int failed_fields[nvars];
	char *vals[nvars];
	char const *what = "failed_fields";
	apop_data *failCount = apop_data_calloc(nrow,nvars);
    apop_name_stack(failCount->names, data->names, 'c', 'c');
    apop_name_stack(failCount->names, data->names, 'c', 't');

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
		consistency_check(fields,vals,&nvars,&what, &id, failed_fields, NULL);
		//insert failure counts
		for(int jdx=0; jdx < nvars; jdx++){
			apop_data_set(failCount,.row=idx,.col=jdx,.val=failed_fields[jdx]);
		}
	}
	return failCount;
}
