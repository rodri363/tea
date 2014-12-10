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
                if (*oext_values[j]=='\0') continue;
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
    asprintf(&qstring, "create table tea_test(");
    asprintf(&insert, "insert into tea_test values(");
    for (int i=0; i< total_var_ct; i++){
        if (*oext_values[i]=='\0') continue;
        xprintf(&qstring, "%s%c'%s' ", qstring, comma, used_vars[i].name);
        xprintf(&insert, "%s%c'%s' ", insert, comma, oext_values[i]);
        comma=',';
    }
    xprintf(&qstring, "%s numeric); %s);", qstring, insert);
    apop_query("%s", qstring);
    free(insert);
    free(qstring);
}

bool run_preedits(char ** oext_values, char const *preed){
    bool out=false;
    apop_query("update tea_test %s", preed);
    apop_data *newvals = apop_query_to_text("select * from tea_test");

    int ctr=0; //order is the same as oext_values; just have to find the entering fields.
    for (int f=0; f< newvals->textsize[1]; f++){
        if (*oext_values[ctr]=='\0') continue;
        asprintf(oext_values+ctr, newvals->text[0][f]);
        out=true;
    };

    apop_data_free(newvals);
    return out;
}

//call iff there are SQL edits to be checked.
static int check_a_record_sql(char ** oext_values, int ** ofailures,
           //              char *restrict* ud_post_preedit, //if we don't wan't preedits, NULL
                         int wanted_preed  //if the discrete edits found a failure, its row number is here; else -1.
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
            int fails = wanted_preed==i ? 1
                         : apop_query_to_float("select count(*) from tea_test where (%s)",
                                                                *edit_grid->text[i]);
            if (fails){
                char *preed = edit_grid_to_list[i]->pre_edit;
                if (wanted_preed>=0 && preed)
                    pre_edits_changed_something = run_preedits(oext_values, preed);
                out+=wanted_preed !=i && fails;
                if (ofailures)
                    for (int i=0; i<total_var_ct; i++){
                        if (*oext_values[i]=='\0') continue; //skip non-entering.
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
   /* for (int m=0; m< edit_grid->textsize[1]; m++)
        if (strlen(edit_grid->text[i][m]))
            apop_query("insert into editinfo values(%i, %i, 'f', \"%s\", '')",
                                         rownumber, i, edit_grid->text[i][m]);
    for (int m=0; m< total_var_ct; m++)
        if (rowfailures[m])
            apop_query("insert into editinfo values(%i, %i, 'r', \"%s\", '%s')",
                                         rownumber, i, used_vars[m].name,
                                         ext_from_ri(used_vars[m].name, rowfailures[m]));*/
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
static int check_a_record_discrete(int const * restrict row,  int ** failures, 
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

/* Fill the table of all possible alternatives. This involves checking all combinations of
   an indeterminate number of variables, and that's the sort of thing most easily done via recursion. 

   When you get to the last field in the list, you will have a record that is ready to
   test via check_a_record_discrete and check_a_record_sql. If that function returns zero,
   then write to \c fillme. To make life easier, <tt>fillme</tt>'s vector holds a
   single number: the length of the array so far.

   Now that you know what happens at the end, the recursion that leads up to it will, for each field, blank out the field, then for (i= each possible record value), set the record to i, and call the function to operate on the next field down.  This loop-and-recurse setup will guarantee that the last field in the list will run a test on all possible values.

   Notice that fields that aren't marked as failing are just skipped over.
*/
/*   TeaKEY(timeout, <<<Once it has been established that a record has failed a consistency
   check, the search for alternatives begins. Say that variables one, two, and three each have 100
    options; then there are 1,000,000 options to check against possibly thousands
    of checks. If a timeout is present in the spec (outside of all groups), then the
    alternative search halts and returns what it has after the given number of seconds
    have passed.>>>)
   */
static void do_a_combo(int *record, char **oext_values,
         int **failed_edits_in, int this_field, apop_data *fillme,
		 jmp_buf jmpbuf, time_t const timeout){

    int option_ct, skipme=0;
	if (*oext_values[this_field]==-1 || !*failed_edits_in[this_field]
           || used_vars[this_field].type=='r'){//not a failed field or DISCRETE editable; skip.
		skipme=1;
		option_ct=1;
	}
	else option_ct = find_e[this_field] - find_b[this_field] +1;
    if (!skipme)
        memset(record+find_b[this_field]-1, 0, sizeof(int)* option_ct);
	//else leave this field exactly as it was originally
    for (int i=0; i < option_ct; i++){
        if (!skipme){
            record[find_b[this_field]-1+i] = 1;
            if (i > 0) //clear the last value
                record[find_b[this_field]-1+i-1] = 0;
        }
        if (this_field +1 < total_var_ct)
            do_a_combo(record, oext_values, failed_edits_in, this_field+1,
							  fillme, jmpbuf, timeout);
        else{
			if (timeout && time(NULL) > timeout) longjmp(jmpbuf, 1);
            bool has_sql_edits = 0;
            int want_preedits = 0;
            if (!check_a_record_discrete(record, NULL, 0,  &has_sql_edits, &want_preedits)
                && (!has_sql_edits || !check_a_record_sql(oext_values, NULL, -1))
                ){//OK record; write it.
                int this_row = apop_data_get(fillme, 0, -1);
                (*gsl_vector_ptr(fillme->vector, 0))++;
                for (int k=0; k< total_var_ct; k++){
                    if (*failed_edits_in[this_field]){
                        for (int j=find_b[this_field]-1; j< find_e[this_field]; j++)
                            if (record[j]){
								if (fillme->matrix->size1 <=this_row)
									fillme->matrix = apop_matrix_realloc(
												fillme->matrix,
												fillme->matrix->size1*2, fillme->matrix->size2);
								//int rec_by_user_ordering = edit_order_to_user_order(this_fieldx, record_name_in, field_count);
                                apop_data_set(fillme, .col=this_field,
                                        .row=this_row, 
                                        .val=j-(find_b[this_field]-1));
								break;
							}
                    }
                }
            }
        }
    }
}

/* This function just gets the maximal dimensions of the alternatives table, allocates,
    and fills via the recursive \c do_a_combo function above. So this is really just the
    prep function for the recursion, where the real work happens.
*/
static apop_data * get_alternatives(int *record, char ** oext_values,
							int ** failing_records){
    int total_fails = 0;
    int rows = 1;
    for (int this_field = 0; this_field< total_var_ct; this_field++){
        total_fails += *failing_records[this_field] ? 1 : 0;
        if (*failing_records[this_field])
            rows *= find_e[this_field] - find_b[this_field]+1;
    }
	Tea_stopif(!total_fails,  apop_data*out=apop_data_alloc(); out->error='c'; return out,
                0, "Failed internal consistency check: I marked this "
				"record as failed but couldn't find which fields were causing failure.");
    apop_data *out = apop_data_alloc(1, rows, total_fails);
    out->vector->data[0] = 0;
    for (int i=0; i< total_var_ct; i++)
        if (*failing_records[i])
            apop_name_add(out->names, used_vars[i].name, 'c');

	//prepare the timeout; call the recursion
	jmp_buf jmpbuf;
	double user_timeout = get_key_float("timeout", NULL);
	time_t timeout = isnan(user_timeout) ? 0 : user_timeout+time(NULL);
	if (!setjmp(jmpbuf))
		do_a_combo(record, oext_values, failing_records, 0, out, jmpbuf, timeout);
	else
		fprintf(stderr,"Timed out on consistency query. Partial alternatives returned, but set may not be complete.\n");
    out->matrix = apop_matrix_realloc(out->matrix, out->vector->data[0], out->matrix->size2);
    return out;
}

// Generate a record in DISCRETE's preferred format for the discrete-valued fields,
// and a query for the real-valued.
static void fill_a_record(int *record, int record_width, char * const restrict *oext_values, int id){
    for (int rctr=0; rctr < record_width; rctr++)
        record[rctr]=-1;   //-1 == ignore-this-field marker
    int rctr=0; //i is the index for oext_values or used_vars; rctr for record.
    for (int i=0; i < total_var_ct; i++){
        if (*oext_values[i]=='\0' ||used_vars[i].type=='r') continue;
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

apop_data * cc2(char * *oext_values, char const *const *what_you_want, 
			int const *id, int *fails_edits, int **ofailed_fields, bool do_preedits){

    if (!edit_grid) init_edit_list();
    if (!edit_grid){ //then there are no edits.
		*fails_edits = 0;
        return NULL;
	}
    int width = edit_grid->matrix->size2;
	Tea_stopif(!width, return NULL, 1, "zero edit grid; returning NULL.");
    int record[width];
	fill_a_record(record, width, oext_values, *id);
    bool has_sql_edits = 0;
    int wanted_preed=-1;
    bool pf = !strcmp(what_you_want[0], "passfail");
    *fails_edits = check_a_record_discrete(record, ofailed_fields, (pf ? 0:*id),
                              &has_sql_edits, &wanted_preed);
    if (!do_preedits) wanted_preed=-1;//don't apply preedit, even if a failure was found.
    *fails_edits += (has_sql_edits||wanted_preed>=0) 
                       && check_a_record_sql(oext_values, ofailed_fields, wanted_preed);
    if (pf) return NULL;
    do_fields_and_fails_agree(ofailed_fields, *fails_edits, total_var_ct);

    if (!strcmp(what_you_want[0], "failed_fields"))
        return NULL;
    if (!strcmp(what_you_want[0], "find_alternatives") && *fails_edits)
		return get_alternatives(record, oext_values, ofailed_fields);
	return NULL;
}


/* see tea.h for documentation.

This function takes in a data set with an arbitrarily-ordered list of records. cc2,
above, takes in a list that exactly matches the order of the edit matrix. It simplifies
this file a lot, and perhaps we can some day deprecate this function in favor of
that one.
*/
apop_data * consistency_check(char * const *record_names, char * const *ud_values, 
			int const *record_size, char const *const *what_you_want, 
			int const *id, int *fails_edits, int *failed_fields,
            char * restrict *ud_post_preedit){
	Tea_stopif(*record_size <= 0, return NULL, 1, "zero record size; returning NULL.");
    char *oext_values[total_var_ct];
    int *ofailed_fields[total_var_ct];
    order_things(ud_values, record_names, *record_size, oext_values);
    if (failed_fields) order_things_int(failed_fields, record_names, *record_size, ofailed_fields);
    return cc2(oext_values, what_you_want, id, fails_edits, (failed_fields? ofailed_fields : NULL), !!ud_post_preedit);
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
	int fails_edits, failed_fields[nvars];
	char *vals[nvars];
	char const *what = "failed_fields";
	apop_data *failCount = apop_data_calloc(nrow,nvars);
    apop_name_stack(failCount->names, data->names, 'c', 'c');
    apop_name_stack(failCount->names, data->names, 'c', 't');

	for(int idx=0; idx<nrow; idx++){
		for(int jdx=0; jdx<nvars; jdx++){
		   if(data->matrix && jdx < data->matrix->size2){
						double v = apop_data_get(data,idx,jdx);
						if (isnan(v)) asprintf(&vals[jdx], "%s", apop_opts.nan_string);
						else          asprintf(&vals[jdx], "%g", v);
					}

			else vals[jdx] = data->text[idx][jdx - data->names->colct];
		}
        memset(failed_fields, 0, nvars*sizeof(int));
		consistency_check(fields,vals,&nvars,&what, &id,&fails_edits,failed_fields, NULL);
		//insert failure counts
		for(int jdx=0; jdx < nvars; jdx++){
			apop_data_set(failCount,.row=idx,.col=jdx,.val=failed_fields[jdx]);
		}
	}
	return failCount;
}
