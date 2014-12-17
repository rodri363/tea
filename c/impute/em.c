#include "internal.h"
#include "em_weight.h"

/* The EM algorithm is a bit of a mess. it is divided into two files, this and em_weights.c.
Here we have what gets called by impute.c, which sets up the table for the EM iteration,
which is primarily in em_weight.c.

It went through some evolution, and is based on an ill-fated raking algorithm. Some
of this file and em_weight.c may be redundant.
*/

/* Zero out the weights of those rows that don't match. 
 * Thanks to copy_by_name, we know that the column list for the two input data sets match.
 */
double cull(apop_data *onerow, void *subject_row_in){
    apop_data *subject_row = subject_row_in;
    for (int i=0; i< subject_row->matrix->size2; i++){
       // double this = apop_data_get(subject_row, .col=i);
        double this = gsl_matrix_get(subject_row->matrix, 0, i);
        if (isnan(this)) continue;
        /* If match or no information, then continue
         * If not a match, then reweight by 1/distance? */
        //double dist = fabs(apop_data_get(onerow, .col=i) - this);
        double dist = fabs(gsl_matrix_get(onerow->matrix, 0, i) - this);
        if (dist< 1e-5) continue;
        else {
            if (!subject_row->more) *onerow->weights->data = 0;
            else {   //if allow_near_misses on this field then weight, don't cull.
                char type = subject_row->more->text[i][0][0];
                *onerow->weights->data *= (type=='r') ? 1/(1+dist): 0;
            }
        }
    }
    return 0;
}

/* OK, exact matching didn't work? Use this function to find
 * the difference count between the reference record and each row in the raked table. */
double count_diffs(apop_data *onerow, void *subject_row_in){
    apop_data *subject_row = subject_row_in;
    int diffs=0;
    for (int i=0; i< subject_row->matrix->size2; i++){
        double this = apop_data_get(subject_row, .col=i);
        if (isnan(this)) continue;
        if (apop_data_get(onerow, .col=i) != this) {
            diffs++;
        }
    }
    return diffs;
}


/* We're depending on the data columns and the draw columns to
   be the same fields, and we aren't guaranteed that the raking
   gave us data in the right format(BK: check this?). Copy the 
   raked output to explicitly fit the data format. */
apop_data *copy_by_name(apop_data const *data, apop_data const *form){
    apop_data *out= apop_data_alloc(data->matrix->size1, form->matrix->size2);
    apop_name_stack(out->names, data->names, 'r');
    apop_name_stack(out->names, form->names, 'c');
    for (int i=0; i< out->matrix->size2; i++){
        int corresponding_col = apop_name_find(out->names, data->names->col[i], 'c');
        Apop_col_v(data, i, source);
        if (corresponding_col!=-2) {
            Apop_matrix_col(out->matrix, corresponding_col, dest);
            gsl_vector_memcpy(dest, source);
        }
    }
    return out;
}

/* We have raked output, and will soon be making draws from it. 
   So it needs to be a PMF of the right format to match the data point
   we're trying to match.  
   
   It may be that the row in the data has no match in the raked output, due to structural
   zeros and the inputs. E.g., if a person is married and has missing age, and everybody
   else in the data set is under 15, and (age <15 && married) is an edit failure, there will
   be no entries in the rake table with married status. In this case, blank out the existing married status.
   
   */
apop_model *prep_the_draws(apop_data *raked, apop_data *fin, gsl_vector const *orig,  int cutctr){
    Tea_stopif(!raked, return NULL, 0, "NULL raking results.");
    double s = apop_sum(raked->weights);
    Tea_stopif(isnan(s), return NULL, 0, "NaNs in raking results.");
    Tea_stopif(!s, return NULL, 0, "No weights in raking results.");
    bool done = false;
    apop_map(raked, .fn_rp=cull, .param=fin, .inplace='v');
    done = apop_sum(raked->weights);
    Tea_stopif(!done, return NULL, 0, "Still couldn't find something to draw.");
    return apop_estimate(raked, apop_pmf);
}

//An edit that uses variables not in our current raking table will cause SQL failures.
bool vars_in_edit_are_in_rake_table(used_var_t* vars_used, size_t ct, apop_data rake_vars){
    for (int i=0; i< ct; i++){
        bool pass=false;
        for (int j=0; j< rake_vars.textsize[1] && !pass; j++)
            if (!strcmp(vars_used[i].name, rake_vars.text[0][j])) pass=true;
        if (!pass) return false;
    }
    return true;
}

apop_data *get_data_for_em(const char *datatab, char *catlist, const char *id_col, 
                           char const *weight_col, char *previous_filltab, impustruct is){
    apop_data as_data = (apop_data){.textsize={1,is.allvars_ct}, .text=&is.allvars};
    char *varlist = apop_text_paste(&as_data, .between=", ");
    apop_data *d = apop_query_to_data("select %s, %s %c %s from %s %s %s", id_col, varlist, 
                    weight_col ? ',' : ' ',
                    XN(weight_col), /*previous_filltab ? dt :*/datatab, catlist ? "where": " ", XN(catlist));
    free(varlist);
    Tea_stopif(!d || d->error, return d, 0, "Query trouble.");
    if (!d->weights) {
        if (weight_col){
            Apop_col_tv(d, weight_col, wc);
            d->weights = apop_vector_copy(wc);
            gsl_vector_set_all(wc, 0); //debris.
        } else {
            d->weights = gsl_vector_alloc(d->matrix->size1);
            gsl_vector_set_all(d->weights, 1);
        }
    }
    return d;
}

void writeout(apop_data *fillins, gsl_vector *cp_to_fill, apop_data const *focus, int *ctr, int drawno){
    for (size_t j=0; j< focus->matrix->size2; j++)  
     //   if (isnan(apop_data_get(focus, .col=j))){
        if (isnan(gsl_matrix_get(focus->matrix, 0, j))){
            apop_text_add(fillins, *ctr, 0, *focus->names->row);
            apop_text_add(fillins, *ctr, 1, focus->names->col[j]);
            gsl_matrix_set(fillins->matrix, *ctr, 0, drawno);
            gsl_matrix_set(fillins->matrix, (*ctr)++, 1, cp_to_fill->data[j]);
        }
}

double nnn (double in){return isnan(in);}

void get_types(apop_data *raked){
    apop_data *names = apop_data_add_page(raked, 
                       apop_text_alloc(NULL, raked->names->colct, 1), "types");
    for (int i=0; i< raked->names->colct; i++)
        apop_text_add(names, i, 0, "%c", get_coltype(raked->names->col[i]));
}

apop_data *make_rake_draws(apop_data const *d, apop_data *raked, impustruct is, int count_of_nans, gsl_rng *r, int draw_count, int *ctr){
    gsl_vector *original_weights = apop_vector_copy(raked->weights);
    gsl_vector *cp_to_fill = gsl_vector_alloc(d->matrix->size2);
    Apop_row(raked, 0, firstrow);
    apop_data *name_sorted_cp = copy_by_name(d, firstrow);
    if (is.allow_near_misses) get_types(name_sorted_cp); //add a list of types as raked->more.
    apop_data *fillins = apop_text_alloc(apop_data_alloc(count_of_nans*draw_count, 2), count_of_nans*draw_count, 2);
    for (size_t i=0; i< d->matrix->size1; i++){
        Apop_matrix_row(d->matrix, i, focusv);    //as vector
        if (!isnan(apop_sum(focusv))) continue;
        Apop_row(d, i, focus);               //as data set w/names
        Apop_row(name_sorted_cp, i, focusn); //as data set w/names, arranged to match rake
        focusn->more = name_sorted_cp->more;

        //draw the entire row at once, but write only the NaN elmts to the filled tab.
        apop_model *m = prep_the_draws(raked, focusn, original_weights, 0); 
        if (!m || m->error) goto end; //get it on the next go `round.
        for (int drawno=0; drawno< draw_count; drawno++){
            Tea_stopif(!focus->names, goto end, 0, "focus->names is NULL. This should never have happened.");
            apop_draw(cp_to_fill->data, r, m);
            writeout(fillins, cp_to_fill, focus, ctr, drawno);
        }
        end:
        apop_model_free(m);
        gsl_vector_memcpy(raked->weights, original_weights);
    }
    apop_data_free(name_sorted_cp);
    gsl_vector_free(original_weights);
    gsl_vector_free(cp_to_fill);
    return fillins;
}

void diagnostic_print(apop_data *d, apop_data *raked){
    apop_data *dcp = apop_data_sort(apop_data_pmf_compress(apop_data_copy(d)));
    asprintf(&dcp->names->title, "<<<<<<<<<<<<<<<<<<<<<<<<<<<");
    asprintf(&raked->names->title, ">>>>>>>>>>>>>>>>>>>>>>>>>>>");
    apop_data_pmf_compress(raked);
    apop_data_sort(raked);
    apop_data_print(dcp, .output_name="ooo", .output_append='a');
    apop_data_print(raked, .output_name="ooo", .output_append='a');
    apop_data_free(dcp);
}

void em_to_completion(char const *datatab, char const *underlying,
        impustruct is, int min_group_size, gsl_rng *r,
        int draw_count, char *catlist,
        apop_data const *fingerprint_vars, char const *id_col,
        char const *weight_col, char const *fill_tab, char const *margintab,
        char *previous_filltab, int autofill){

    apop_data *d = get_data_for_em(datatab, catlist, id_col, weight_col, previous_filltab, is);
    Tea_stopif(!d, return, 0, "Query for appropriate data returned no elements. Nothing to do.");
    Tea_stopif(d->error, return, 0, "query error.");
    int count_of_nans = apop_map_sum(d, .fn_d=nnn);
    if (!count_of_nans) return;
    if (is.allow_near_misses) get_types(d); //add a list of types as d->more
    
    apop_data *raked = em_weight(d, .tolerance=1e-3);
    Tea_stopif(!raked, return, 0, "Raking returned a blank table. This shouldn't happen.");
    Tea_stopif(raked->error, return, 0, "Error (%c) in raking.", raked->error);

    //diagnostic_print(d, raked);

    int ctr = 0;
    apop_data *fillins = make_rake_draws(d, raked, is, count_of_nans, r, draw_count, &ctr);
    apop_matrix_realloc(fillins->matrix, ctr, fillins->matrix->size2);
    apop_text_alloc(fillins, ctr, 2);
    begin_transaction();
    if (fillins->matrix) {
        if (!autofill) apop_data_print(fillins, .output_name=fill_tab, .output_type='d', .output_append='a');
        else {
            for (int i=0; i< fillins->matrix->size1; i++)
                for (int j=0; j< fillins->matrix->size2; j++)
                    apop_query("update %s set %s = %g where  %s='%s');",
                           datatab, fillins->names->col[j], apop_data_get(fillins, i, j),
                           id_col, fillins->names->row[i]);

        }
    }
    commit_transaction();
    apop_data_free(fillins);
    //apop_data_print(raked, .output_name="ooo", .output_append='a');
    apop_data_free(raked); //Thrown away.
    apop_data_free(d);
}


