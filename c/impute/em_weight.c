#include "em_weight.h"

// Zero out the weights of those rows that don't match.
static double cull2(apop_data const *onerow, apop_data *cullback){
    for (int row=0; row<cullback->matrix->size1; row++){
        Apop_row(cullback, row, cull_row);
        for (int i=0; i< cull_row->matrix->size2; i++){
            double this = apop_data_get(onerow, .col=i);
            if (isnan(this)) continue;
            if (apop_data_get(cull_row, .col=i) != this) {
                gsl_vector_set(cullback->weights, row, 0);
                break;
            }
        }
    }
    return 0;
}

/* In this version, both the reference row and the weight set to be culled
   may have NaNs. We still require compabibility in those fields where both
   have data, but where one has a NaN and the other doesn't, we write down 
   the nonmissing value for that field, regardless of which side it came from.
   Therefore, the resultant data has fewer NaN fields than either source, and 
   repeating this over several iterations can eventually produce a NaN-free set.

   Not all data sets can complete like this.

   If a row has NaNs, but there is no additional fill-in, then give it zero weight.

   The rules:
--If a row has any NaN data, skip self in the cullback.
--If there is another row with unambiguously more data, skip this row.
--As an elaboration, if there is complete data anywhere in the candidate set, ignore
any incomplete rows (even if they are complete after fillin).

So, I need two passes:
(1) mark whether the row has NaNs.
(2) mark whether any admissable row has no NaNs.
(3) Check whether the row has any fill-ins. If has NaNs but no fill-ins, then it is either self or has even less data.
2nd pass:
(4) If any admissable rows have no NaNs, zero out all previously admissable 
rows with NaNs.
*/
static double cull_w_nans(apop_data const *onerow, apop_data *cullback){
    if (!cullback || !cullback->matrix) return 0;
    bool has_nans[cullback->matrix->size1];
    bool complete_admissable_row = false;
    for (int row=0; row<cullback->matrix->size1; row++){
        Apop_row(cullback, row, cull_row);
        has_nans[row] = false;
        bool this_row_has_fillins = false;
        double *weight = gsl_vector_ptr(cullback->weights, row);
        for (int i=0; i< cull_row->matrix->size2; i++){
            double ref_field = apop_data_get(onerow, .col=i);
            double *cull_field = apop_data_ptr(cull_row, .col=i);
            has_nans[row] = has_nans[row] || isnan(*cull_field);  //step (1)
            if ((*cull_field != ref_field && !isnan(*cull_field) && !isnan(ref_field)) //mismatch
                    || (has_nans[row] && complete_admissable_row)) { //step (4)
                *weight = 0;
                break;
            }
            if (isnan(*cull_field) && !isnan(ref_field)){
                *cull_field = ref_field;
                this_row_has_fillins = true;
            }
        }
        if (!has_nans[row] && *weight != 0)         //step (2)
            complete_admissable_row = true;
        if (has_nans[row] && !this_row_has_fillins) //step (3)
            *weight = 0;
    }
    if (complete_admissable_row)                    //step (4)
        for (int row=0; row<cullback->matrix->size1; row++)
            if (has_nans[row]) gsl_vector_set(cullback->weights, row, 0);
    return 0;
}

void save_candidate(apop_data *candidate, apop_data **prior_candidate){
    apop_data_free(*prior_candidate);
    *prior_candidate = candidate;
    //apop_data_pmf_compress(*prior_candidate);
}

/* This version uses cull_w_nans. At the bottom of the for loop,
   we have a candidate set that has fewer NaNs than before, and is weighted to the
   count of the original data.

   make one copy for the candidate weights; make another copy which is now the data
   set; rerun for as many times as there are fields, because the worst case is that
   each run reduces an observation's NaN count by one field.

   So what changed? 
   --The indata is not listwise deleted.
   --the candidate is copied to be the new data set.
   --we use a different cull function.

*/
apop_data *em_weight_base(em_weight_s in){
    apop_data *candidate = apop_data_copy(in.d);
    double tolerance = in.tolerance ? in.tolerance : 1e-5;
    apop_data *complete = apop_data_listwise_delete(apop_data_copy(in.d)); //may be NULL.
    apop_vector_normalize(candidate->weights);
    apop_data *prior_candidate = NULL;
    int ctr = 0;
    int saturated = (int) log2(in.d->matrix->size2);
    do {
        save_candidate(candidate, &prior_candidate);
        candidate = apop_data_copy(complete);
        for (int i=0; i < in.d->matrix->size1; i++){
            Apop_row(in.d, i, row);
            if (isnan(apop_matrix_sum(row->matrix))) {
                apop_data *cp = apop_data_copy(prior_candidate);
                (ctr<saturated ? cull_w_nans : cull2)(row, cp);
                if (!cp->matrix) {
                    apop_data_free(cp);
                    continue;
                }
                apop_vector_normalize(cp->weights);
                gsl_vector_scale(cp->weights, gsl_vector_get(row->weights, 0));
                candidate = apop_data_stack(candidate, cp, .inplace='y');
                apop_data_free(cp);
            }
        }
        apop_data_pmf_compress(candidate);
        apop_vector_normalize(candidate->weights);
        if (ctr==saturated) apop_data_listwise_delete(candidate, .inplace='y');
        apop_vector_normalize(candidate->weights);
    } while (ctr++<= saturated || (candidate->weights->size != prior_candidate->weights->size || apop_vector_distance(candidate->weights, prior_candidate->weights, .metric='m') > tolerance));
    apop_data_free(complete);
    return candidate;
}
