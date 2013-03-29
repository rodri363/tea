#include <apop.h>

#include "internal.h" //only for (begin|commit)_transaction

/* This is a somewhat fragile model for hot-decking relation. It
   is designed to work in the context of Tea.

   We walk in with a list of potentially dozens of HHs,
    all in the subcategory we want; all with the same number of
    members.

    !! I currently expect to have a column named mafid, and a column named rel.
    !! I currently expect that we've already categorized by hhsize. This is
        a pretty strong dependency; maybe work around somehow?

    Here's a spec subset that meets the criteria:

group recodes {
    group id: mafid
    hhsize : count(*)
}

impute {
    category{
        hhsize
    }
    method: rel
    output vars: rel
    input vars: mafid
}


    Estimate step just writes them to a db table.

    If at the beginning of a household, draw step will select a new HH mafid,
    then draw its data to a static apop_data set. Then step through to return
    individual rel codes.
*/

static apop_model *rel_est(apop_data *d, apop_model *m){
    begin_transaction();
    apop_data_print(d, .output_file="tea_hhs", .output_type='d', .output_append='w');
    commit_transaction();
    return m;
}

static void rel_draw(double *out, gsl_rng *r, apop_model *m){
    static int ctr;
    static apop_data *this_hh;
    if (ctr == 0){
        //All HHs appear in equal numbers, so uniform draw works.
        int hh_id = apop_query_to_float("select mafid from tea_hhs where oid+0.0 = 1+%i",
              (int)(gsl_rng_uniform(r)*apop_query_to_float("select count(*) from tea_hhs")));
        this_hh = apop_query_to_data("select rel from tea_hhs where "
                    "mafid = %i", hh_id);
        Apop_stopif (!this_hh || this_hh->error, *out=GSL_NAN; return,
                0, "Something went wrong drawing a household from my "
                    "table of households, tea_hh.");
    }
    *out = apop_data_get(this_hh, ctr);
    ctr = (ctr+1) % this_hh->matrix->size1;
}

apop_model relmodel = {"Model for drawing relationship", .estimate=rel_est, .draw=rel_draw};
