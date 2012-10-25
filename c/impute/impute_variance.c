#include "imputation_variance.h"
#define apop_strcmp(a, b) (((a)&&(b) && !strcmp((a), (b))) || (!(a) && !(b)))

// from the Apophenia internals.
#define Get_vmsizes(d) \
    int firstcol = d && (d)->vector ? -1 : 0; \
    int vsize = d && (d)->vector ? (d)->vector->size : 0; \
    int wsize = d && (d)->weights ? (d)->weights->size : 0; \
    int msize1 = d && (d)->matrix ? (d)->matrix->size1 : 0; \
    int msize2 = d && (d)->matrix ? (d)->matrix->size2 : 0; \
    int tsize = vsize + msize1*msize2; \
    int maxsize = GSL_MAX(vsize, GSL_MAX(msize1, d?d->textsize[0]:0));\
    (void)(tsize||wsize||firstcol||maxsize) /*prevent unused variable complaints */;

#define apop_strcmp(a, b) (((a)&&(b) && !strcmp((a), (b))) || (!(a) && !(b)))

/**
Imputation (in this context) is the process of finding fill-in values for missing data
points. Filling in values from a single imputation and then returning the values will give
you a single complete data set, but statistics you derive from that data set don't reflect
the uncertainty inherent in using artifical, model-derived data rather than actual
observations. This function uses several imputations and a statistic-calculating function
you provide to find the total variance (see details below).


The multiple imputation process involves two steps:

\li Generating imputations via a model of your choosing, and writing down the results in a
\c fill_ins table, described in the parameter list. You've already done this by the time
you call this function.

\li For a given complete data set, generating a summary statistic, such as the mean of a
column, or the ratio of columns one and two; plus the variance of your statistic(s).

This function takes the output from the first step (a list of fill-ins), and uses it to
calculate a list of statistics/variances, as per the second step. After generating this
list of statistics/variances, it ties them together to produce a single best estimate of
the statistic and its full variance.


\param stat A function that takes in a single \ref apop_data set, and calculates
statistics and their covariances. The output should have two pages. The first
will be the statistics themselves; the second will be the covariance matrix. If
I find a page with the name <tt>\<Covariance\></tt> then I will use that; else
the second page. This rule means you can return the \c parameters from most estimated
models. Default: a function to find the per-column mean and the sample covariance matrix.

\param base_data The data, with \c NaNs to be filled in. When calculating the statistics,
I fill in the values in the \c base_data set directly, so it is modified, and in
the end will have the value of the last replication.

\param fill_ins This is the list of values to fill in to the base data set, and it must be
an \ref apop_data set whose matrix includes the following two column names toward the
beginning: \c row and \c col, with an optional \c page. Every column after those two or
three columns is taken to be an imputation that can be used to fill in values. That is, I
will first take the first column after the row/col/page column and plug its values
into the corresponding row/col/pages of \c base_data, calculate the variances, then repeat
with the second column, and so on.

\param row_name Which column in the fill-in table should I check for the row id to fill in. This could be an integer or text. If this name refers to a numeric column, then it indicates the row number in the main data to which this row of the fill-in table refers. If it refers to text, then this is the name of the row of the main data to fill in. Default: \c row .

\param col_name As with \c row_name, the name of the column in the fill-in table holding the names or numbers of the column in the main data set to be filled in. Default: \c col .

\param value_name The name of the column in the fill-in table holding the values to be written to the main data set. Default: \c value .

\param imputation_name The column of the fill-in table holding the name or number of the imputation. Default: imputation .


\return The first page is the mean of each replicate's statistics; the second page is the
overall covariance (and will have the page title <tt>\<Covariance\></tt>).
Let \f$S_i\f$ be the covariance for each replicate \f$i\f$; let there be \f$m\f$
replicates; let \f$\mu(\cdot)\f$ indicate the mean; then the overall covariance is the mean of the individual variances plus 

\f$\mu(S_i) + {\rm var}(S_i)/(1+1/m).\f$


\li Multiple pages for input data are not yet implemented.
*/

typedef apop_data *(*data_to_data)(apop_data*);

static apop_data *colmeans(apop_data *in){
    Get_vmsizes(in); //maxsize
    apop_data *sums = apop_data_summarize(in);
    Apop_col_t(sums, "mean", means);
    apop_data *out = apop_matrix_to_data(apop_vector_to_matrix(means, 'r'));
    apop_name_stack(out->names, in->names, 'c', 'c');
    apop_data *cov = apop_data_add_page(out, apop_data_covariance(in), "<Covariance>");
    gsl_matrix_scale(cov->matrix, 1/sqrt(maxsize));
    return out;
}

apop_data* multiple_imputation_variance_base(multiple_imputation_variance_t in){
    /*The first half of this is filling in the values. In an attempt at versatility, I allow users to 
      give any named column, be it numeric or text, for every piece of input info. That means a whole lot 
      of checking around to determine what goes where---and a macro.  */

    Apop_assert_c(in.base_data,NULL, 1, "It doesn't make sense to impute over a NULL data set.");
    Apop_assert_c(in.fill_ins, NULL, 1, "Didn't receive a fill-in table. Returning NULL.");
    data_to_data stat = in.stat? in.stat : colmeans;

//At the end of this macro, you've got rowcol and rowtype, valuecol and valuetype, &c.
#define apop_setup_one_colthing(c) \
    int c##col = apop_name_find(in.fill_ins->names, in.c##_name, 'c');   \
    int c##type = 'd';         \
    if (c##col==-2){           \
        c##col = apop_name_find(in.fill_ins->names, in.c##_name, 't');   \
        c##type = 't';         \
       Apop_assert(c##col!=-2, "I couldn't find the c##_name %s in the column/text names of your fill_in table.", in.c##_name);    \
    }

    apop_setup_one_colthing(row)
    apop_setup_one_colthing(col)
    apop_setup_one_colthing(value)
    apop_setup_one_colthing(imputation)

    Apop_assert(!(rowtype=='t' && !in.base_data->names->rowct),
            "the rowname you gave refers to text, so I will be searching for a row name in the base data."
            " But the base_data set has no row names.");
    Apop_assert(!(coltype=='t' && !in.base_data->names->colct),
            "the colname you gave refers to text, so I will be searching for a column name in the base data."
            " But the base_data set has no column names.");

    //get a list of unique imputation markers.
    gsl_vector *imps = NULL;
    apop_data *impt = NULL; 
    if (imputationtype == 'd'){
        Apop_col(in.fill_ins, imputationcol, ic);
        imps = apop_vector_unique_elements(ic);
    } else impt = apop_text_unique_elements(in.fill_ins, imputationcol);

    int len = imps ? imps->size : impt->textsize[0];
    int thisimp=-2; char *thisimpt=NULL;
	apop_data *estimates[len];
    for (int impctr=0; impctr< len; impctr++){
        if (imps) thisimp  = gsl_vector_get(imps, impctr);
        else      thisimpt = impt->text[impctr][0];
        Get_vmsizes(in.fill_ins); //masxize
        int fillsize = maxsize ? maxsize : in.fill_ins->textsize[0];
        for (int i=0; i< fillsize; i++){
            if (!(thisimpt && apop_strcmp(in.fill_ins->text[i][imputationcol], thisimpt))
                && !(imps && thisimp==apop_data_get(in.fill_ins, i, imputationcol)))
                continue;
            int thisrow = (rowtype=='d') ? 
                                apop_data_get(in.fill_ins, i, rowcol)
                               :apop_name_find(in.base_data->names, in.fill_ins->text[i][rowcol], 'r');
            int thiscol = (coltype=='d') ? 
                                apop_data_get(in.fill_ins, i, colcol)
                               :apop_name_find(in.base_data->names, in.fill_ins->text[i][colcol], 'c');
            if (valuetype=='d') apop_data_set(in.base_data, thisrow, thiscol, 
                                            apop_data_get(in.fill_ins, i, valuecol));
            else apop_text_add(in.base_data, rowcol, colcol, in.fill_ins->text[i][valuecol]);
        }
        //OK, base_data is now filled in. Estimate the statistic for it.
		estimates[impctr] = stat(in.base_data);
    }


    //Part II: find the mean of the statistics and the total variance of the cov matrix.
	gsl_vector *vals = gsl_vector_alloc(len);
    apop_data *out = apop_data_copy(estimates[0]);
	//take the simple mean of the main data set.
	{ //this limits the scope of the Get_vmsizes macro.
	 Get_vmsizes(estimates[0]); 
     for (int j=0; j < msize2; j++)
         for (int i=0; i < (vsize ? vsize : msize1); i++){
            for (int k=0; k< len; k++)
                gsl_vector_set(vals, k, apop_data_get(estimates[k], i, j));
             apop_data_set(out, i, j, apop_vector_mean(vals));
         }
	}
    apop_data *out_var = apop_data_get_page(estimates[0], "<Covariance>");
    int cov_is_labelled = out_var !=NULL;
    if (!cov_is_labelled){
        sprintf(out->more->names->title, "<Covariance>");
        out_var = estimates[0]->more;
    }
	Get_vmsizes(out_var);
    for (int i=0; i < msize1; i++)
        for (int j=i; j < msize2; j++){
            for (int k=0; k< len; k++){
                apop_data *this_p = cov_is_labelled ? apop_data_get_page(estimates[k], "<Covariance>")
                                        : estimates[k]->more;
                gsl_vector_set(vals, k, apop_data_get(this_p, i, j));
            }
            double total_var = apop_vector_mean(vals) + apop_var(vals)/(1+1./len);
            apop_data_set(out_var, i, j, total_var);
            if (j != i)
                apop_data_set(out_var, j, i, total_var);
        }
    return out;	
}
