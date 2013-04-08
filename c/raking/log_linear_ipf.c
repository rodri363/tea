#define __USE_POSIX //for strtok_r
#include "tea.h"
#include "internal.h"
/*
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

char **(*PEP_R_STRSXP_TO_C)(SEXP s);//from Rapophenia
SEXP (*PEP_R_get_apop_data_matrix)(const apop_data *D);
*/

void unbiasround( double *mat, int *imat, int * imat1, int * rowparity,int *nrow, int *ncol, gsl_rng *r);

void to_integer(apop_data *d){
gsl_rng *r  =apop_rng_alloc(231);

    int nrow =d->matrix->size1;
    int ncol =d->matrix->size2;
    gsl_matrix_int *imat = gsl_matrix_int_alloc(nrow+1, ncol+1);
    gsl_matrix_int *imat1 = gsl_matrix_int_alloc(nrow+1, ncol+1);
    int *rowparity= malloc(sizeof(int)*(nrow+1)*2);

    apop_data *dt=apop_data_transpose(d);
    unbiasround(dt->matrix->data, imat->data, imat1->data, rowparity, &nrow, &ncol, r);
    apop_data_free(dt);
    int k=0;
    for (int j=0; j< ncol; j++, k++)
        for (int i=0; i< nrow; i++, k++)
            apop_data_set(d, i, j, imat->data[k]);
    gsl_matrix_int_free(imat);
    gsl_matrix_int_free(imat1);
    free(rowparity);
}

void rake_for_r(char **table_name){
	//Sadly, this function is entirely conversion of forms.
    /* TeaKEY(raking/input table, <<<The table to be raked.>>>)
    TeaKEY(raking/all vars, <<<The full list of variables that will be involved in the
       raking. All others are ignored.>>>)
    TeaKEY(raking/max iterations, <<<If convergence to the desired tolerance isn't 
       achieved by this many iterations, stop with a warning.>>>)
    TeaKEY(raking/count col, <<<If this key is not present take each row to be a
		single observation, and count them up to produce the cell counts to which the
		system will be raking. If this key is present, then this column in the data set
		will be used as the cell count.>>>)
    TeaKEY(raking/tolerance, <<<If the max(change in cell value) from one step to the next
       is smaller than this value, stop.>>>)
    TeaKEY(raking/run number, <<<If running several raking processes simultaneously via
        threading on the R side, specify a separate run\_number for each. If
        single-threading (or if not sure), ignore this.>>>)
     */
	Apop_stopif(!*table_name, return, 0, "I couldn't find a name for the input table "
                            "to the raking segment.");

    apop_data *all_vars_d = get_key_text("raking", "all vars");
    double max_iterations = get_key_float("raking", "max iterations");
	double tolerance = get_key_float("raking", "tolerance");
	double run_number = get_key_float("raking", "run number");

    /* TeaKEY(raking/thread count, <<<You can thread either on the R side among several tables,
     or interally to one table raking. To thread a single raking process, set this to the
     number of desired threads.>>>)
     */
	int thread_ct = get_key_float("raking", "thread count");
    int prior_threads= apop_opts.thread_count;
    if (thread_ct > 0) apop_opts.thread_count = thread_ct;

    /* TeaKEY(raking/structural zeros, <<<A list of cells that must always be zero, 
     in the form of SQL statements.>>>)
     */
	apop_data *zero_data = get_key_text("raking", "structural zeros");

	char *all_zeros = NULL;
	char *or = "    ";
	if (zero_data)
		for (int i=0; i < zero_data->textsize[0]; i++){
			char *r = all_zeros;
			asprintf(&all_zeros, "%s%s%s", all_zeros ? all_zeros : " ", or, zero_data->text[i][0]);
			or = " or ";
			free(r);
		}	

/* \key raking/contrasts The sets of dimensions whose column/row/cross totals must be kept constant. One contrast to a row; pipes separating variables on one row.
\begin{lstlisting}
raking{
    contrasts{
        age | sex | race
        age | block
    }
}
\end{lstlisting} */
	apop_data *contras = get_key_text("raking", "contrasts");
	int contrast_ct = contras ? contras->textsize[0]: 0;
	char *list_of_contrasts[contrast_ct];
	if (contras){
		for (int i=0; i < contrast_ct; i++)
			list_of_contrasts[i] = contras->text[i][0];
	}

	apop_data* outdata = apop_rake(.table_name = *table_name,
		.all_vars = all_vars_d ? all_vars_d ->text[0][0] : NULL,
		.contrasts = contras ? list_of_contrasts : 0,
		.contrast_ct = contrast_ct,
		.structural_zeros = all_zeros,
		.max_iterations= !gsl_isnan(max_iterations) ? max_iterations : 1000,
		.tolerance = !gsl_isnan(tolerance) ? tolerance : 1e-5,
		.count_col = get_key_word("raking", "count col"),
		.run_number = !gsl_isnan(run_number) ? run_number : 1,
		.nudge = get_key_float("raking", "count col"),
		.init_table = get_key_word("raking", "init table"),
		.init_count_col = get_key_word("raking", "init count col")
		);
	char *outname;
	asprintf(&outname, "%s_raked", *table_name);
	asprintf(table_name, "%s", outname); //report new name to R
	apop_table_exists(outname, 'd');
apop_opts.verbose=0;
apop_vector_show(outdata->weights);
	apop_data_print(outdata, outname, .output_type='d');
//    to_integer(outdata);

//Still not there
//	SEXP sexpout = outdata ? PEP_R_get_apop_data_matrix(outdata) : R_NilValue;

	free(outname);
	apop_data_free(outdata);
	apop_data_free(contras);
	apop_data_free(zero_data);
	apop_data_free(all_vars_d);
    apop_opts.thread_count = prior_threads;
}
