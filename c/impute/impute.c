//See readme file for notes.
#define __USE_POSIX //for strtok_r
#include "tea.h"
#include <rapophenia.h>
#include <stdbool.h>
#include "internal.h"
extern char *datatab;
void qxprintf(char **q, char *format, ...); //bridge.c
char *process_string(char *inquery, char **typestring); //parse_sql.c
void xprintf(char **q, char *format, ...); //parse_sql.c
char *strip (const char*); //peptalk.y
#define XN(in) ((in) ? (in) : "")
#define apop_strcmp(a, b) (((a)&&(b) && !strcmp((a), (b))) || (!(a) && !(b)))

apop_model relmodel; //impute/rel.c

data_frame_from_apop_data_type *rapop_df_from_ad;//alloced in PEPedits.c:R_init_tea

/* The imputation system is arguably the core of Tea; correspondingly, it is the
   most complex part. Fundamentally, all imputations have the same basic form: identify
that something is missing data, find some other sufficiently complete data
(herein, the donors), fit a specified model to that data, and draw from the 
estimated model to fill in the missing point.


From there, a lot of details have to be addressed. Borrowing from the network 
literature (among others), I use "ego" to indicate the observation that we are 
currently trying to fill in. Here is the detailed procedure.

--First, identify the subcategory of the data for the missing data point
	--Imputation is customarily done by subgroups: we fill in for Californians
		with other Californians, or fill in for rich kids with other rich kids.

--identify missing points, and candidates for estimation. This is get_nans_and_notnans().
	--Both are done at once because 80% of the work is the same, like the first
		step of identifying ego's category.

	--Having identified ego's category, we might as well fill in all NaNs for
		that category and variable at once. This makes the main for loop in get_nans_...
		more complex, but means we're running O(categories) queries instead of
		O(observations) queries. The output of this function is then two tables: egos
		and donors for the given category.

	--Donors need to be complete in all of the explanatory variables specified
		by the user as inputs to the model. If there is only one dependent variable
		(most casese) then I'll use records flagged for disclosure problems as donors.
		If multiple variables are to be filled in at once, I won't. 

	--If disclosure avoidance was done (the fingerprinting routine), check whether
		this variable is marked in that table; add thus-flagged id numbers to
		the to-fill-in table.

	--The output is the ego table and donor table for the category.

--Now, fill in missing values
	--The user gets to specify a model for each variable (soon, sets of variables).
	
	--This is what Apophenia was written for:
		apop_data *fitted_m=apop_estimate (donors, your_model); 
		double fill_me= apop_data_ptr(fill_ins, i, colname);
		apop_draw(&fill_me, rng, fitted_m);

	--There are many models that you can use. Notable:
		--The Multinomial distribution == randomized hot deck. We don't care
			about straight hot deck, which is an effort to simulate punched cards 
			as closely as possible.
		--One can implement a sequential regression setup by simply setting the
			first model to be univariate, the second a 2-D regression, then a 3-D
			regression, &c. Useful for loops; see below.
		--Other models we've implemented basically fall into distributions (Normal,
			 Multivariate Normal, Lognormal, Poisson, Kernel Density Estimation)
			and generalized linear methods (Logit & probit).


	--The interchangeable-model system will always be imperfect, so there are a few 
		tweaks to be made. After the fitted_m=apop_estimate (donors, your_model) step, 
		these are done if appropriate.
		--A Multinomial draw given N donors produces N draws, so change that to one unit.
		--The distribution given by the OLS model is incomplete: it depends 
			on the underlying distribution of the input variables [and this is not an
			Apophenia thing---it's the nature of OLS]. We specify a uniform prior. 
			Similarly for logit and probit.
	--Write-ups for sequential regression methods typically suggest looping back
		and re-doing the regression as often as necessary. NaN observations at the
		first round become donors in subsequent rounds.
		--This is not yet implemented---it takes some bookkeeping. 

--Draw
	--As above, this step is one line for a straight draw.
	--Run consistency_check, and throw out if the draw doesn't pass
	--We want to multiply impute, which means just repeating the draw step as often as needed.
	--the output is a multi-impute table, with a row/col coordinate for what's
		being filled in, plus a column per imputation.
	--So far we've been talking about the straight draw, but there are other options
		These will generally make sense via the multinomial model
		--MLE
		--nearest neighbor, or probabilistic n.n., where the model is simply a distance function.

--Anybody left? If somebody didn't have enough data in their category to produce good estimates,
    remove the bottom-most category from the list and re-run.

Now that you have the output of the imputations, there's a function to use that 
table to produce variances on each imputation, and another function to fill in 
the original data.

To do:
	SRMI
	Bayesian updating, generally (versus just using a Dirichlet)
	rowids aren't quite correctly implemented
	clusters of variables to simultaneously fill.
	function to fill in from the impute table
		needs a decision from the user on how: mean? draw 1?

the players
    the data always lives in the db table. pull by id
    we get one row for the names in there somewhere.
    the impustruct array has one specification for each model, which typically means for each variable
    isnan, notnan: the ego and donor tables, as above.

*/

typedef struct {
	apop_model base_model, *fitted_model;
	char * depvar, **allvars, *vartypes, *selectclause;
	int position, allvars_ct, error;
	apop_data *isnan, *notnan;
    bool is_bounds_checkable, is_hotdeck, textdep, is_rake, is_regression;
} impustruct;


////////////////////////

/* This function is a hack under time pressure by BK, intended 
    to allow raking to use the fingerprinting output. I just blank
    out the results from the fingerprinting and do the imputation. 
    In an effort to be conservative, I blank out only the most 
    implicated field in each record.
    So you'll want to run a for loop that re-checks the vflags.
    I'll have to see later if that creates problems...

    Input is **datatab for the convenience of R.

*/
int blank_fingerprints(char const **datatab){
    if (!apop_table_exists("vflags")) return 0;
    char *id = get_key_word(NULL, "id");
    
    sprintf(apop_opts.db_name_column, "%s", id);
    apop_data *prints = apop_query_to_data("select * from vflags");
    Apop_stopif(!prints || prints->error, return -1, 0, 
            "vflags exists, but select * from vflags failed.");
    for (int i=0; i< prints->matrix->size1; i++){
        //mm is a matrix with one row; v is the vector view.
        Apop_submatrix(prints->matrix, i, 0, 1, prints->matrix->size2-2, mm);
        Apop_matrix_row(mm, 0, v);

        apop_query("update %s set %s = null where %s+0.0 = %s",
                *datatab, 
                prints->names->column[gsl_vector_max_index(v)],
                id, prints->names->row[i]
                );
    }
    return 0;
}

////////////////////////




///// second method: via raking

/* Zero out the weights of those rows that don't match. 
 * Thanks to copy_by_name, we know that the two input data sets match.
 */
double cull(apop_data *onerow, void *subject_row_in){
    apop_data *subject_row = subject_row_in;
    for (int i=0; i< subject_row->matrix->size2; i++){
        double this = apop_data_get(subject_row, .col=i);
        if (isnan(this)) continue;
        if (apop_data_get(onerow, .col=i) != this) {
            *onerow->weights->data = 0;
            return 0;
        }
    }
    return 0;
}

/* We're depending on the data columns and the draw columns to
   be the same fields, and we aren't guaranteed that the raking
   gave us data in the right format(BK: check this?). Copy the 
   raked output to explicitly fit the data format. */
apop_data *copy_by_name(apop_data *data, apop_data *form){
    apop_data *out= apop_data_copy(form);
    gsl_matrix_set_all(form->matrix, NAN);
    for (int i=0; i< data->matrix->size2; i++){
        double this = apop_data_get(data, .col=i);
        if (isnan(this)) continue; //don't bother.
        int corresponding_col = apop_name_find(out->names, data->names->column[i], 'c');
        if (corresponding_col!=-2) 
            apop_data_set(out, 0, corresponding_col, this);
    }
    return out;
}

/* We have raked output, and will soon be making draws from it. 
   So it needs to be a PMF of the right format to match the data point
   we're trying to match.  
   
   It may be that the row in the data has no match in the raked outout, due to structural
   zeros and the inputs. E.g., if a person is married and has missing age, and everybody
   else in the data set is under 15, and (age <15 && married) is an edit failure, there will
   be no entries in the rake table with married status. In this case, blank out the existing married status.
   
   */
apop_model *prep_the_draws(apop_data *raked, apop_data *fin, gsl_vector *orig){
    Apop_stopif(!raked, return NULL, 0, "NULL raking results.")
    Apop_stopif(isnan(apop_sum(raked->weights)), return NULL, 0, "NaNs in raking results.")
    Apop_stopif(!apop_sum(raked->weights), return NULL, 0, "No weights in raking results.")
    bool done=false;
    Apop_data_row(raked, 0, firstrow);
    while (!done) {
        apop_data *cp = copy_by_name(fin, firstrow);
        apop_data_free_base(
            apop_map(raked, .fn_rp=cull, .param=cp) );
        done=apop_sum(raked->weights);
        if (!done){
            gsl_vector_memcpy(raked->weights, orig);
            for (int i=0; i< fin->matrix->size2; i++) 
                if (!isnan(apop_data_get(fin, 0, i))) {
                    apop_data_set(fin, 0, i, NAN);    //modify the input data.
                    Apop_notify(1, "Blanking the %s field because I couldn't "
                            "find a match otherwise.", fin->names->column[i]);
                    break;
                }
        }
        apop_data_free(cp);
    }
    return apop_estimate(raked, apop_pmf);
}

static void rake_to_completion(const char *datatab, const char *underlying, 
        impustruct is, const int min_group_size, gsl_rng *r, 
        const int draw_count, char *catlist, 
        const apop_data *fingerprint_vars, const char *id_col, 
        char const *weight_col, char const *fill_tab, char const *margintab){
    apop_data as_data = (apop_data){.textsize={1,is.allvars_ct}, .text=&is.allvars};
    char *varlist = apop_text_paste(&as_data, .between=", ");
    apop_data *d = apop_query_to_data("select %s, %s %c %s from %s %s %s", id_col, varlist, 
                    weight_col ? ',' : ' ',
                    XN(weight_col), datatab, catlist ? "where": " ", XN(catlist));
    Apop_stopif(!d, return, 0, "Query for appropriate data returned no elements. Nothing to do.");
    Apop_stopif(d->error, return, 0, "query error.");
    apop_data *notnan = apop_data_listwise_delete(d);
    Apop_stopif(notnan && notnan->error, return, 0, "listwise deletion error");
    //if notnan=NULL or < 1/2 the full data set, then raking will use the default all-one init table.
    bool notnan_tab_big_enough = notnan && notnan->matrix->size1 > 0.5*d->matrix->size1;
    if (notnan_tab_big_enough){
        begin_transaction();
        apop_data_print(notnan, .output_file="notnan", .output_append='w', .output_type='d');
        commit_transaction();
        apop_data_free(notnan);
    }

    char *zeros=NULL, *or="";
    for (int i=0; edit_list[i].clause; i++){
        asprintf(&zeros, "%s%s(%s)", XN(zeros), or, edit_list[i].clause);
        or = " or ";
    }

    begin_transaction();
    apop_data *raked =
        apop_rake(.margin_table=margintab ? margintab : datatab,
                .var_list=is.allvars, .var_ct=is.allvars_ct,
                .contrasts=is.allvars, .contrast_ct=is.allvars_ct,
                .count_col=weight_col, .init_table=notnan_tab_big_enough ? "notnan": NULL, 
                .structural_zeros=zeros,
                .init_count_col=notnan_tab_big_enough ? weight_col: NULL);
    commit_transaction();
    Apop_stopif(!raked, return, 
                0, "Raking returned a blank table. This shouldn't happen.");
    Apop_stopif(raked->error, return,
                0, "Error (%c) in raking.", raked->error);
    apop_data_pmf_compress(raked); //probably a no-op, but just in case.

    int batch_size=1000; //just in case...
    gsl_vector *original_weights=apop_vector_copy(raked->weights);
    gsl_vector *cp_to_fill = gsl_vector_alloc(d->matrix->size2);
    apop_data *fillins = apop_data_alloc();
    for (size_t i=0; i< d->matrix->size1; i++){
        Apop_row(d, i, focusv); //as vector
        if (!isnan(apop_sum(focusv))) continue;
        Apop_data_row(d, i, focus); //as data set w/names

        //draw the entire row at once, but write only the NaN elmts to the filled tab.
        apop_model *m = prep_the_draws(raked, focus, original_weights); 
        if (!m || m->error) {apop_model_free(m); continue;} //get it on the next go `round.
        size_t len=0;
        for (int drawno=0; drawno< draw_count; drawno++){
            Apop_stopif(!focus->names, continue, 0, "focus->names is NULL. This should never have happened.");
            apop_draw(cp_to_fill->data, r, m);
            for (size_t j=0; j< focus->matrix->size2; j++)  
                if (isnan(apop_data_get(focus, .col=j))){
                    len = *fillins->textsize;
                    fillins->matrix = apop_matrix_realloc(fillins->matrix, len+1, 2);
                    apop_text_alloc(fillins, len+1, 2);
                    Apop_stopif(fillins->error, return, 0, "Something wrong with the table of fill-ins when drawing from the imputation model. Out of memory?");
                    apop_text_add(fillins, len, 0, *focus->names->row);
                    apop_text_add(fillins, len, 1, focus->names->column[j]);
                    gsl_matrix_set(fillins->matrix, len, 0, drawno);
                    gsl_matrix_set(fillins->matrix, len, 1, cp_to_fill->data[j]);
                }
            if (len > batch_size){
                begin_transaction();
                apop_data_print(fillins, .output_file=fill_tab, .output_type='d', .output_append='a');
                commit_transaction();
                gsl_matrix_free(fillins->matrix);
                fillins->matrix = NULL;
                apop_text_alloc(fillins, 0, 0);
            }
        }
        apop_model_free(m);
        gsl_vector_memcpy(raked->weights, original_weights);
    }
    begin_transaction();
    if (fillins->matrix) apop_data_print(fillins, .output_file=fill_tab, .output_type='d', .output_append='a');
    commit_transaction();
    apop_data_free(fillins);
    gsl_vector_free(cp_to_fill);
    gsl_vector_free(original_weights);
    apop_data_free(d);
}


	
static void lil_ols_draw(double *out, gsl_rng *r, apop_model *m){
    double temp_out[m->parameters->vector->size+1];
    m->draw = apop_ols.draw;
    apop_draw(temp_out, r, m);
    m->draw = lil_ols_draw;
    *out = temp_out[0];
}

static char *construct_a_query(char const *datatab, char const *underlying, char const *varlist, 
                                apop_data const *category_matrix, char const *id_col, int ego_id, char *depvar){
/* Find out which constraints fit the given record, then join them into a query.
   The query ends in "and", because get_constrained_page will add one last condition.  */

	//These ad hoc indices help in sqlite. ---except they're commented out right
	//now, due to a rush project.
/*    if (categories_left){ //maybe make a subindex
        int catct=0;
	 	char *idxname =strdup("idx_"); 
        for (int i=0; i< category_matrix->textsize[0] && catct < categories_left; i++)
		//index only the active categories, of course, and only those that aren't a pain to parse.
	    if (active_cats[i] && !apop_regex(category_matrix->text[i][0], "[()<>+*!%]")){
		    catct++;
	   apop_data *test_for_index = apop_query_to_data("select * from sqlite_master where name='%s'", idxname);
	   if (!test_for_index){
           //create index idx_a_b_c on datatab(a, b, c);
           char *make_idx, comma =' ';
           asprintf(&make_idx, "create index %s on %s (", idxname, underlying);
           for (int i=0, catct=0; i< category_matrix->textsize[0] && catct < categories_left; i++)
               if (active_cats[i]){
                   catct++;
                   qxprintf(&make_idx, "%s%c %s", make_idx, comma, category_matrix->text[i][0]);
                   if (strchr(make_idx,'=')) *strchr(make_idx,'=')='\0'; //cat is prob. varname=x, cut string at =.
                   comma = ',';
               }
           apop_query("%s)", make_idx);
           apop_data_free(test_for_index);
	   }
    }
	}
*/
    char *q;
    asprintf(&q, "select %s from %s where ", varlist, datatab);
    if (!category_matrix) return q;
    //else
    for (int i=0; i< category_matrix->textsize[0]; i++){
        char *n = *category_matrix->text[i]; //short name
        if (strcmp(n, depvar)) //if n==depvar, you're categorizing by the missing var.
            qxprintf(&q, "%s (%s) = (select %s from %s where %s = %i) and\n",  
                           q,  n,           n,  datatab, id_col, ego_id);
    }
    return q;
}

static char *construct_a_query_II(char const *id_col, const impustruct *is, apop_data const *fingerprint_vars){
    /* Do we need the clause that includes the requisite items from the vflags table? */
    if (!fingerprint_vars || !apop_table_exists("vflags")) return NULL;
    char *q2 = NULL;
    int is_fprint=0; 
    for (int i=0; !is_fprint && i< fingerprint_vars->textsize[0]; i++)
        if (!strcasecmp(fingerprint_vars->text[i][0], is->depvar))
            is_fprint++;
    if (is_fprint)
        asprintf(&q2, "select %s from vflags where %s > 0", is->selectclause, is->depvar);
    return q2;
}

void install_data_to_R(apop_data *d, apop_model *m){
    R_model_settings *settings = Apop_settings_get_group(m, R_model);
    if (!settings) return; //not an R model.

    SEXP env;
    if (!settings->env){
        PROTECT(env = allocSExp(ENVSXP));
        SET_ENCLOS(env, R_BaseEnv);
        settings->env = env;
    } else {
        PROTECT(env = settings->env);
    }
    defineVar(mkChar("data"), rapop_df_from_ad(d), env);
    UNPROTECT(1);
}

apop_data * get_data_from_R(apop_model *m){
    R_model_settings *settings = Apop_settings_get_group(m, R_model);
    if (!settings) return NULL; //not an R model.

    static int is_inited;
    static apop_data_from_frame_type *rapop_ad_from_df;
    if (!is_inited)
        rapop_ad_from_df =  (void*) R_GetCCallable("Rapophenia", "apop_data_from_frame");

    SEXP env;
    PROTECT(env = settings->env);
    assert(TYPEOF(env) == ENVSXP);
    SEXP data_as_sexp = findVar(mkChar("data"), env);
    UNPROTECT(1);
    return rapop_ad_from_df(data_as_sexp);
}

apop_data *nans_no_constraints, *notnans_no_constraints;
char *last_no_constraint;

void verify(impustruct is){
    /*Apop_stopif(is.isnan && !is.notnan, return, 0, "Even with no constraints, I couldn't find any "
                "non-NaN records to use for fitting a model and drawing values for %s.", is.depvar);*/
    if (is.notnan){
        int v= apop_opts.verbose; apop_opts.verbose=0;
        Apop_stopif(gsl_isnan(apop_matrix_sum((is.notnan)->matrix)+apop_sum((is.notnan)->vector)), 
                return, 0, 
                "NULL values or infinities where there shouldn't be when fitting the model for %s.", is.depvar);
        apop_opts.verbose=v;
    }
    Apop_assert_c(is.isnan, , 2, "%s had no missing values. This sometimes happens when the fields used for sub-classes "
            "still has missing values. Perhaps do an imputation for these fields and add an 'earlier output table' line to "
            "this segment of the spec.", is.depvar);
}

/* Generate two tables: those that have Nulls and therefore need to be
   considered; those that don't have nulls, and therefore can be used to fit
   models.

   The generation here is almost symmetric, but usage is asymmetric. The isnans are mostly used only for
   the rowids of those who need filling in, and are read as text for the consistency_check system; the 
	notnans are a data set for model estimation, so rowids are irrelevant. 

   The information for both is there should you want to use it.

	Given a list of other flaws---consistency failures, disclosure---we've gotta decide 
	how we're gonna fit to those models. 

	Currently, one method involves hot-deck --> not really any ML anything
	OLS finds best fit using the rest of sample, not the item that needs
	subbing, and so it may pull further from the data than is useful; also, our
	version (probably most versions) goes one var. at a time --> leans toward one
	var over the others.
*/
static void get_nans_and_notnans(impustruct *is, int index, char const *datatab, 
        char const *underlying, int min_group_size, apop_data const *category_matrix, 
        apop_data const *fingerprint_vars, char const *id_col){
	is->isnan = NULL;
    char *q, *q2;
    if (!category_matrix || *category_matrix->textsize == 0){
        if (nans_no_constraints){//maybe recycle what we have.
            if(apop_strcmp(last_no_constraint, is->selectclause)){
                is->isnan = nans_no_constraints;
                is->notnan = notnans_no_constraints;
                return;
            } else { //this isn't the right thing; start over
                apop_data_free(nans_no_constraints);
                apop_data_free(notnans_no_constraints);
                is->isnan = is->notnan = NULL;
                last_no_constraint = strdup(is->selectclause);
            }
        }//else, carry on:
        asprintf(&q, "select %s from %s where 1 and ", is->selectclause, datatab);
    } else
        q = construct_a_query(datatab, underlying, is->selectclause, category_matrix, id_col, index, is->depvar);
    q2 = construct_a_query_II(id_col, is, fingerprint_vars);

    if (!strcmp(is->vartypes, "all numeric"))
        is->notnan = q2? apop_query_to_data("%s %s is not null except %s", q, is->depvar, q2) : apop_query_to_data("%s %s is not null", q, is->depvar);
    else is->notnan = q2? 
                 apop_query_to_mixed_data(is->vartypes, "%s %s is not null except %s", q, is->depvar, q2)
               : apop_query_to_mixed_data(is->vartypes, "%s %s is not null", q, is->depvar);
    apop_data_listwise_delete(is->notnan, .inplace='y');
    if (verbose){
        printf("-------------- Not NaN:");
        apop_data_show(is->notnan);
        printf("--------------");
    }
    if (!strcmp(is->vartypes, "all numeric")){
         is->isnan = q2 ? 
              apop_query_to_data("%s %s is null union %s", q, is->depvar, q2)
            : apop_query_to_data("%s %s is null", q, is->depvar);
         //but we'll need the text for the consistency checking anyway.
         apop_text_alloc(is->isnan, is->isnan->matrix->size1, is->isnan->matrix->size2);
         for (int i=0; i< is->isnan->matrix->size1; i++)
             for (int j=0; j< is->isnan->matrix->size2; j++)
                 apop_text_add(is->isnan, i, j, "%g", apop_data_get(is->isnan, i, j));
    } else is->isnan = q2 ? 
              apop_query_to_text("%s %s is null union %s", q, is->depvar, q2)
            : apop_query_to_text("%s %s is null", q, is->depvar);
    free(q); q=NULL;
    free(q2); q2=NULL;
    verify(*is);
}

//Primarily just call apop_estimate, but also make some minor tweaks.
static void model_est(impustruct *is, int *model_id){
    apop_data *notnan = is->notnan; //just an alias.
    assert(notnan);
    /*for(int i=0; i< notnan->textsize[1]; i++){
        notnan->matrix=apop_matrix_realloc(notnan->matrix, 
                            notnan->matrix ? notnan->matrix->size1   : *notnan->textsize, 
                            notnan->matrix ? notnan->matrix->size2+1 : 1);
        apop_data_to_factors(notnan, .incol=i, .outcol=notnan->matrix->size2-1);
    }*/
    //maybe check here for constant columns in regression estimations.
    if (notnan->text && !apop_data_get_page(notnan,"<categories"))
        for (int i=0; i< notnan->textsize[1]; i++){
            if (!is->is_hotdeck && is->textdep && strcmp(notnan->names->column[i], is->depvar) )
                apop_data_to_dummies(notnan, i, .keep_first='y', .append='y');
            //the actual depvar got factor-ized in prep_for_draw.
//            apop_data_to_dummies(is->isnan, i, .keep_first='y', .append='y'); //presumably has the same structure.
        }
    Apop_stopif(notnan->vector && isnan(apop_vector_sum(notnan->vector)), return, 
            0, "NaNs in the not-NaN vector that I was going to use to estimate "
            "the imputation model. This shouldn't happen")
    Apop_stopif(notnan->matrix && isnan(apop_matrix_sum(notnan->matrix)), return,
            0, "NaNs in the not-NaN matrix that I was going to use to estimate "
            "the imputation model. This shouldn't happen")
    if (is->is_hotdeck) apop_data_pmf_compress(notnan);
	//Apop_model_add_group(&(is->base_model), apop_parts_wanted); //no extras like cov or log like.

    install_data_to_R(notnan, &is->base_model); //no-op if not an R model.
	is->fitted_model = apop_estimate(notnan, is->base_model);
    Apop_stopif(!is->fitted_model, return, 0, "model fitting fail.");
    if (apop_strcmp(is->base_model.name, "multinomial"))
        apop_data_set(is->fitted_model->parameters, .row=0, .col=-1, .val=1);
    if (apop_strcmp(is->base_model.name, "Ordinary Least Squares"))
        is->fitted_model->draw=lil_ols_draw;
    if (verbose) apop_model_print(is->fitted_model);
    (*model_id)++;
    apop_query("insert into model_log values(%i, 'type', '%s');", *model_id, is->fitted_model->name);
    if (is->fitted_model->parameters && is->fitted_model->parameters->vector) //others are hot-deck or kde-type
        for (int i=0; i< is->fitted_model->parameters->vector->size; i++){
            char *param_name = NULL;
            if (is->fitted_model->parameters->names->rowct > i)
                param_name = is->fitted_model->parameters->names->row[i];
            else {
                free(param_name);
                asprintf(&param_name, "param %i", i);
            }
            apop_query("insert into model_log values(%i, '%s', %g);",
                    *model_id, param_name,
                    is->fitted_model->parameters->vector->data[i]);
        }
    apop_query("insert into model_log values(%i, 'subuniverse size', %i);"
                    , *model_id, (int) (is->fitted_model->data->matrix 
                                    ? is->fitted_model->data->matrix->size1
                                    : *is->fitted_model->data->textsize));
    /*if (is->fitted_model->parameters->vector)
        assert(!isnan(apop_sum(is->fitted_model->parameters->vector)));*/
}

static void prep_for_draw(apop_data *notnan, impustruct *is){
    apop_lm_settings *lms = apop_settings_get_group(is->fitted_model, apop_lm);
    if (is->textdep) apop_data_to_factors(is->notnan);
    if (lms){
        apop_data *p = apop_data_copy(notnan);
        p->vector=NULL;
        lms->input_distribution = apop_estimate(p, apop_pmf);
        is->fitted_model->dsize=1;
    }
}


/*Here, we check that the variable is within declared bounds. We have to do this
  because the edit system in consistency_check will assume that all variables are 
  within their declared range. By doing the checking here, we may even save some trips
  to the consistency checker.

Return: 1=input wasn't in the list, was modified to nearest value
        0=input was on the list, wasn't modified
        0=input variable has no associated checks.
  If function returns a 1, you get to decide whether to use the rounded value or re-draw.

  If text or real, there's no rounding to be done: it's valid or it ain't. If numeric, then we can do rounding as above.
  I only check for 't' for text; suggest 'n' for numeric but it's up to you.
*/
int check_bounds(double *val, char const *var, char type){
//Hey, B: Check the SQL that is about only this variable. This will need to be modified for multi-variable models.
//After checking the SQL, check the declarations.

    char *val_as_text;
    asprintf(&val_as_text, "%g", *val);
    int rowid = ri_from_ext(var, val_as_text);
    free(val_as_text); 
    if (rowid >=0) return 0;
    //else, try to round it.
    if (type=='r' || type=='c') return 0;
/*    if (type =='c')
		return !!apop_query_to_float("select count(*) from %s where %s = '%s'",
var, var, val);*/
    else {//integer type
        double closest = find_nearest_val(var, *val);
        if (fabs(closest-*val) > 0){//may overdetect for some floats
            if (verbose) printf("rounding %g -> %g\n", *val , closest);
            *val = closest;
            return 1;
        }
    }
    return 0;
}

void R_check_bounds(double *val, char **var, int *fails){
    if (ri_from_ext(*var, "0") == -100) {//-100=var not found.
        *fails=0; 
        return;
    }
	*fails = check_bounds(val, *var, 'i');
}

    /* notes for check_records: 
		I have on hand all of the imputation models, so I can make new draws as desired.
      I expect all N replications to work together, so they all have to be tested.

      --for each NaN found, impute if there's a spec for it.
      --do{
          --run the consistency check, requesting failed var.s
          --if no failure, break.
          --generate a list of failed var.s with specs.
          --if empty, display a fault---I need a model to reconcile this---and break
          --randomly pick a failed var with a spec., impute.
     } while(1); //because the internal breaks are what will halt this.

      If we fill a record and find that it doesn't work, randomly select one variable to re-impute, 
      then try again. Max likelihood methods that would use the alternatives table that 
      consistency_check can produe are reserved for future work.
      */

static apop_data *get_all_nanvals(impustruct is, const char *id_col, const char *datatab){
    //db_name_column may be rowid, which would srsly screw us up.
    //first pass: get the full list of NaNs; no not-NaNs yet.
    apop_data *nanvals = apop_query_to_data("select %s, %s from %s where %s is null",
									 id_col, is.depvar, datatab, is.depvar);
    if (!nanvals) return NULL; //query worked, nothing found.
    Apop_stopif(nanvals->error, return NULL, 0, "Error querying for missing values.");
    if (verbose){
        printf("For %s, the following NULLs:\n", is.depvar);
        apop_data_show(nanvals); //may print NULL.
    }
    return nanvals;
}

//we're guaranteed to find it, so this will run fast---and we're gonzo and don't
//do checks. We start the ctr at the last found value and allow it to loop around.
static void mark_an_id(int *ctr, const char *target, char * const *list, int len){
    int tries=0;
    for (int tries=0; tries < len && !apop_strcmp(target, list[*ctr]); tries++)
        (*ctr)= (*ctr+1) % len;
    if (tries == len){
        *ctr = 0;
        Apop_assert_c(0,  , 0, "%s isn't in the main list of nans.", target);
    }
    sprintf(list[*ctr], ".");
}

typedef struct {
    char *textx;
    double pre_round;
    int is_fail;
} a_draw_struct;

/*This function is the inner loop cut out from impute(). As you can see from the list of
arguments, it really doesn't stand by itself.

The draw itself is one line---just call apop_draw. The hard part is in checking that the draw is OK.
This involves bounds-checking, if applicable, 
then generating a dummy version of the observation that is in an R-friendly format, 
then sending it to consistency_check for an up-down vote.

The parent function, make_a_draw, then either writes the imputation to the db or tries this fn again.
*/
static a_draw_struct onedraw(gsl_rng *r, impustruct *is, 
        char type, int id_number, int fail_id, 
        int model_id, apop_data *full_record, int col_of_interest){
    a_draw_struct out = { };
	static char const *const whattodo="passfail";
    double x;
    apop_draw(&x, r, is->fitted_model);
    Apop_stopif(isnan(x), return out, 0, "I drew NaN from the fitted model. Something is wrong.");
    apop_data *rd = get_data_from_R(is->fitted_model);
    if (rd) {
        x = rd->vector ? *rd->vector->data : *rd->matrix->data;
        apop_data_free(rd);
    }
    out.pre_round=x;
    if (type == '\0'){ // '\0' means not in the index of variables to check.
        asprintf(&out.textx, "%g", x);
    } else {
        if (!is->is_hotdeck && is->is_bounds_checkable) //inputs all valid ==> outputs all valid
            check_bounds(&x, is->depvar, type); // just use the rounded value.
        apop_data *f;
        if (is->textdep && (f = apop_data_get_factor_names(is->fitted_model->data, .type='t')))
             out.textx = strdup(*f->text[(int)x]);
        else asprintf(&out.textx, "%g", x);
/*        apop_query("insert into impute_log values(%i, %i, %i, %g, '%s', 'cc')",
                                id_number, fail_id, model_id, out.pre_round, out.textx);
*/
        //copy the new impute to full_record, for re-testing
        apop_text_add(full_record, 0, col_of_interest, "%s", out.textx);
        int size_as_int =*full_record->textsize;
//printf("Gut says: textx=%s; preround=%g, is_fail=%i\n", out.textx, out.pre_round, out.is_fail);
        consistency_check((char *const *)(full_record->names->text ? full_record->names->text : full_record->names->column),
                          (char *const *)full_record->text[0],
                          &size_as_int,
                          &whattodo,
                          &id_number,
                          &out.is_fail,
                          NULL);//record_fails);
    }
//printf("Out says: textx=%s; preround=%g, is_fail=%i\n", out.textx, out.pre_round, out.is_fail);
    return out;
}

//a shell for do onedraw() while (!done).
static void make_a_draw(impustruct *is, gsl_rng *r, int fail_id,
                        int model_id, int draw, apop_data *nanvals, char *filltab){
    int done_ctr = 0; //for marking what's done.
    char type = get_coltype(is->depvar);
    int col_of_interest=apop_name_find(is->isnan->names, is->depvar, type !='c' && is->isnan->names->colct ? 'c' : 't');
    Apop_stopif(col_of_interest < -1, return, 0, "I couldn't find %s in the list of column names.", is->depvar);
    for (int rowindex=0; rowindex< is->isnan->names->rowct; rowindex++){
        int tryctr=0;
        int id_number = atoi(is->isnan->names->row[rowindex]);
        a_draw_struct drew;
        Apop_data_row(is->isnan, rowindex, full_record);
        do drew = onedraw(r, is, type, id_number, fail_id, 
                          model_id, full_record, col_of_interest);
        while (drew.is_fail && tryctr++ < 1000);
        Apop_stopif(drew.is_fail, , 0, "I just made a thousand attempts to find an "
            "imputed value that passes checks, and couldn't. "
            "Something's wrong that a computer can't fix.\n "
            "I'm at id %i.", id_number);
        char * final_value = (type=='c') 
                                ? ext_from_ri(is->depvar, drew.pre_round+1)
                                : strdup(drew.textx); //I should save the numeric val.
        apop_query("insert into %s values(%i, '%s', '%s', '%s');",
                       filltab,  draw, final_value, is->isnan->names->row[rowindex], is->depvar);
        free(final_value);
        free(drew.textx);
        mark_an_id(&done_ctr,is->isnan->names->row[rowindex], nanvals->names->row, nanvals->names->rowct);
    }
}

double still_is_nan(apop_data *in){return strcmp(*in->names->row, ".");}

/* As named, impute a single variable.
   We check bounds, because those can be done on a single-variable basis.
   But overall consistency gets done on the whole-record basis. 
   
    The main i-indexed loop is really just a search for representatives
    of every subgroup. The rowindex-indexed loop is where the filled table is actually filled.

    within the i loop:
        Get those in the category matching this guy, split by those having the
        relevant variable and those with NaNs at the var.
        Set the imputation model based on those w/o NaNs.
        Loop (rowindex-loop) over those with NaNs to write imputations to table of filled values
   
   */
static void impute_a_variable(const char *datatab, const char *underlying, impustruct *is, 
        const int min_group_size, gsl_rng *r, const int draw_count, apop_data *category_matrix, 
        const apop_data *fingerprint_vars, const char *id_col, char *filltab,
        char *previous_filltab){
    static int fail_id=0, model_id=-1;
    apop_data *nanvals = get_all_nanvals(*is, id_col, datatab);
    if (!nanvals) return;
    char *dt;
    char *dataxxx = (char*)datatab; //can't constify checkout, because of R

    //if there is a previous fill tab, then we need to do a re-estimation
    //of the model every time. If not, then we do one est & many draws.
    int outermax = previous_filltab ? draw_count : 1;
    int innermax = previous_filltab ? 1 : draw_count;

    apop_name *clean_names = NULL;
    if (outermax > 1) clean_names = apop_name_copy(nanvals->names);

    for (int outerdraw=0; outerdraw < outermax; outerdraw++){
        if (previous_filltab){
            asprintf(&dt, "%s_copy", datatab);
            check_out_impute(&dataxxx, &dt, &outerdraw, NULL, &previous_filltab);
        } else dt=strdup(datatab);
        begin_transaction();

        is->is_bounds_checkable = (ri_from_ext(is->depvar, "0") != -100); //-100=var not found.

        bool still_has_missings=true, hit_zero=false;
        do {
            for (int i=0; i < nanvals->names->rowct; i++){ //see notes above.
                if (apop_strcmp(nanvals->names->row[i], ".")) continue; //already got this person.
                get_nans_and_notnans(is, atoi(nanvals->names->row[i]) /*ego_id*/, 
                        dt, underlying, min_group_size, category_matrix, fingerprint_vars, id_col);

                if (!is->isnan) goto bail; //because that first guy should've been missing.
                if (!is->notnan || GSL_MAX((is->notnan)->textsize[0]
                            , (is->notnan)->matrix ? (is->notnan)->matrix->size1: 0) < min_group_size)
                    goto bail;
                is->is_hotdeck = (is->base_model.estimate == apop_multinomial.estimate 
                                        ||is->base_model.estimate ==apop_pmf.estimate);
                model_est(is, &model_id); //notnan may be pmf_compressed here.
                prep_for_draw(is->notnan, is);
                for (int innerdraw=0; innerdraw< innermax; innerdraw++)
                    make_a_draw(is, r, ++fail_id, model_id, 
                                    GSL_MAX(outerdraw, innerdraw), nanvals, filltab);
                apop_model_free(is->fitted_model); //if (is_hotdeck) apop_data_free(is->fitted_model->data);
                bail:
                apop_data_free(is->notnan);
                apop_data_free(is->isnan);
            }
            /*shrink the category matrix by one, then loop back and try again if need be. This could be more efficient, 
              but take recourse in knowing that the categories that need redoing are the ones with 
              few elements in them.*/
            if (category_matrix && *category_matrix->textsize>0)
                apop_text_alloc(category_matrix, category_matrix->textsize[0]-1, category_matrix->textsize[1]);
            else {
                hit_zero++;
                nans_no_constraints= is->isnan;
                notnans_no_constraints= is->notnan;
                last_no_constraint = strdup(is->selectclause);
            }
            still_has_missings = apop_map_sum(nanvals, .fn_r=still_is_nan);
        } while (still_has_missings && !hit_zero);
        if (previous_filltab) apop_table_exists(dt, 'd');
        commit_transaction();
        if (still_has_missings && hit_zero) printf("Even with no constraints, I still "
                             "couldn't find enough data to model the data set.");
        if (outermax > 1){
            apop_name_free(nanvals->names);
            nanvals->names = apop_name_copy(clean_names);
        }
    }
    apop_data_free(nanvals);
    apop_name_free(clean_names);
}

apop_model tea_get_model_by_name(char *name, impustruct *model){
    static get_am_from_registry_type *rapop_model_from_registry;
    static int is_inited=0;
    if (!is_inited && using_r)
        rapop_model_from_registry = (void*) R_GetCCallable("Rapophenia", "get_am_from_registry");

    apop_model out= apop_strcmp(name, "normal")
          ||apop_strcmp(name, "gaussian")
				? apop_normal :
			apop_strcmp(name, "multivariate normal")
				? apop_multivariate_normal :
			apop_strcmp(name, "lognormal")
				? apop_lognormal :
			apop_strcmp(name, "rake")
		  ||apop_strcmp(name, "raking")
				?  (model->is_rake = true, (apop_model){.name="null model"}) :
			apop_strcmp(name, "hotdeck")
		  ||apop_strcmp(name, "hot deck")
	      ||apop_strcmp(name, "multinomial")
		  ||apop_strcmp(name, "pmf")
				? (model->is_hotdeck=true, apop_pmf) :
			apop_strcmp(name, "poisson")
				? (model->is_regression=true, apop_poisson) :
			apop_strcmp(name, "ols")
				? (model->is_regression=true, apop_ols) :
			apop_strcmp(name, "logit")
				? (model->is_regression=true, apop_logit) :
			apop_strcmp(name, "probit")
				? (model->is_regression=true, apop_probit) :
			apop_strcmp(name, "rel")
				? relmodel :
			apop_strcmp(name, "kernel")
	      ||apop_strcmp(name, "kernel density")
				? apop_kernel_density 
				: (apop_model) {.name="Null model"};
        if (using_r && apop_strcmp(out.name, "Null model")) //probably an R model.
            out= *rapop_model_from_registry(name);
/*        Apop_assert(!apop_strcmp(out.name, "Null model"), 
                "I couldn't find the model named '%s' in my list of supported "
                "models (or among models you added using setupRapopModel.", name);
*/
        Apop_stopif(!strcmp(out.name, "Null model"), return (apop_model){}, 0, "model selection fail.");
        Apop_model_add_group(&out, apop_parts_wanted, .predicted='y'); //no cov
        if (!strcmp(out.name, "PDF or sparse matrix")) out.dsize=-2;
        return out;
}

void prep_imputations(char *configbase, char *id_col, gsl_rng **r, char *filltab){
    int seed = get_key_float(configbase, "seed");
    *r = apop_rng_alloc((!isnan(seed) && seed>=0) ? seed : 35);
    apop_table_exists("impute_log", 'd');
    apop_query("create table impute_log (%s, 'fail_id', 'model', 'draw', 'declared', 'status')", id_col);
    if (!apop_table_exists("model_log"))
        apop_query("create table model_log ('model_id', 'parameter', 'value')");
}

impustruct read_model_info(char const *configbase, char const *tag, char const *id_col){
	apop_data *vars=NULL, *indepvarlist=NULL;
    apop_regex(get_key_word_tagged(configbase, "output vars", tag),
                " *([^,]*[^ ]) *(,|$) *", &vars); //split at the commas
    apop_regex(get_key_word_tagged(configbase, "input vars", tag),
                " *([^,]*[^ ]) *(,|$) *", &indepvarlist); //same
    Apop_stopif(!vars, return (impustruct){.error=1}, 0, "I couldn't find an 'output vars' line in the %s segment", configbase);

	impustruct model = (impustruct) {.position=-2, .vartypes=strdup("n")};
    model.depvar = strdup(**vars->text);

    //find the right model.
    char *model_name = get_key_word_tagged(configbase, "method", tag);
    model.base_model = tea_get_model_by_name(model_name, &model);
    Apop_stopif(!strlen(model.base_model.name), model.error=1; return model, 0, "model selection fail; you requested %s.", model_name);
  

/* In this section, we construct the select clause that will produce the data we need for estimation. apop_query_to_mixed_data also requires a list of type elements, so get that too.

Further, the text-to-factors function requires a spare column in the numeric part for each text element.

Hot deck: needs no transformations at all.
Raking: needs all the variables, in numeric format
*/

    char *indep_vars = get_key_word_tagged(configbase, "input vars", tag);
    char coltype = get_coltype(model.depvar); // \0==not found.
    
    int ict = indepvarlist ? *indepvarlist->textsize : 0;
    int dct = vars ? *vars->textsize : 0;
    model.allvars_ct = ict + dct;
    Apop_stopif(!model.allvars_ct, model.error=1; return model, 0, "Neither 'output vars' nor 'input vars' in the raking segment of the spec.");

    if (coltype=='r' || coltype=='i') 
          asprintf(&model.vartypes, "%sm", model.vartypes);
    else  asprintf(&model.vartypes, "%st", model.vartypes), model.textdep=true;

    if (model.is_rake){
        int i=0;
        model.allvars = malloc(sizeof(char*)*model.allvars_ct);
        if (vars) for (; i< *vars->textsize; i++) model.allvars[i]= strdup(*vars->text[i]);
        if (indepvarlist) for (int j=0; j< *indepvarlist->textsize; j++) model.allvars[i+j]= strdup(*indepvarlist->text[j]);
    }

    //if a text dependent var & a regression, set aside a column to be filled in with factors.
    if (model.is_regression)
         asprintf(&model.selectclause, "%s, %s%s", id_col, model.textdep ? "1, ": " ", model.depvar);	
    else asprintf(&model.selectclause, "%s, %s", id_col, model.depvar);	
    if (indep_vars){
        asprintf(&model.selectclause, "%s%c %s", 
                XN(model.selectclause),model.selectclause ? ',' : ' ',
                                    process_string(indep_vars, &(model.vartypes)));
        asprintf(&model.vartypes, "all numeric"); //lost patience implementing text data for OLS.
    }
	apop_data_free(vars);
    return model;
}

int impute_is_prepped; //restarts with new read_specs.
char *configbase = "impute";

/* \key {impute/input table} The table holding the base data, with missing values. 
  Optional; if missing, then I rely on the sytem having an active table already recorded. So if you've already called <tt>doInput()</tt> in R, for example, I can pick up that the output from that routine (which may be a view, not the table itself) is the input to this one. 
  \key{impute/seed} The RNG seed
  \key{impute/draw count} How many multiple imputations should we do? Default: 5.
  \key{impute/output table} Where the fill-ins will be written. You'll still need <tt>checkOutImpute</tt> to produce a completed table.
  \key{impute/earlier output table} If this imputaiton depends on a previous one, then give the fill-in table from the previous output here.
  \key{impute/margin table} Raking only: if you need to fit the model's margins to out-of-sample data, specify that data set here.
 */
int do_impute(char **tag, char **idatatab){ 
    //This fn does nothing but read the config file and do appropriate setup.
    //See impute_a_variable for the real work.
    Apop_stopif(!*tag, return -1, 0, "All the impute segments really should be tagged.")
    Apop_stopif(!*idatatab, return -1, 0, "I need an input table, "
                        "via a '%s/input table' key. Or, search the documentation "
                        "for the active table (which is currently not set).", configbase);
    Apop_stopif(!apop_table_exists(*idatatab), return -1, 0, "'%s/input table' is %s, but I can't "
                     "find that table in the db.", configbase, *idatatab);

    char *underlying = get_key_word_tagged(configbase, "underlying table", *tag);
    apop_data *category_matrix = get_key_text_tagged(configbase, "categories", *tag);

    float min_group_size = get_key_float_tagged(configbase, "min group size", *tag);
    if (isnan(min_group_size)) min_group_size = 1;

    float draw_count = get_key_float_tagged(configbase, "draw count", *tag);
    if (isnan(draw_count) || !draw_count) draw_count = 1;

    char *weight_col = get_key_word_tagged(configbase, "weights", *tag);
    char *out_tab = get_key_word_tagged(configbase, "output table", *tag);

    char *previous_fill_tab = get_key_word_tagged(configbase, "earlier output table", *tag);
    if (!out_tab) out_tab = "filled";

    char *id_col= get_key_word(NULL, "id");
    if (!id_col) {
        id_col=strdup("rowid");
        if (verbose) printf("I'm using the rowid as the unique identifier for the "
                    "index for the imputations. This is not ideal; you may want "
                    "to add an explicit Social Security number-type identifier.");
    }
    char *tmp_db_name_col = strdup(apop_opts.db_name_column);
    sprintf(apop_opts.db_name_column, "%s", id_col);

    static gsl_rng *r;
    if (!impute_is_prepped++) prep_imputations(configbase, id_col, &r, out_tab);
    //I depend on this column order in a few other places, like check_out_impute_base.
    if (!apop_table_exists(out_tab))
        apop_query("create table %s ('draw', 'value', '%s', 'field');"
                "create index %sind   on %s (%s);"
                "create index %sindx  on %s (field);"
                "create index %sindex on %s (draw);",
                    out_tab, id_col, out_tab, out_tab, id_col,
                    out_tab, out_tab, out_tab, out_tab);
    apop_data *fingerprint_vars = get_key_text("fingerprint", "key");

    impustruct model = read_model_info(configbase, *tag, id_col);
    Apop_stopif(model.error, return -1, 0, "Trouble reading in model info.");

    if (model.is_rake) {
        apop_data *catlist=NULL;
        if (category_matrix){
            char *cats= apop_text_paste(category_matrix, .between=", ");
            catlist = apop_query_to_text("select distinct %s from %s", cats, *idatatab);
            Apop_stopif(!catlist || catlist->error, return -1, 0, 
                    "Trouble querying for categories [select distinct %s from %s].", cats, *idatatab);
        }
        for (int i=0; i< (catlist ? *catlist->textsize: 1); i++){
            char *wherecat = NULL;
            if (catlist){
                char *and = " ";
                for (int j=0; j< catlist->textsize[1]; j++){
                    qxprintf(&wherecat, "%s %s %s='%s'", XN(wherecat), and, catlist->names->text[j],
                            catlist->text[i][j]);
                    and = " and ";
                }
            }
            char *margintab = get_key_word_tagged(configbase, "margin table", *tag);
            rake_to_completion(*idatatab, underlying, model, min_group_size, 
                        r, draw_count, wherecat, fingerprint_vars, id_col, weight_col, out_tab, margintab);
        }
    }
    else impute_a_variable(*idatatab, underlying, &model, min_group_size, 
                r, draw_count, category_matrix, fingerprint_vars, id_col, out_tab,
                previous_fill_tab);
    apop_data_free(fingerprint_vars);
    apop_data_free(category_matrix);
    sprintf(apop_opts.db_name_column, "%s", tmp_db_name_col);
    return 0;
}


void impute(char **idatatab){ 
    apop_data *tags = apop_query_to_text("%s", "select distinct tag from keys where key like 'impute/%'");
    for (int i=0; i< *tags->textsize; i++){
        char *out_tab = get_key_word_tagged(configbase, "output table", *tags->text[i]);
        if (!out_tab) out_tab = "filled";
        apop_table_exists(out_tab, 'd');
    }

    for (int i=0; i< *tags->textsize; i++)
        do_impute(tags->text[i], idatatab);
    apop_data_free(tags);
}

/* multiple_imputation_variance's default now.
static apop_data *colmeans(apop_data *in){
    apop_data *sums = apop_data_summarize(in);
    Apop_col_t(sums, "mean", means);
    apop_data *out = apop_matrix_to_data(apop_vector_to_matrix(means, 'r'));
    apop_name_stack(out->names, in->names, 'c', 'c');
    apop_data *cov = apop_data_add_page(out, apop_data_covariance(in), "<Covariance>");
    gsl_matrix_scale(cov->matrix, sqrt(in->matrix->size1));
    return out;
}*/

void get_means(){
    char *configbase = "impute_by_groups";
	char *idatatab = get_key_word(configbase, "datatab");
    if (!idatatab) idatatab = datatab;
    apop_data * main_data = apop_query_to_data("select * from %s", idatatab);
    //apop_data * fill_ins = apop_query_to_data("select * from all_imputes");
    //apop_data_show(apop_multiple_imputation_variance(colmeans, main_data, fill_ins));
    apop_data_free(main_data);
}
