//See readme file for notes.
#define __USE_POSIX //for strtok_r
#include "tea.h"
#include "discrete.h"
extern char *datatab;
void qxprintf(char **q, char *format, ...); //bridge.c
char *process_string(char *inquery, char **typestring); //parse_sql.c
void xprintf(char **q, char *format, ...); //parse_sql.c
char *strip (const char*); //peptalk.y
#define XN(in) ((in) ? (in) : "")

//globals!
int fail_id=0;
int model_id=-1;

/* The imputation system is arguably the core of PEP; correspondingly, it is the
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
	--We have an elaborate system that starts with all categorizations, and
		then, if the group of complete data points is too small, drop one 
		category at a time. This is mostly in find_active_cats().

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
	--Run check_consistency, and throw out if the draw doesn't pass
	--We want to multiply impute, which means just repeating the draw step as often as needed.
	--the output is a multi-impute table, with a row/col coordinate for what's
		being filled in, plus a column per imputation.
	--So far we've been talking about the straight draw, but there are other options
		These will generally make sense via the multinomial model
		--MLE
		--nearest neighbor, or probabilistic n.n., where the model is simply a distance function.

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
	char * depvar, *vartypes;
	int depvar_count, position;
	char * selectclause;
	apop_data *isnan;
} impustruct;
	
static void lil_ols_draw(double *out, gsl_rng *r, apop_model *m){
    double temp_out[m->parameters->vector->size+1];
    m->draw = apop_ols.draw;
    apop_draw(temp_out, r, m);
    m->draw = lil_ols_draw;
    *out = temp_out[0];
}


static void find_active_cats(const char *datatab, const int ego_id, int active_cats[], const apop_data *category_matrix, const char *id_col){
/*For each category:
  Go through each element in the category matrix; if it's in the category, get
  this observation's value, and test it against each constraint.

  On exit, you have a list of elements of the category_matrix that could be used for the query.
*/
    if (!category_matrix || category_matrix->textsize[0] == 0)
        return;
    apop_table_exists("internal_test", 'd');
    apop_query("create table internal_test as select * from %s where %s = %i", 
                                            datatab, id_col, ego_id);
    for (int j=0; j < category_matrix->textsize[0]; j++){
        active_cats[j] = 0;
        if (!strlen(category_matrix->text[j][0]))
            continue; //this was blanked out by process_category_matrix
        apop_data *d = apop_query_to_data("select * from internal_test where %s",
                                        category_matrix->text[j][0]);
        if (d != NULL){
            apop_data_free(d);
            active_cats[j] = 1;
        }
    }
    apop_table_exists("internal_test", 'd');
}

static char *construct_a_query(const char *datatab, const char *underlying, int categories_left, const int active_cats[], const char *varlist, const apop_data *category_matrix, const char *id_col){
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
    asprintf(&q, "select %s, %s from %s where ", id_col, varlist, datatab);
    if (!category_matrix)
        return q;
    //else
    for (int i=0; categories_left && i< category_matrix->textsize[0]; i++)
        if (active_cats[i]){
            qxprintf(&q, "%s %s and\n", q, category_matrix->text[i][0]);
            categories_left--;
        }
    return q;
}

static char *construct_a_query_II(const char *id_col, const impustruct *is, const apop_data *fingerprint_vars){
    /* Do we need the clause that includes the requisite items from the vflags table? */
    if (!fingerprint_vars || !apop_table_exists("vflags")) return NULL;
    char *q2 = NULL;
    int is_fprint=0; 
    for (int i=0; !is_fprint && i< fingerprint_vars->textsize[0]; i++)
        if (!strcasecmp(fingerprint_vars->text[i][0], is->depvar))
            is_fprint++;
    if (is_fprint)
        asprintf(&q2, "select  %s, %s from vflags where %s > 0", id_col, is->selectclause, is->depvar);
    return q2;
}

apop_data *nans_no_constraints, *notnans_no_constraints;
char *last_no_constraint;

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
static void get_nans_and_notnans(impustruct *is, apop_data **notnan,
        int index, const char *datatab, const char *underlying, int min_group_size,
		const apop_data *category_matrix, const apop_data *fingerprint_vars, const char *id_col){
	is->isnan = *notnan = NULL;
    int active_cats[category_matrix ? category_matrix->textsize[0] : 1];
    int v = apop_opts.verbose; apop_opts.verbose=1;
    find_active_cats(datatab, index, active_cats, category_matrix, id_col);
    apop_opts.verbose=v;
    char *q, *q2;
    int done, constraints_left = 1, already_ran=0;
    for(int i=0; i< (category_matrix ? category_matrix->textsize[0] : 0); i++)
        constraints_left += active_cats[i]+1;
    do {
        if (constraints_left-- == 0){
			if (nans_no_constraints){//maybe recycle what we have.
			    if(apop_strcmp(last_no_constraint, is->selectclause)){
					is->isnan = nans_no_constraints;
					*notnan = notnans_no_constraints;
					return;
				} else { //this isn't the right thing; start over
					apop_data_free(nans_no_constraints);
					apop_data_free(notnans_no_constraints);
					is->isnan = *notnan = NULL;
					last_no_constraint = strdup(is->selectclause);
				}
			}//else, carry on:
            asprintf(&q, "select %s, %s from %s where 1 and ", id_col, is->selectclause, datatab);
        } else
            q = construct_a_query(datatab, underlying, constraints_left, active_cats,
												is->selectclause, category_matrix, id_col);
        q2 = construct_a_query_II(id_col, is, fingerprint_vars);
        //(varposn) This used to be apop_query_to_mixed_data(var_coltypes, ...) but for now it's all in the matrix.
        if (notnan){//NULL indicates skipping this step
            *notnan = q2? apop_query_to_mixed_data(is->vartypes, "%s %s is not null except %s", q, is->depvar, q2)
                        : apop_query_to_mixed_data(is->vartypes, "%s %s is not null", q, is->depvar);
            apop_data_listwise_delete(*notnan, .inplace='y');
        }
apop_opts.verbose=2;
        if (!already_ran++) //we don't winnow down the isnan query.
            is->isnan = q2 && is->depvar_count == 1 ? 
                  apop_query_to_text("%s %s is null union %s", q, is->depvar, q2)
                : apop_query_to_text("%s %s is null", q, is->depvar);
        done = 0;
        if (constraints_left == -1){
            done++;
			nans_no_constraints= is->isnan ;
			notnans_no_constraints= *notnan ;
			last_no_constraint = strdup(is->selectclause);
		}
        if (*notnan && GSL_MAX((*notnan)->textsize[0], (*notnan)->matrix ? (*notnan)->matrix->size1: 0) > min_group_size)
            done++;
        if (verbose && !done){
            if (*notnan) printf("I just have %i items in the notnan list. Trying w/smaller category list\n"
                                ,(*notnan)->textsize[0]);
            else printf("I have a big fat zero items in the notnan list. Trying w/smaller category list\n");
        }
		free(q); q=NULL;
		free(q2); q2=NULL;
    } while (!done);
    if (notnan){
        Apop_assert(*notnan, "Even with no constraints, I couldn't find any "
					"non-NaN records to use for fitting a model and drawing values for %s.", is->depvar);
        int v= apop_opts.verbose=0;apop_opts.verbose=0;
        Apop_assert(!(gsl_isnan(apop_matrix_sum((*notnan)->matrix)+apop_sum((*notnan)->vector))), 
                "NULL values or infinities where there shouldn't be when fitting the model for %s.", is->depvar);
        apop_opts.verbose=v;
    }
	apop_opts.verbose=v;
    Apop_assert_c(is->isnan, , 1, "%s had no missing values.", is->depvar);
printf("This set had %i nans.\n", is->isnan->textsize[0]);
}

//Primarily just call apop_estimate, but also make some minor tweaks.
static void model_est(apop_data *notnan, impustruct *is){
    //maybe check here for constant columns in regression estimations.
    if(notnan->text && !apop_data_get_page(notnan,"<categories")){
        for(int i=0; i< notnan->textsize[1]; i++)
            apop_data_to_dummies(notnan, i, .append='y');
    }  
    assert(notnan);
    Apop_assert (!notnan->vector || !isnan(apop_vector_sum(notnan->vector)), "NaNs in the not-NaN vector that I was going to use to estimate the imputation model. This shouldn't happen")
    Apop_assert (!notnan->matrix || !isnan(apop_matrix_sum(notnan->matrix)), "NaNs in the not-NaN matrix that I was going to use to estimate the imputation model. This shouldn't happen")
    if (apop_strcmp(is->base_model.name, "Multinomial distribution"))
        apop_data_pmf_compress(notnan);
	//Apop_model_add_group(&(is->base_model), apop_parts_wanted); //no extras like cov or log like.
	is->fitted_model = apop_estimate(notnan, is->base_model);
    if (apop_strcmp(is->base_model.name, "Multinomial distribution"))
        apop_data_set(is->fitted_model->parameters, .row=0, .col=-1, .val=1);
    if (apop_strcmp(is->base_model.name, "Ordinary Least Squares"))
        is->fitted_model->draw=lil_ols_draw;
    if (verbose)
        apop_model_print(is->fitted_model);
    model_id++;
    apop_query("insert into model_log values(%i, 'type', '%s');", model_id, is->fitted_model->name);
    if (is->fitted_model->parameters->vector) //others are hot-deck or kde-type
        for (int i=0; i< is->fitted_model->parameters->vector->size; i++)
            apop_query("insert into model_log values(%i, '%s', %g);",
                    model_id, is->fitted_model->parameters->names->row[i],
                              is->fitted_model->parameters->vector->data[i]);
    apop_query("insert into model_log values(%i, 'subuniverse size', %i);", model_id, (int)is->fitted_model->data->matrix->size1);
}

static void prep_for_draw(apop_data *notnan, impustruct *is){
    apop_lm_settings *lms = apop_settings_get_group(is->fitted_model, apop_lm);
    if (lms){
        apop_data *p = apop_data_copy(notnan);
        p->vector=NULL;
        lms->input_distribution = apop_estimate(p, apop_pmf);
    }
}


/*Here, we check that the variable is within declared bounds. We have to do this
  because the edit system in check_consistency will assume that all variables are 
  within their declared range. By doing the checking here, we may even save some trips
  to the consistency checker.

Return: 1=input wasn't in the list, was modified to nearest value
        0=input was on the list, wasn't modified
  If function returns a 1, you get to decide whether to use the rounded value or re-draw.

  If text or real, there's no rounding to be done: it's valid or it ain't. If numeric, then we can do rounding as above.
  I only check for 't' for text; suggest 'n' for numeric but it's up to you.
*/
int check_bounds(double *val, char *var, char type){
//Hey, B: Check the SQL that is about only this variable. This will need to be modified for multi-variable models.
//After checking the SQL, check the declarations.
    if (type=='r' || type=='c') return 0;
/*    if (type =='c')
		return !!apop_query_to_float("select count(*) from %s where %s = '%s'",
var, var, val);*/
    else {//integer type
        apop_table_exists("tea_bound_check", 'd');
apop_opts.verbose--;
        apop_data *closest= apop_query_to_text ("create view tea_bound_check as "
                                      " select %s as value, abs(%s - %g) as dist from %s; "
                                      " select value, dist from "
                                      "  tea_bound_check t, "
                                      "  (select min(dist) as d from tea_bound_check) m "
                                      " where t.dist = m.d; "
                                      "drop view tea_bound_check;"
                                      , var, var, *val, var);
        apop_assert(closest, "Checking a variable that wasn't declared. This shouldn't happen.");
        if (verbose>1) apop_data_show(closest);
        if (fabs(atof(closest->text[0][1])) > 0){//may overdetect for some floats
if (verbose) printf("rounding %g -> %s\n", *val , closest->text[0][0]);
            *val = atof(closest->text[0][0]);
            apop_data_free(closest);
            return 1;
        }
        apop_data_free(closest);
    }
    return 0;
}

void R_check_bounds(double *val, char **var, int *fails){
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

      If we fill a record and find that it doesn't work, randomly select one variabel to re-impute, 
      then try again. Max likelihood methods that would use the alternatives table that 
      consistency_check can produe are reserved for future work.
      */

//we're guaranteed to find it, so this will run fast---and we're gonzo and don't do checks
void mark_an_id(int *ctr, char *target, char **list, int len){
    while (*ctr < len && !apop_strcmp(target, list[*ctr])) (*ctr)++;
    if (*ctr == len){
        *ctr = 0;
        Apop_assert_c(0,  , 0, "%s isn't in the main list of nans.", target);
    }
    sprintf(list[*ctr], ".");
}

/* As named, impute from a single model, almost certainly a single variable.
   We check bounds, because those can be done on a single-variable basis.
   But overall consistency gets done on the whole-record basis. */
static void impute_a_variable(const char *datatab, const char *underlying, impustruct *is, const int min_group_size, gsl_rng *r, 
            const int draw_count, const apop_data *category_matrix, const apop_data *fingerprint_vars, 
            const char *id_col){
    static apop_data *blank_category_matrix= NULL;
    if (!blank_category_matrix) blank_category_matrix = apop_data_alloc();
    apop_data *nanvals=NULL, *notnan=NULL;
	int ctr=0, catctr=0;

	char *nametmp = strdup(apop_opts.db_name_column);
    sprintf(apop_opts.db_name_column, "%s", id_col);
    //first pass: get the full list of NaNs; no not-NaNs yet.
    nanvals = apop_query_to_data("select %s, %s from %s where %s is null",
									 id_col, is->depvar, datatab, is->depvar);
    sprintf(apop_opts.db_name_column, "%s", nametmp); free(nametmp);

printf("For %s, the following NULLs:\n", is->depvar);
apop_data_show(nanvals);
    if (!nanvals) return;

	char const *whattodo="passfail";
    apop_query("begin;");

    //The main i-indexed loop is really just a search for representatives
    //of every subgroup. The k-indexed loop is where the filled table is actually filled.
    for (int i=0; i < nanvals->names->rowct; i++){
        int is_bounded=0, done_ctr=i;
        //is this guy already OK?
        if (apop_strcmp(nanvals->names->row[i], "."))
            continue; //already got this person.
        /*Get those in the category matching this guy, split by those having the
            relevant variable and those with NaNs at the var.
            Set the imputation model based on those w/o NaNs.
            Loop over those with NaNs to write imputations to table of filled values */
        int ego_id =  atoi(nanvals->names->row[i]);
		catctr++;
        get_nans_and_notnans(is, &notnan, ego_id, datatab, underlying, min_group_size, 
                                                   category_matrix, fingerprint_vars, id_col);
		if (!is->isnan) {/* apop_data_free(notnan)*/; continue;} //nothing missing.
		int col_of_interest=apop_name_find(is->isnan->names, is->depvar, 't');
        char type = 'i'; //default to integer
        for (int v=0; v<total_var_ct && !is_bounded; v++)
            if (!strcasecmp(is->depvar, used_vars[v].name)){
                is_bounded =1;
                type= used_vars[v].type;
            }
		int is_hotdeck = (is->base_model.estimate ==apop_multinomial.estimate 
								||is->base_model.estimate ==apop_pmf.estimate);
    assert(notnan->matrix || notnan->vector || *notnan->textsize);
		if (is_hotdeck) {
			apop_data *compressed = apop_data_pmf_compress(notnan);
//            apop_data_free(notnan);
            notnan=compressed;
        }
        model_est(notnan, is);
        if (is->fitted_model->parameters->vector)
            assert(!isnan(apop_sum(is->fitted_model->parameters->vector)));
        for (int rowindex=0; rowindex< is->isnan->names->rowct; rowindex++){//more==rowids.
            prep_for_draw(notnan, is);
            fail_id++;
			double pre_round,x;
			int id_number = atoi(is->isnan->names->row[rowindex]);
			Apop_data_row(is->isnan, rowindex, full_record);
            for (int draw=0; draw< draw_count; draw++){
				int is_fail=0, tryctr=0;
                char *textx = NULL;
				do {
                    if (textx) free(textx);
					apop_draw(&x, r, is->fitted_model);
					pre_round=x;
                    if (!is_bounded){
                        is_fail = 0;
                        asprintf(&textx, "%g", x);
                    } else {
						if (!is_hotdeck) //inputs all valid ==> outputs all valid
                            check_bounds(&x, is->depvar, type); // We're just going to use the rounded value.
                        apop_data *f;
                        char *cats; asprintf(&cats, "<categories for %s>", is->depvar);
                        if (f = apop_data_get_page(is->fitted_model->data, cats))
                             asprintf(&textx, "%s", *f->text[(int)x]);
                        else asprintf(&textx, "%g", x);
                        free (cats);
                        apop_query("insert into impute_log values(%i, %i, %i, %g, '%s', 'cc')",
                                                id_number, fail_id, model_id, pre_round, textx);
                        //copy the new impute to full_record, for re-testing
                        apop_text_add(full_record, 0, col_of_interest, "%s", textx);
                        consistency_check((char const * restrict *)full_record->names->text,
                                          (char const * restrict *)full_record->text[0],
                                          full_record->textsize+1,
                                          &whattodo,
                                          &id_number,
                                          &is_fail,
                                          NULL);//record_fails);
                    }
				} while (is_fail && tryctr++ < 1000);
				Apop_assert(!is_fail, "I just made a thousand attempts to find an "
					"imputed value that passes checks, and couldn't. "
					"Something's wrong that a computer can't fix.\n "
					"I'm at id %i.", id_number);
                apop_query("insert into filled values( %s, '%s', %i, '%s');\n"
                           "insert into impute_log values(%s, %i, %i, %g, '%s', '? ?')",
                                           is->isnan->names->row[rowindex], is->depvar, draw, textx,
                                           is->isnan->names->row[rowindex], fail_id, model_id, pre_round, textx);
                free(textx);
			}
            mark_an_id(&done_ctr,is->isnan->names->row[rowindex], nanvals->names->row, nanvals->names->rowct);
			if (!(ctr++ % 1000)) {printf("cat %i, %i elmts\t%g%% done\r",catctr, is->isnan->names->rowct, ((1000*ctr)/nanvals->names->rowct)/10.0);fflush(NULL);}
        }
		//if (is_hotdeck) apop_data_free(is->fitted_model->data);
        apop_model_free(is->fitted_model);
        //apop_data_free(notnan);
        apop_data_free(is->isnan);
    }
    //gsl_vector_free(ids_to_fill);
    apop_query("commit;");
    apop_data_free(nanvals);
}

void process_category_matrix(apop_data *category_matrix, char *idatatab){
    if (!category_matrix) return;
    int initial_size = category_matrix->textsize[0];
    for (int i=0; i< initial_size; i++){
        char const *cat = category_matrix->text[i][0];
        if (!apop_regex(cat, "[<>!|=&]")){
            apop_data *queried;
            if (apop_table_exists(strip(cat))) //If we have the declaration, then use it
                queried = apop_query_to_text("select * from %s", cat);
            else
                queried = apop_query_to_text("select distinct(%s) from %s", cat, idatatab);
            //= apop_query_to_text("select distinct('%s='||%s) from %s", cat, cat, idatatab);//far too slow
            for (int j=0; j< queried->textsize[0]; j++)
                xprintf(&queried->text[j][0], "%s='%s'", cat, queried->text[j][0]);
            apop_data_stack(category_matrix, queried, .inplace='y');
            apop_data_free(queried);
            apop_text_add(category_matrix, i, 0, "0 = 1");//always-false filler
        }
    }
}

apop_model tea_get_model_by_name(char *name){
return		apop_strcmp(name, "normal")
          ||apop_strcmp(name, "gaussian")
				? apop_normal :
			apop_strcmp(name, "multivariate normal")
				? apop_multivariate_normal :
			apop_strcmp(name, "lognormal")
				? apop_lognormal :
			apop_strcmp(name, "hotdeck")
		  ||apop_strcmp(name, "hot deck")
	      ||apop_strcmp(name, "multinomial")
				? apop_multinomial :
			apop_strcmp(name, "poisson")
				? apop_poisson :
			apop_strcmp(name, "ols")
				? apop_ols :
			apop_strcmp(name, "logit")
				? apop_logit :
			apop_strcmp(name, "probit")
				? apop_probit :
			apop_strcmp(name, "kernel")
	      ||apop_strcmp(name, "kerneldensity")
	      ||apop_strcmp(name, "kernel density")
				? apop_kernel_density 
				: (apop_model) {.name="Null model"};
}

void prep_imputations(char *configbase, char *id_col, gsl_rng **r){
    int seed = get_key_float(configbase, "seed");
    *r = apop_rng_alloc((!isnan(seed) && seed>=0) ? seed : 35);
    apop_table_exists("filled", 'd');
    apop_query("create table filled ('%s', 'field', 'draw', 'value');"
                "create index filledind   on filled (%s);"
                "create index filledindx  on filled (field);"
                "create index filledindex on filled (draw);"
                    , id_col, id_col);
    apop_table_exists("impute_log", 'd');
    apop_query("create table impute_log (%s, 'fail_id', 'model', 'draw', 'declared', 'status')", id_col);
    if (!apop_table_exists("model_log"))
        apop_query("create table model_log ('model_id', 'parameter', 'value')");
}

int impute_is_prepped = 0; //restarts with new read_specs.

/* \key {impute/input table} The table holding the base data, with missing values. 
  Optional; if missing, then I rely on the sytem having an active table already recorded. So if you've already called <tt>doInput()</tt> in R, for example, I can pick up that the output from that routine (which may be a view, not the table itself) is the input to this one. 
  \key{impute/seed} The RNG seed
  \key{impute/draw count} How many multiple imputations should we do? Default: 5.
 */
void impute(char **tag, char **idatatab){ 
    char *configbase = "impute";
    Apop_assert(*idatatab, "I need an input table, "
                        "via a '%s/input table' key.", configbase);
    Apop_assert(apop_table_exists(*idatatab), "'%s/input table' is %s, but I can't "
                     "find that table in the db.", configbase, *idatatab);
    char *underlying = get_key_word(configbase, "underlying table");
    apop_data *category_matrix = get_key_text_tagged(configbase, "categories", tag?*tag:NULL);
    process_category_matrix(category_matrix, *idatatab);
    float min_group_size = get_key_float(configbase, "min group size");
    if (isnan(min_group_size)) min_group_size = 1;
    float draw_count = get_key_float(configbase, "draw count");
    if (isnan(draw_count) || !draw_count) draw_count = 1;
    //int verbose = get_key_float(configbase, "verbose", .default_val = 0);
    char *id_col = get_key_word(configbase, "id");
    if (!id_col) id_col= get_key_word(NULL, "id");
    if (!id_col) {
        id_col=strdup("rowid");
        if (verbose) printf("I'm using the rowid as the unique identifier for the "
                    "index for the imputations. This is not ideal; you may want "
                    "to add an explicit Social Security number-type identifier.");
    }
    char *tmp_db_name_col = strdup(apop_opts.db_name_column);
    sprintf(apop_opts.db_name_column, "%s", id_col);

    char *tagbit; if (tag) asprintf(&tagbit,"and tag='%s'", *tag);
	apop_data * vars = apop_query_to_text("select distinct key, value from keys where "
								" key like '%s/models/%%/method' %s order by count", configbase, (tag && *tag) ?tagbit:" ");
    if(tag && *tag) free(tagbit);
    Apop_assert(vars, "I couldn't find a models section (or a method section with a key of the form '%s/models/yr_variable/method').", configbase);
	int vars_to_impute = vars->textsize[0];
	Apop_assert(vars_to_impute, "I couldn't find a models section (or its contents).");
	impustruct models[vars_to_impute+1];
	for (int i=0; i < vars_to_impute; i++){
        //I don't use vartypes; it's there if I need it. Search for (varposn).
        models[i] = (impustruct) {.position=-2, .vartypes=strdup("nm")}; //zero out everything; posn to be filled in later.
		models[i].depvar = strdup(vars->text[i][0]);
		models[i].depvar = strchr(models[i].depvar, '/')+1; //skip two slashes
		models[i].depvar = strchr(models[i].depvar, '/')+1;
		models[i].depvar_count = 1; //TO DO: implement more than one.
		*(strchr(models[i].depvar, '/')) = '\0';  //set last slash to \0.
		models[i].base_model = tea_get_model_by_name(vars->text[i][1]);
		Apop_assert(!apop_strcmp(models[i].base_model.name, "Null model"), 
							"I couldn't find the model named %s in "
							"my list of supported models.", vars->text[i][1]);
		char *varkey;
		asprintf(&varkey, "models/%s/vars", models[i].depvar);
		char *d = get_key_word(configbase, varkey);
            for (int j=0; j < total_var_ct; j++)
                if (apop_strcmp(used_vars[j].name, models[i].depvar)){
                        if (used_vars[j].type=='c')
                            asprintf(&models[i].vartypes, "nt");
                        break;
                }
		if (!d) asprintf(&models[i].selectclause, "%s", models[i].depvar);	
        else    asprintf(&models[i].selectclause, "%s, %s", models[i].depvar, 
										process_string(d, &(models[i].vartypes)));
	}
    //The models list has one for each item in the spec. Position is in em order.
    models[vars_to_impute] = (impustruct) {.position=-2}; //NULL sentinel
	apop_data_free(vars);

    gsl_rng *r;
    if (!impute_is_prepped++) prep_imputations(configbase, id_col, &r);
    apop_data *fingerprint_vars = get_key_text("fingerprint", "key");
    for (int i = 0; i< vars_to_impute; i++)
        impute_a_variable(*idatatab, underlying, models+i, min_group_size, r, draw_count, 
                          category_matrix, fingerprint_vars, id_col);
    apop_data_free(fingerprint_vars);
    sprintf(apop_opts.db_name_column, "%s", tmp_db_name_col);
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


/*
The alternative would work thusly:

checkout an imputation; make sure rowids mesh.

run consistency_check on every row
if a change, update filled set value = %s where field = '%s' and rowid = %i

Um, then you're done?

 */
