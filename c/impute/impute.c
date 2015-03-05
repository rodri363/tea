//See Notes (the `walk-through of imputation' section) for a detailed discussion of what goes on here.

#include <rapophenia.h>
#include <stdbool.h>
#include "internal.h"
extern char *datatab;
void qxprintf(char **q, char *format, ...); //bridge.c
char *process_string(char *inquery, char **typestring); //parse_sql.c

apop_model *relmodel; //impute/rel.c

data_frame_from_apop_data_type *rapop_df_from_ad;//alloced in PEPedits.c:R_init_tea

static void index_cats(char const *tab, apop_data const *category_matrix){
    if (!category_matrix||!*category_matrix->textsize) return;
    apop_data *cats = apop_text_alloc(apop_data_transpose((apop_data*)category_matrix, .inplace='n'), 1, *category_matrix->textsize+1);
    cats->text[0][*category_matrix->textsize] = NULL;
    create_index_base(tab, (const char**)*cats->text);
    apop_data_free(cats);
}

static int lil_ols_draw(double *out, gsl_rng *r, apop_model *m){
    double temp_out[m->parameters->vector->size+1];
    m->draw = apop_ols->draw;
    Tea_stopif(apop_draw(temp_out, r, m), out[0]=NAN; return 1,
            0, "OLS draw failed.");
    m->draw = lil_ols_draw;
    *out = temp_out[0];
    return 0;
}

static char *construct_a_query(char const *datatab, char const *varlist, 
                                apop_data const *category_matrix, char const *id_col, char const* ego_id,
                                char *depvar, char const *subset){
/* Find out which constraints fit the given record, then join them into a query.
   The query ends in "and", because get_constrained_page will add one last condition.  */

//There were some ad hoc indices here, which had been commented out.
//If this function runs too slow, maybe check out revision a1a454d and try them out.

    char *q;
    Asprintf(&q, "select %s from %s where %s and ", varlist, datatab, subset);
    if (!category_matrix) return q;
    //else
    for (int i=0; i< category_matrix->textsize[0]; i++){
        char *n = *category_matrix->text[i]; //short name
        if (!strcmp(n, depvar)) continue; //we know the missing var is missing.
        apop_data *val = apop_query_to_text("select %s from %s where %s = %s",  
                                                    n,  datatab, id_col, ego_id);
        //char *endptr;
        //strtol(**val->text, &endptr, 10);
        //char sp_or_q = (*endptr == '\0') ? ' ': '"';
        char sp_or_q = '"'; //sqlite does just as well treating everything as a string.
        if (strcmp(**val->text, apop_opts.nan_string))
            qxprintf(&q, "%s (%s) = %c%s%c and\n", q, n, sp_or_q, **val->text, sp_or_q);
        else {
            free(q);
            apop_data_free(val);
            return "stop"; //a NULL category will have to be culled off, hopefully on the next run-through.
        }
        apop_data_free(val);
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
        Asprintf(&q2, "select %s from vflags where %s > 0", is->selectclause, is->depvar);
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

static void verify(impustruct is){
    /*Tea_stopif(is.isnan && !is.notnan, return, 0, "Even with no constraints, I couldn't find any "
                "non-NaN records to use for fitting a model and drawing values for %s.", is.depvar);*/
    if (is.notnan){
        int v= apop_opts.verbose; apop_opts.verbose=0;
        Tea_stopif(gsl_isnan(apop_matrix_sum((is.notnan)->matrix)+apop_sum((is.notnan)->vector)), 
                return, 0, 
                "NULL values or infinities where there shouldn't be when fitting the model for %s.", is.depvar);
        apop_opts.verbose=v;
    }
    Tea_stopif(!is.isnan, return, 2, "%s had no missing values. This sometimes happens when the fields used for sub-classes "
            "still has missing values. Perhaps do an imputation for these fields and add an 'earlier output table' line to "
            "this segment of the spec.", is.depvar);

    if (is.isnan && is.isnan->names->rowct){
        char *lastname=is.isnan->names->row[0];
        for (int i=1; i< is.isnan->names->rowct; i++){
            Tea_stopif(!strcmp(lastname, is.isnan->names->row[i]),
                   return, 0, "IDs should be unique, but found a duplicate id (%s). I stopped checking after this one.");
            lastname= is.isnan->names->row[i];
        }
    }
}

/* Generate two tables: those that have Nulls and therefore need to be
   considered; those that don't have nulls, and therefore can be used to fit
   models.

   The generation here is almost symmetric, but usage is asymmetric. The isnans are mostly used only for
   the rowids of those who need filling in, and are read as text for the consistency_check system; the 
	notnans are a data set for model estimation, so rowids are irrelevant. 

   The information for both is there should you want to use it.
*/
static void get_nans_and_notnans(impustruct *is, char const* index, char const *datatab, 
        int min_group_size, apop_data const *category_matrix, 
        apop_data const *fingerprint_vars, char const *id_col){
    is->isnan = NULL;
    char *q, *q2;
    /* There used to be verbiage here about saving the results of the query with no
       subcategories and checking whether what we had saved is still what we need.
       In the interest of reducing moving parts, it was removed after commit 750ef17f.*/
        q = construct_a_query(datatab, is->selectclause, category_matrix, id_col,
                                index, is->depvar,  is->subset?is->subset:"1");
    if (!strcmp(q, "stop")){
        apop_data_free(is->isnan);
        apop_data_free(is->notnan);
        return;
    }
    q2 = construct_a_query_II(id_col, is, fingerprint_vars);

    if (!strcmp(is->vartypes, "all numeric"))
        is->notnan = q2? apop_query_to_data("%s %s is not null except %s", q, is->depvar, q2) : apop_query_to_data("%s %s is not null", q, is->depvar);
    else is->notnan = q2? 
                 apop_query_to_mixed_data(is->vartypes, "%s %s is not null except %s", q, is->depvar, q2)
               : apop_query_to_mixed_data(is->vartypes, "%s %s is not null", q, is->depvar);
    apop_data_listwise_delete(is->notnan, .inplace='y');
    if (!strcmp(is->vartypes, "all numeric")){
         is->isnan = q2 ? 
              apop_query_to_data("%s %s is null union %s order by %s", q, is->depvar, q2, id_col)
            : apop_query_to_data("%s %s is null order by %s", q, is->depvar, id_col);
         //but we'll need the text for the consistency checking anyway.

         //Check whether isnan is null somewhere
         Tea_stopif(!is->isnan, return, 0, "query returned no data.");
         Tea_stopif(is->isnan->error, return, 0, "query failed.");
         apop_text_alloc(is->isnan, is->isnan->matrix->size1, is->isnan->matrix->size2);
         for (int i=0; i< is->isnan->matrix->size1; i++)
             for (int j=0; j< is->isnan->matrix->size2; j++)
                 apop_text_add(is->isnan, i, j, "%g", apop_data_get(is->isnan, i, j));
    } else is->isnan = q2 ? 
              apop_query_to_text("%s %s is null union %s order by %s", q, is->depvar, q2, id_col)
            : apop_query_to_text("%s %s is null order by %s", q, is->depvar, id_col);
    free(q); q=NULL;
    free(q2); q2=NULL;
    verify(*is);
}

//Primarily just call apop_estimate, but also make some minor tweaks.
static void model_est(impustruct *is, int *model_id){
    apop_data *notnan = is->notnan; //just an alias.
    assert(notnan);
    //maybe check here for constant columns in regression estimations.
    if (notnan->text && !apop_data_get_page(notnan,"<categories"))
        for (int i=0; i< notnan->textsize[1]; i++){
            if (!is->is_hotdeck && is->textdep && 
                   strcmp(notnan->names->col? notnan->names->col[i] : notnan->names->text[i], is->depvar) )
                apop_data_to_dummies(notnan, i, .keep_first='y', .append='y');
            //the actual depvar got factor-ized in prep_for_draw.
//            apop_data_to_dummies(is->isnan, i, .keep_first='y', .append='y'); //presumably has the same structure.
        }
    Tea_stopif(notnan->vector && isnan(apop_vector_sum(notnan->vector)), return, 
            0, "NaNs in the not-NaN vector that I was going to use to estimate "
            "the imputation model. This shouldn't happen");
    Tea_stopif(notnan->matrix && isnan(apop_matrix_sum(notnan->matrix)), return,
            0, "NaNs in the not-NaN matrix that I was going to use to estimate "
            "the imputation model. This shouldn't happen");
    if (is->is_hotdeck) apop_data_pmf_compress(notnan);
	//Apop_model_add_group(&(is->base_model), apop_parts_wanted); //no extras like cov or log like.
    install_data_to_R(notnan, is->base_model); //no-op if not an R model.

    //suppress a useless error msg
    if (is->is_hotdeck && !notnan->matrix && !notnan->vector) is->base_model->dsize=1;

	is->fitted_model = apop_estimate(notnan, is->base_model);
    Tea_stopif(!is->fitted_model, return, 0, "model fitting fail.");
    if (!strcmp(is->base_model->name, "multinomial"))
        apop_data_set(is->fitted_model->parameters, .row=0, .col=-1, .val=1);
    if (!strcmp(is->base_model->name, "Ordinary Least Squares"))
        is->fitted_model->draw=lil_ols_draw;
    if (verbose) apop_model_print(is->fitted_model, NULL);
    (*model_id)++;
    apop_query("insert into model_log values(%i, 'type', '%s');", *model_id, is->fitted_model->name);
    if (is->fitted_model->parameters && is->fitted_model->parameters->vector) //others are hot-deck or kde-type
        for (int i=0; i< is->fitted_model->parameters->vector->size; i++){
            char *param_name = NULL;
            if (is->fitted_model->parameters->names->rowct > i)
                param_name = is->fitted_model->parameters->names->row[i];
            else {
                free(param_name);
                Asprintf(&param_name, "param %i", i);
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

static void prep_for_draw(impustruct *is){
    apop_lm_settings *lms = apop_settings_get_group(is->fitted_model, apop_lm);
    if (is->textdep) apop_data_to_factors(is->notnan);
    if (lms){
        apop_data *p = apop_data_copy(is->notnan);
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

/* TeaKEY(checks, <<<This key is where the user specifies the parameters for the variables she declared in types. The parameters given here are checked during each round of imputation.>>>)
 */
int check_bounds(double *val, char const *var, char type){
    char *val_as_text;
    Asprintf(&val_as_text, "%g", *val);
    int rowid = ri_from_ext(var, val_as_text);
    free(val_as_text); 
    if (rowid >=0) return 0;
    //else, try to round it.
    if (type=='r' || type=='c')
        return 0;
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

static apop_data *get_all_nanvals(impustruct is, const char *id_col, const char *datatab){
    //db_name_column may be rowid, which would srsly screw us up.
    //first pass: get the full list of NaNs; no not-NaNs yet.
    apop_data *nanvals = apop_query_to_data("select distinct %s, %s from %s where %s is null "
                                            "and %s order by %s+0.0",
									 id_col, is.depvar, datatab, is.depvar, 
                                     is.subset?is.subset:"1", id_col);
    if (!nanvals) return NULL; //query worked, nothing found.
    Tea_stopif(nanvals->error, return NULL, 0, "Error querying for missing values.");
    if (verbose){
        printf("For %s, the following NULLs:\n", is.depvar);
        apop_data_show(nanvals); //may print NULL.
    }
    return nanvals;
}

static char *get_edit_associates(char const*depvar, int depvar_posn, char const*dt, char const*id_col,
                                    long int id_number, bool *has_edits){
    *has_edits = false;
    if (!edit_list) return NULL;
    used_var_t *this = used_vars+depvar_posn;

    if (!this->edit_associates){
        this->edit_associates = apop_text_alloc(NULL, 1, 1);
        apop_text_add(this->edit_associates, 0, 0, depvar);
        for (edit_t *this_ed=edit_list; this_ed && this_ed->clause; this_ed++){
            bool use_this_edit=false;
            for (int i=0; i< this_ed->var_ct; i++)
                if ((use_this_edit=!strcasecmp(depvar, this_ed->vars_used[i].name))) break;
            if (!use_this_edit) continue;

            *has_edits = true;
            apop_data *this_ed_list = apop_data_alloc();
            for(int i=0; i< this_ed->var_ct; i++){
                this_ed_list = apop_text_alloc(this_ed_list, *this_ed_list->textsize+1, 1);
                apop_text_add(this_ed_list, *this_ed_list->textsize-1, 0, this_ed->vars_used[i].name);
            }
            this->edit_associates= apop_data_stack(this->edit_associates, this_ed_list);
            apop_data_free(this_ed_list);
        }
        if (*this->edit_associates->textsize) apop_data_pmf_compress(this->edit_associates);
    } else //don't have to rebuild the edit_associates list, but do have to check has_edits
        for (edit_t *this_ed=edit_list; !*has_edits && this_ed && this_ed->clause; this_ed++)
            for(int i=0; !*has_edits && i< this_ed->var_ct; i++)
                if (strcasecmp(depvar, this_ed->vars_used[i].name)) 
                    *has_edits = true;

    if (!*this->edit_associates->textsize) return NULL;
    char *tail;
    Asprintf(&tail, " from %s where %s=%li", dt, id_col, id_number);
    char *out = apop_text_paste(this->edit_associates, .between=", ",  .before="select ", .after=tail);
    free(tail);
    return out;
}

//static int forsearch(const void *a, const void *b){return strcmp(a, *(char**)b);}
static int forsearch(const void *a, const void *b){
    //long int -> int conversion could break, so return just 1, -1, or 0.
    long int diff= atol((char*)a) - atol(*(char**)b);
    return diff > 0 ? 1 : (diff < 0 ? -1 : 0);
}

/* return 'n'=not found (bad news)   'm'=found, already marked  'u'=found, not marked. */
static char mark_an_id(const char *target, char * const *list, int len, char just_check){
    char **found = bsearch(target, list, len, sizeof(char*), forsearch);
    if (!found) return 'n';
    int out= (*found)[strlen(*found)-1]== '.' ? 'm' : 'u' ;
    if (just_check) return out;
    if (out=='u') Asprintf(found, "%s.", *found); //it's marked now.
    return 0;
}

/*This function is the inner loop cut out from impute(). As you can see from the list of
arguments, it really doesn't stand by itself.

The draw itself is one line---just call apop_draw. The hard part is in checking that the draw is OK.
This involves bounds-checking, if applicable, 
then generating a dummy version of the observation with ordered, external (user-defiend) values,
then sending it to consistency_check for an up-down vote.

The parent function, make_a_draw, then either writes the imputation to the db or tries this fn again.
*/
static int onedraw(gsl_rng *r, impustruct *is, long int id_number,
                    char **oext_values, int col_of_interest, bool has_edits){
    double x[is->fitted_model->dsize];
    apop_draw(x, r, is->fitted_model);
    Tea_stopif(isnan(*x), return -1, 0, "I drew NaN from the fitted model. Something is wrong.");
    apop_data *rd = get_data_from_R(is->fitted_model);
    if (rd) {
        *x = rd->vector ? *rd->vector->data : *rd->matrix->data;
        apop_data_free(rd);
    }
    if (is->depvar){ //everything but the EM model
        char type = used_vars[col_of_interest].type;
        if (type == '\0'){ // '\0' means not in the index of variables to check.
            Asprintf(oext_values+col_of_interest, "%g", *x);
            return 0;
        } else {
            if (!is->is_hotdeck && is->is_bounds_checkable) //inputs all valid ==> outputs all valid
                check_bounds(x, is->depvar, type); // just use the rounded value.
            apop_data *f;
            if (is->textdep && (f = apop_data_get_factor_names(is->fitted_model->data, .type='t')))
                 oext_values[col_of_interest] = strdup(*f->text[(int)*x]);
            else Asprintf(oext_values+col_of_interest, "%g", *x);
            if (!has_edits) return 0;
            
        }
    } else  //the EM model
        for (int i=0; i<is->fitted_model->dsize; i++)
            Asprintf(oext_values+(is->var_posns[i]), "%g", x[i]);

    //just get a success/failure, but a smarter system would request the list of failed fields.
    return cc2(oext_values, (char const*[]){"passfail"},
                                 &id_number, NULL, /*do_preedits=*/true, 0);
}

static void setit(char const *tabname, int draw, char const *final_value, char const *id_col,
                    char const *id, char const *field_name, bool autofill){
        char tick = final_value ? '\'': ' ';
        if (!autofill)
             apop_query("insert into %s values(%i, %c%s%c, '%s', '%s');",
                       tabname,  draw, tick, XN(final_value), tick, id, field_name);
        else apop_query("update %s set %s = %c%s%c where  %s='%s';",
                       tabname, field_name, tick, XN(final_value), tick, id_col, id);
}

bool strings_dont_match(char const *a, char const *b){ return (a && !b) || (!a && b) || strcmp(a, b); }

/*
There are many cases, which primarily boil down to whether we need to potentially record one field or all.

We would need to record all if there are edits, or for the EM algorithm.

If is->depvar is set, this is a signal that we're not using EM. The hope is to allow
for other models with multiple specified variables (depvars), but we currently don't
use any such models(!), so this remains on the to-implement list.

To potentially record all fields, we initialize oext_values, which is an ordered,
external-value(*) list of the record. For non-EM dealings, care is taken to record
only those fields that are relevant to the current record's edits.

A NULL in oext_values means the element doesn't need to be recorded later.



(*) Arbitrary user-defined values, versus the rowids that are used for the DISCRETE subsystem.

 */

//a shell for do onedraw() while (!done).
void make_a_draw(impustruct *is, gsl_rng *r, char const* id_col, char const *dt,
                        int draw, apop_data *nanvals, char const *filltab, bool last_chance){
    int col_of_interest = is->depvar ? is->var_posns[0]: -1;
    for (int rowindex=0; rowindex< is->isnan->names->rowct; rowindex++){
        char *name = is->isnan->names->row[rowindex];
        if (mark_an_id(name, nanvals->names->row, nanvals->names->rowct, 0)=='m')
            continue;
        int tryctr=0;
        long int id_number = atol(is->isnan->names->row[rowindex]);
        bool has_edits;

        char *oext_values[total_var_ct]; char *pre_preedit[total_var_ct];
        memset(oext_values, 0, total_var_ct * sizeof(char*));

        if (is->depvar){
            apop_data *drecord = NULL;
            char *associated_query = get_edit_associates(is->depvar, col_of_interest, dt, id_col, id_number, &has_edits);
            if (has_edits && associated_query) drecord = apop_query_to_text(associated_query);
            Tea_stopif(has_edits && associated_query && !*drecord->textsize, return, 0,
                        "Trouble querying for fields associated with %s", is->depvar);
            if (drecord && (has_edits || !is->depvar))
                order_things(*drecord->text, drecord->names->text, drecord->textsize[1], oext_values);
        } else 
            for (int j=0; j< is->isnan->matrix->size2; j++){
                double val = apop_data_get(is->isnan, rowindex, j);
                if (!isnan(val))
                    Asprintf(oext_values+is->var_posns[j], "%g", val)
            }

        for (int i=0; i< total_var_ct; i++)
            pre_preedit[i] = oext_values[i] ? strdup(oext_values[i]): NULL;

        int fail_count=0;
        do fail_count = onedraw(r, is, id_number, oext_values, col_of_interest, has_edits);
        while (fail_count && tryctr++ < 100);
        Tea_stopif(last_chance && fail_count, 
                apop_query("insert into tea_fails values(%li)", id_number)
                , 0, "I just made a hundred attempts to find an imputed value "
            "that passes checks, and couldn't. Something's wrong that a "
            "computer can't fix.\nI'm at id %li.", id_number);

        if (!fail_count){
            if (!has_edits && is->depvar){ //is->depvar is a semaphore for not the EM model.
                char * final_value = oext_values[col_of_interest];
                Tea_stopif(!final_value || isnan(atof(final_value)), return, 0,
                         "I drew a blank from the imputed column "
                         "when I shouldn't have for record %li.", id_number);
                setit(is->autofill?datatab:filltab, draw, final_value, id_col,
                        is->isnan->names->row[rowindex], is->depvar, is->autofill);
            }
            else for (int i=0; i< total_var_ct; i++){
               if (!oext_values[i] && !pre_preedit[i]) continue;
               if (col_of_interest==i || strings_dont_match(oext_values[i], pre_preedit[i]))
                    setit(is->autofill?datatab:filltab, draw, oext_values[i], id_col,
                        is->isnan->names->row[rowindex], used_vars[i].name, is->autofill);
            }
        }
        for (int i=0; i< total_var_ct; i++) free(pre_preedit[i]);
    }
}

double still_is_nan(apop_data *in){return in->names->row[0][strlen(*in->names->row)-1]!= '.';}


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
static void impute_a_variable(const char *datatab, impustruct *is, 
        const int min_group_size, gsl_rng *r, const int draw_count, apop_data *category_matrix, 
        const apop_data *fingerprint_vars, const char *id_col, char *filltab,
        char *previous_filltab){
    static int model_id=-1;
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
    create_index(datatab, is->depvar);
    create_index(datatab, id_col);

    for (int outerdraw=0; outerdraw < outermax; outerdraw++){
        if (previous_filltab){
            Asprintf(&dt, "%s_copy", datatab);
            check_out_impute(&dataxxx, &dt, &outerdraw, NULL, &previous_filltab);
            create_index(dt, is->depvar);
            create_index(dt, id_col);
        } else dt=strdup(datatab);
        begin_transaction();

        is->is_bounds_checkable = (ri_from_ext(is->depvar, "0") != -100); //-100=var not found.

        bool still_has_missings=true, hit_zero=false;
        do {
            for (int i=0; i < nanvals->names->rowct; i++){ //see notes in Notes file.
                Apop_row(nanvals, i, row_i);
                if (!still_is_nan(row_i)) continue;
                get_nans_and_notnans(is, nanvals->names->row[i] /*ego_id*/, 
                        dt, min_group_size, category_matrix, fingerprint_vars, id_col);
                if (!is->isnan) goto bail; //because that first guy should've been missing.
                if (!is->notnan || GSL_MAX((is->notnan)->textsize[0]
                            , (is->notnan)->matrix ? (is->notnan)->matrix->size1: 0) < min_group_size)
                    goto bail;
                is->is_hotdeck = (is->base_model->estimate == apop_multinomial->estimate 
                                        ||is->base_model->estimate ==apop_pmf->estimate);
                model_est(is, &model_id); //notnan may be pmf_compressed here.
                prep_for_draw(is);
                for (int innerdraw=0; innerdraw< innermax; innerdraw++)
                    make_a_draw(is, r, id_col, dt, GSL_MAX(outerdraw, innerdraw), nanvals, filltab,
                                    (!category_matrix||!*category_matrix->textsize));
                apop_model_free(is->fitted_model); //if (is_hotdeck) apop_data_free(is->fitted_model->data);
                bail:
                apop_data_free(is->notnan);
                apop_data_free(is->isnan);
            }
            still_has_missings = apop_map_sum(nanvals, .fn_r=still_is_nan);
            /*Shrink the category matrix by one, then loop back and try again if need be.
              This could be more efficient, but take recourse in knowing that the
              categories that need redoing are the ones with few elements in them.*/
            if (!still_has_missings) break;
            if (!category_matrix || *category_matrix->textsize==0)
                hit_zero++;
            else {
                if (*category_matrix->textsize > 1)
                    apop_text_alloc(category_matrix, category_matrix->textsize[0]-1, category_matrix->textsize[1]);
                else
                    *category_matrix->textsize=0; //Apophenia can't (yet) allocate a zero-sized matrix
                index_cats(dt, category_matrix);
            }
        } while (!hit_zero); //primary means of exit is "if (!still_has_missings) break;" above.
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

apop_model null_model = {.name="null model"};

/* TeaKEY(impute/method, <<<Specifies what model to use to impute output vars for a given impute key.>>>)
 */
apop_model *tea_get_model_by_name(char *name, impustruct *model){
    static get_am_from_registry_type *rapop_model_from_registry;
    static int is_inited=0;
    if (!is_inited && using_r)
        rapop_model_from_registry = (void*) R_GetCCallable("Rapophenia", "get_am_from_registry");

    apop_model *out= !strcasecmp(name, "normal")
          ||!strcasecmp(name, "gaussian")
				? apop_normal :
			!strcasecmp(name, "multivariate normal")
				? apop_multivariate_normal :
			!strcasecmp(name, "lognormal")
				? apop_lognormal :
			!strcasecmp(name, "rake") ||!strcasecmp(name, "em")
				?  (model->is_em = true, &null_model) :
			!strcasecmp(name, "hotdeck")
		  ||!strcasecmp(name, "hot deck")
	      ||!strcasecmp(name, "multinomial")
		  ||!strcasecmp(name, "pmf")
				? (model->is_hotdeck=true, apop_pmf) :
			!strcasecmp(name, "poisson")
				? (model->is_regression=true, apop_poisson) :
			!strcasecmp(name, "ols")
				? (model->is_regression=true, apop_ols) :
			!strcasecmp(name, "logit")
				? (model->is_regression=true, apop_logit) :
			!strcasecmp(name, "probit")
				? (model->is_regression=true, apop_probit) :
			!strcasecmp(name, "rel")
				? relmodel :
			!strcasecmp(name, "kernel")
	      ||!strcasecmp(name, "kernel density")
				? apop_kernel_density 
				: &null_model;
        if (using_r && !model->is_em && !strcasecmp(out->name, "Null model")) //probably an R model.
            out= rapop_model_from_registry(name);
        Tea_stopif(!strcmp(out->name, "Null model"), return &(apop_model){}, 0, "model selection fail.");
        Apop_model_add_group(out, apop_parts_wanted, .predicted='y'); //no cov
        return out;
}

void prep_imputations(char *configbase, char *id_col, gsl_rng **r){
    int seed = get_key_float(configbase, "seed");
    *r = apop_rng_alloc((!isnan(seed) && seed>=0) ? seed : 35);

    if (!apop_table_exists("model_log"))
        apop_query("create table model_log ('model_id', 'parameter', 'value')");
}

/* 
TeaKEY(impute/vars, <<<A comma-separated list of the variables to be put into the imputation model. 
For OLS-type models where there is a distinction between inputs and outputs, don't use this; use the "impute/input vars" and "impute/output vars" keys. Note that this is always the plural "vars", even if you are imputing only one field.>>>)
TeaKEY(impute/input vars, <<<A comma-separated list of the independent, right-hand side variables for imputation methods such as OLS that require them. These variables are taken as given and will not be imputed in this step, so you probably need to have a previous imputation step to ensure that they are complete.>>>)
TeaKEY(impute/output vars, <<<The variables that will be imputed. For OLS-type models, the left-hand, dependent variable (notice that we still use the plural "vars"). For models that have no distinction between inputs and outputs, this behaves identically to the "impute/vars" key (so only use one or the other).>>>)
TeaKEY(impute/subset, <<<If you would like to do imputation only on some subset of the data, specify a condition here. This will become an SQL where clause, such as "age>15 and status='married'".>>>)
 */
static impustruct read_model_info(char const *configbase, char const *tag, char const *id_col){
	apop_data *varlist=NULL, *indepvarlist=NULL, *outputvarlist=NULL;
    apop_regex(get_key_word_tagged(configbase, "vars", tag),
                " *([^,]*[^ ]) *(,|$) *", &varlist); //split at the commas
    apop_regex(get_key_word_tagged(configbase, "output vars", tag),
                " *([^,]*[^ ]) *(,|$) *", &outputvarlist);
    apop_regex(get_key_word_tagged(configbase, "input vars", tag),
                " *([^,]*[^ ]) *(,|$) *", &indepvarlist);
    Tea_stopif(!varlist && !outputvarlist, return (impustruct){.error=1}, 0, "I couldn't find a 'vars' or 'output vars' line in the %s segment", configbase);

	impustruct model = (impustruct) {.vartypes=strdup("n")};
    model.depvar = strdup(outputvarlist ? **outputvarlist->text : **varlist->text);
    model.allvars_ct = (indepvarlist ? *indepvarlist->textsize : 0)
                       + (varlist ? *varlist->textsize : 0)
                       + (outputvarlist ? *outputvarlist->textsize : 0);

    //find the right model.
    char *model_name = get_key_word_tagged(configbase, "method", tag);
    model.base_model = tea_get_model_by_name(model_name, &model);
    Tea_stopif(!strlen(model.base_model->name), model.error=1; return model, 0, "model selection fail; you requested %s.", model_name);

/* In this section, we construct the select clause that will produce the data we need for estimation. apop_query_to_mixed_data also requires a list of type elements, so get that too.

Further, the text-to-factors function requires a spare column in the numeric part for each text element.

Hot deck: needs no transformations at all.
Raking: needs all the variables, in numeric format
*/

    model.subset = get_key_word_tagged(configbase, "subset", tag);

    char coltype = get_coltype(model.depvar); // \0==not found.
    if (coltype=='r' || coltype=='i') 
        {Asprintf(&model.vartypes, "%sm", model.vartypes);}
    else  {Asprintf(&model.vartypes, "%st", model.vartypes); model.textdep=true;}

    if (model.is_em){
        int i=0;
        model.allvars = malloc(sizeof(char*)*model.allvars_ct);
        if (varlist) for (; i< *varlist->textsize; i++) model.allvars[i]= strdup(*varlist->text[i]);
        if (outputvarlist) for (; i< *outputvarlist->textsize; i++) model.allvars[i]= strdup(*outputvarlist->text[i]);
        if (indepvarlist) for (int j=0; j< *indepvarlist->textsize; j++) model.allvars[i+j]= strdup(*indepvarlist->text[j]);

        /*TeaKEY(impute/near misses, <<<If this is set to any value, then the EM algorithm (the
          only consumer of this option) will weight nearby cells when selecting cells to draw
          from for partial imputations. Else, it will use only cells that match the nonmissing data.>>>)*/
        if (get_key_word_tagged(configbase, "near misses", tag)) model.allow_near_misses = true;
    }

    //if a text dependent var & a regression, set aside a column to be filled in with factors.
    if (model.is_regression)
         Asprintf(&model.selectclause, "%s, %s%s", id_col, model.textdep ? "1, ": " ", model.depvar)
    else Asprintf(&model.selectclause, "%s, %s", id_col, model.depvar);	
    char *indep_vars = get_key_word_tagged(configbase, "input vars", tag); //as a comma-separated list, for SQL.
    if (indep_vars){
        Asprintf(&model.selectclause, "%s%c %s", 
                XN(model.selectclause),model.selectclause ? ',' : ' ',
                                    process_string(indep_vars, &(model.vartypes)));
        Asprintf(&model.vartypes, "all numeric"); //lost patience implementing text data for OLS.
    }
	apop_data_free(varlist); apop_data_free(outputvarlist); apop_data_free(indepvarlist);
    return model;
}

int impute_is_prepped; //restarts with new read_specs.
char *configbase = "impute";

/* TeaKEY(impute/input table, <<<The table holding the base data, with missing values. 
  Optional; if missing, then I rely on the sytem having an active table already recorded. So if you've already called {\tt doInput()} in R, for example, I can pick up that the output from that routine (which may be a view, not the table itself) is the input to this one.>>>)
  TeaKEY(impute/seed, <<<The RNG seed>>>)
  TeaKEY(impute/draw count, <<<How many multiple imputations should we do? Default: 1.>>>)
  TeaKEY(impute/output table, <<<Where the fill-ins will be written. You'll still need {\tt checkOutImpute} to produce a completed table. If you give me a value for {\tt impute/eariler output table}, that will be the default output table; if not, the default is named {\tt filled}.>>>)
  TeaKey(impute/autofill, <<<Write the imputations directly to the input table, rather than to an output table. Of course, this makes sense for only one imputation, so the draw count will be set to one. Including this key and setting it to any value except "no" will turn on this option.>>>)
  TeaKEY(impute/earlier output table, <<<If this imputation depends on a previous one, then give the fill-in table from the previous output here.>>>)
  TeaKEY(impute/margin table, <<<Raking only: if you need to fit the model's margins to out-of-sample data, specify that data set here.>>>)
 */
int do_impute(char **tag, char **idatatab, int *autofill){ 
    Tea_stopif(get_key_word("impute", "method") == NULL, return false, 0, "You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.");
    
     *idatatab = in_out_get(*tag, 'i');
    Tea_stopif(!*idatatab, return false-1, 0,
                        "I need an input table, via a '%s/input table' key. "
                        "Or, search the documentation "
                        "for the active table (which is currently not set).", configbase);
    Tea_stopif(!apop_table_exists(*idatatab), return false-1, 0, "'%s/input table' is %s, but I can't "
                     "find that table in the db.", configbase, *idatatab);

    char *af = get_key_word_tagged(configbase, "autofill", *tag);
    *autofill = *autofill || (af && !strcmp(af, "no"));

/* TeaKEY(impute/categories, <<<Denotes the categorized set of variables by which to impute your output vars.>>>)
 */
    apop_data *category_matrix = get_key_text_tagged(configbase, "categories", *tag);

/* TeaKEY(impute/min group size, <<<Specifies the minimum number of known inputs that must be present in order to perform an imputation on a set of data points.>>>)
 */
    float min_group_size = get_key_float_tagged(configbase, "min group size", *tag);
    if (isnan(min_group_size)) min_group_size = 1;

    float draw_count = *autofill ? 1 : get_key_float_tagged(configbase, "draw count", *tag);
    if (isnan(draw_count) || !draw_count) draw_count = 1;

    char *weight_col = get_key_word_tagged(configbase, "weights", *tag);
    char *out_tab = in_out_get(*tag, 'o');

    char *previous_fill_tab = get_key_word_tagged(configbase, "earlier output table", *tag);
    if (!out_tab || (!*out_tab && previous_fill_tab)) out_tab = previous_fill_tab;
    if (!out_tab || !*out_tab) out_tab = "filled";

    char *id_col= get_key_word(NULL, "id");
    if (!id_col) {
        id_col=strdup("rowid");
        if (verbose) printf("I'm using the rowid as the unique identifier for the "
                    "index for the imputations. This is not ideal; you may want "
                    "to add an explicit Social Security number-type identifier.");
    }
    char *tmp_db_name_col = strdup(apop_opts.db_name_column);
    sprintf(apop_opts.db_name_column, "%s", id_col);

    index_cats(*idatatab, category_matrix);

    static gsl_rng *r;
    if (!impute_is_prepped++) prep_imputations(configbase, id_col, &r);
    //I depend on this column order in a few other places, like check_out_impute_base.
    if (!*autofill && !apop_table_exists(out_tab))
        apop_query("create table %s ('draw', 'value', '%s', 'field');", out_tab,id_col);
    create_index(out_tab, id_col);
    create_index(out_tab, "field");
    create_index(out_tab, "draw");
    apop_data *fingerprint_vars = get_key_text("fingerprint", "key");

    impustruct model = read_model_info(configbase, *tag, id_col);
    model.var_posns = (int[]){get_ordered_posn(model.depvar), -1};
    model.autofill = *autofill;
    Tea_stopif(model.error, return false, 0, "Trouble reading in model info.");

    if (model.is_em) {
        apop_data *catlist=NULL;
        if (category_matrix){
            char *cats = apop_text_paste(category_matrix, .between=", ");
            catlist = apop_query_to_text("select distinct %s from %s", cats, *idatatab);
            Tea_stopif(!catlist || catlist->error, return false, 0, 
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
            em_to_completion(*idatatab, model, min_group_size, 
                        r, draw_count, wherecat, fingerprint_vars, id_col, 
                        weight_col, out_tab, margintab, previous_fill_tab);
        }
    }
    else impute_a_variable(*idatatab, &model, min_group_size, 
                r, draw_count, category_matrix, fingerprint_vars, id_col, out_tab,
                previous_fill_tab);
    apop_data_free(fingerprint_vars);
    apop_data_free(category_matrix);
    sprintf(apop_opts.db_name_column, "%s", tmp_db_name_col);
    return true;
}

/* TeaKEY(impute, <<<The key where the user defines all of the subkeys related to the doMImpute() part of the imputation process. For details on these subkeys, see their descriptions elsewhere in the appendix.>>>)
 */
void impute(char **idatatab, int *autofill){ 
    apop_table_exists("tea_fails", 'd');
    apop_query("create table tea_fails('id')");
    run_all_tags("impute", idatatab, autofill);
}
