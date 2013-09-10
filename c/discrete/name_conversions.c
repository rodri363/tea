#include "internal.h"

/** This file translates between ri=rowid and ext=external name. 

  ri is 1-indexed, not 0-indexed.

It does so by generating a data structure that is held only in this file,
which is a list of apop_data sets. Each data set's name is a variable; each table
has an ordered list of the possible values that variable can take.

Search is simple and naïve: it's all string-matching.
*/

apop_data **ri_ext;
size_t ri_ext_len;

void reset_ri_ext_table(){
    for (int i=0; i< ri_ext_len; i++) apop_data_free(ri_ext[i]);
    free(ri_ext);
    ri_ext=NULL;
    ri_ext_len=0;
}

static void ri_ext_init(){
    ri_ext=malloc(sizeof(apop_data*)*(total_var_ct));
    ri_ext_len=0;
    Apop_stopif(!used_vars, return, 0, "No used_vars list. Please add a \"fields\" section declaring the variables you want me to use.");
    for (int v=0; used_vars[v].name; v++){
        char *q; 
        if (!apop_table_exists(used_vars[v].name) ||
            used_vars[v].type=='r')
                 continue;
        asprintf(&q, "select * from %s order by rowid", used_vars[v].name);
        ri_ext[ri_ext_len] = used_vars[v].type=='i' 
                               ? apop_query_to_data("%s", q)
                               : apop_query_to_text("%s", q);
        sprintf(ri_ext[ri_ext_len]->names->title, "%s", used_vars[v].name);
        free(q);
        ri_ext_len++;
    }
}

static apop_data *get_named_tab(char const *varname){
    if (!ri_ext) ri_ext_init();
    apop_data *this = NULL;
    for (int i=0; i< ri_ext_len; i++)
        if (!strcasecmp(varname, ri_ext[i]->names->title))
            {this=ri_ext[i]; break;}
    return this;
}

/* ri stands for rowid, ext for external, user-defined value. This translates.
   returned -100 means that the variable is undeclared.
   returned -1 means variable found, but value wasn't.
 */
int ri_from_ext(char const *varname, char const * ext_val){
    Apop_stopif(!ext_val, return -1, 0, "You asked about an actual NULL.");
    if (!strcmp(ext_val, "NULL")) ext_val = apop_opts.db_nan;
    apop_data *this = get_named_tab(varname);
    if (!this) return -100;
    if (this->matrix && this->matrix->size1){
		for (int i=0; i< this->matrix->size1; i++){
					double x = apop_data_get(this, i);
					if ((isnan(x) && (!strcmp(ext_val, "nan") ||
							!strcmp(ext_val, apop_opts.db_nan)))
							|| atof(ext_val) == x)
						return i+1;
				}
    } else {
        for (int i=0; i< *this->textsize; i++)
            if (!strcmp(ext_val, *this->text[i])) return i+1;
    }
    return -1;
}

char * ext_from_ri(char const *varname, int const ri_val){
    apop_data *this = get_named_tab(varname);
    if (this->matrix && this->matrix->size1){
        Apop_stopif(ri_val > this->matrix->size1, return strdup("NULL"), 0,
            "You're asking for value %i of variable %s, but it only "
            "has %zu values.", ri_val, varname, this->matrix->size1);
        char *out;
        asprintf(&out, "%g", apop_data_get(this, ri_val-1));
        return out;
    } else {
        Apop_stopif(ri_val > *this->textsize, return strdup("NULL"), 0,
            "You're asking for value %i of variable %s, but it only "
            "has %zu values.", ri_val, varname, *this->textsize);
        return strdup(*this->text[ri_val-1]);
    }
}

double find_nearest_val(char const *varname, double ext_val){
    double this_dist, smallest_dist=INFINITY;
    if (isnan(ext_val)) return NAN;
    apop_data *this = get_named_tab(varname);
    if (!this) return NAN;
    size_t lim = *this->textsize ? *this->textsize : this->matrix->size1;
    double closest_val = NAN; //overwritten on first pass.
    for (int i=0; i< lim; i++){
         int val_i = *this->textsize ? atof(*this->text[i]) : apop_data_get(this, i);
         this_dist = fabs(val_i - ext_val);
         if (!this_dist) return ext_val;
         if (this_dist < smallest_dist){
             smallest_dist = this_dist;
             closest_val = val_i;
         }
    }
    return closest_val;
}
