#include "tea.h"
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
}

static void ri_ext_init(){
    ri_ext=malloc(sizeof(apop_data*)*(total_var_ct));
    ri_ext_len=0;
    for (int v=0; v< total_var_ct; v++){
        char *q; 
        if (!apop_table_exists(used_vars[v].name)){
            v++; continue;
        }
        asprintf(&q, "select * from %s order by rowid", used_vars[v].name);
        ri_ext[ri_ext_len] = apop_query_to_text("%s", q);
        sprintf(ri_ext[ri_ext_len]->names->title, "%s", used_vars[v].name);
        free(q);
        ri_ext_len++;
    }
}

static apop_data *get_named_tab(char const *varname){
    if (!ri_ext) ri_ext_init();
    apop_data *this = NULL;
    for (int i=0; i< ri_ext_len; i++)
        if (apop_strcmp(varname, ri_ext[i]->names->title))
            {this=ri_ext[i]; break;}
    return this;
}

/* ri stands for rowid, ext for external, user-defined value. This translates.
   returned -100 means that the variable is undeclared.
   returned -1 means variable found, but value wasn't.
 */
int ri_from_ext(char const *varname, char const * ext_val){
    if (apop_strcmp(ext_val, "NULL")) ext_val = apop_opts.db_nan;
    apop_data *this = get_named_tab(varname);
    if (!this) return -100;
    for (int i=0; i< *this->textsize; i++)
        if (apop_strcmp(ext_val, *this->text[i])) return i+1;
    return -1;
}

char * ext_from_ri(char const *varname, int const ri_val){
    apop_data *this = get_named_tab(varname);
    return strdup(*this->text[ri_val-1]);
}
