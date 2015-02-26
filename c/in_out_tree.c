/* This file maintains a table with one line per segment of the input spec, listing
the segment's input and output tables. We can use this table to determine whether a
certain input is derived from a previous segment of the spec.

The table could thus be expressed as a set of linked lists, but that much structure seemed like overkill.

col  name
0    tag
1    type of segment (input, recode,...)
2    input table name
3    output table name
4    overwrite
*/

#include "internal.h"

apop_data *in_out_tab;

void in_out_tab_reset(){ apop_data_free(in_out_tab); }

void in_out_row_add(char const *tag){
    static char *last_out;
    apop_data *rows= apop_query_to_text("select key from keys "
                                        "where tag='%s'", tag);
    if (!rows) return;
    char *tagbase= **rows->text;
    for (char *i=tagbase; *i; i++) if (*i=='/') *i='\0';

    char *in = get_key_word_tagged(tagbase, "input table", tag);
    if (!in) in = strdup(last_out ? last_out: "");

    char *out = get_key_word_tagged(tagbase, "output table", tag);
    if (!out && strcmp(tagbase, "impute") && strcmp(tagbase, "input"))
        out = in ? strdup(in): NULL;

    int ts = in_out_tab ? *in_out_tab->textsize : 0;
    in_out_tab = apop_text_alloc(in_out_tab, ts+1, 5);

    apop_text_add(in_out_tab, ts, 0, tag);
    apop_text_add(in_out_tab, ts, 1, tagbase);
    apop_text_add(in_out_tab, ts, 2, in);
    if (out) apop_text_add(in_out_tab, ts, 3, out);

    char *overwrite = get_key_word_tagged(tagbase, "overwrite", tag);
    if ((overwrite && (overwrite[0]=='y' || overwrite[0]=='Y'))
        || (!overwrite && !strcmp(tagbase, "impute")) )
        apop_text_add(in_out_tab, ts, 4, "y");

    apop_data_free(rows); free(in); free(last_out);
    last_out = out;
} 

static bool is_recode_line(int i){ return 
    !strcmp(in_out_tab->text[i][1], "recodes") ||
    !strcmp(in_out_tab->text[i][1], "group recodes");
}

/* Recodes may depend on previous recodes. The last (only) must be named view<input>.
   The first intermediate will be mid<input> the next midmid<input> &c.
*/
void in_out_recode_fix(){
    in_out_tab->vector = gsl_vector_calloc(*in_out_tab->textsize);
    for (int i=0; i< *in_out_tab->textsize; i++){
        if (!is_recode_line(i)) continue;
        char *in = in_out_tab->text[i][2]; //alias
        bool has_successors = false;
        for (int j=i+1; j < *in_out_tab->textsize; j++){
            if (!is_recode_line(j) || strcmp(in_out_tab->text[j][2], in)) 
                continue;
            has_successors = true;
            apop_text_add(in_out_tab, j, 2, "mid%s", in);
            (*gsl_vector_ptr(in_out_tab->vector, j))++;
        }
        if (!has_successors)
            apop_text_add(in_out_tab, i, 3, "view%s",
                             in+3*(int)apop_data_get(in_out_tab, i));//skip the 'mid's.
        else
            apop_text_add(in_out_tab, i, 3, "mid%s", in);
    }
    gsl_vector_set_all(in_out_tab->vector, 0);
}

static int find_in_tab(char const *target, int col){
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (!strcmp(in_out_tab->text[i][col], target)) return i;
    return -1;
}

char *in_out_get(char const *tag, char in_or_out){
    int row = find_in_tab(tag, 0);
    return row < 0 ? NULL : in_out_tab->text[row][in_or_out=='i'? 2 : 3];
}

bool run_one_tag(int row, char **active_tab, void *aux_info){
    char *input_tab = in_out_tab->text[row][2];
    char *output_tab = in_out_tab->text[row][3];

    int prev = find_in_tab(input_tab, 3);
    bool OK = true;
    if (prev>=0 && prev < row) OK = run_one_tag(prev, active_tab, aux_info);
    Tea_stopif(!OK, return false,
            0, "Trouble building predecessor table for %s.\n", output_tab);

    if (in_out_tab->text[row][4][0]=='y') {
        apop_table_exists(output_tab, 'd');
        in_out_tab->text[row][4][0]='n'; //only overwrite once.
    } else 
        if (apop_table_exists(output_tab)) return true;

    *active_tab = output_tab;
    Tea_stopif(!strcmp(in_out_tab->text[row][1], "input"),
                return text_in_by_tag(*in_out_tab->text[row]),
                0, "Doing input for %s.", input_tab);
    Tea_stopif(!strcmp(in_out_tab->text[row][1], "impute"),
                return  do_impute(in_out_tab->text[row], active_tab, aux_info),
                0, "Doing imputations for %s.", input_tab);
    Tea_stopif(is_recode_line(row),
                return  make_recode_view(*in_out_tab->text[row]),
                0, "Doing recodes for %s.", input_tab);
    return true;
}

bool run_all_tags(char *type, char **active_tab, void* aux_info){
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (!strcmp(in_out_tab->text[i][1], type)
            ||(!strcmp("RRR", type) && is_recode_line(i)))
            Tea_stopif(!run_one_tag(i, active_tab, aux_info),
                return false, 0,
                "Trouble doing %s segment with tag '%s'.", type, *in_out_tab->text[i]);
}
