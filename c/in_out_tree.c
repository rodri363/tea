#include "internal.h"

/* This file maintains a table with one line per segment of the input spec, listing
the segment's input and output tables. We can use this table to determine whether a
certain input is derived from a previous segment of the spec.

The table could thus be expressed as a set of linked lists, but that much structure seemed like overkill.

col  name
0    tag
1    type of segment (input, recode,...)
2    input table name
3    output table name
*/

apop_data *in_out_tab;
char *last_out;

void in_out_tab_reset(){ apop_data_free(in_out_tab); }

void in_out_row_add(char *tag){
    apop_data *rows= apop_query_to_text("select key from keys "
                                        "where tag='%s'", tag);
    if (!rows) return;
    char *tagbase= **rows->text;
    for (char *i=tagbase; *i; i++) if (*i=='/') *i='\0';

    char *in = get_key_word_tagged(tagbase, "input table", tag);
    if (!in) in = last_out ? strdup(last_out): NULL;

    char *out = get_key_word_tagged(tagbase, "output table", tag);
    if (!out && strcmp(tagbase, "impute")) out = in ? strdup(in): NULL;

    if(!in_out_tab) in_out_tab=apop_text_alloc(NULL, 1, 4);
    else apop_text_alloc(in_out_tab, *in_out_tab->textsize+1, 4);
    apop_text_add(in_out_tab, *in_out_tab->textsize-1, 0, tag);
    apop_text_add(in_out_tab, *in_out_tab->textsize-1, 1, tagbase);
    if (in)  apop_text_add(in_out_tab, *in_out_tab->textsize-1, 2, in);
    if (out) apop_text_add(in_out_tab, *in_out_tab->textsize-1, 3, out);

    free(tagbase); free(in); free(last_out);
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

static int find_in_tab(char *target, int col){
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (!strcmp(in_out_tab->text[i][col], target)) return i;
    return -1;
}

char *in_out_get(char *tag, char in_or_out){
    int row = find_in_tab(tag, 0);
    return row < 0 ? NULL : in_out_tab->text[row][in_or_out=='i'? 2 : 3];
}

int run_predecessor(char *tag){
    int trow = find_in_tab(tag, 0);
    char *input_tab = in_out_tab->text[trow][3];

    if (apop_table_exists(input_tab)) return 0;

    int prev = find_in_tab(input_tab, 4);
    Tea_stopif(!strcmp(in_out_tab->text[prev][1], "input"),
                return text_in_by_tag(*in_out_tab->text[prev]),
                0, "Doing input for %s.", input_tab);
    Tea_stopif(is_recode_line(prev),
                return  make_recode_view(*in_out_tab->text[prev]),
                0, "Doing recodes for %s.", input_tab);
    return 0;
}
