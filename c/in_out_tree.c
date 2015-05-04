/* This file maintains a table with one line per segment of the input spec, listing
the segment's input and output tables. We can use this table to determine whether a
certain input is derived from a previous segment of the spec.

The table could thus be expressed as a set of linked lists, but that much structure seemed like overkill.
*/

#define Tag(rr) in_out_tab->text[rr][0]         //col 0=tag
#define SegType(rr) in_out_tab->text[rr][1]     //1=input, recode, join, ...
#define Input(rr) in_out_tab->text[rr][2]       //2=input table name
#define Output(rr) in_out_tab->text[rr][3]      //3=output table name
#define Overwrite(rr) in_out_tab->text[rr][4]   //4='y' if overwrite
#define Match(L, R) (!strcasecmp((L), (R))) //got sick of typing this so often.
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

    int ts = in_out_tab ? *in_out_tab->textsize : 0;
    in_out_tab = apop_text_alloc(in_out_tab, ts+1, 5);

    char *in = get_key_word_tagged(tagbase, "input table", tag);
    bool redo_join_tag = false;
    if (Match(tagbase, "join")) {
        if (ts>=1 && Match(SegType(ts-1), "join")
                  && Match(in_out_tab->text[ts-1][0], tag))
            in = get_key_word_tagged(tagbase, "add", tag);
        else {
            redo_join_tag=true;
            in = get_key_word_tagged(tagbase, "host", tag);
        }
    }
    if (!in && !Match(tagbase, "input")) in = strdup(last_out ? last_out: "");

    char *out = get_key_word_tagged(tagbase, "output table", tag);
    if (!out && !Match(tagbase, "impute") && !Match(tagbase, "input"))
        out = in ? strdup(in): NULL;

    apop_text_add(in_out_tab, ts, 0, tag);
    apop_text_add(in_out_tab, ts, 1, tagbase);
    apop_text_add(in_out_tab, ts, 2, in);
    if (out) apop_text_add(in_out_tab, ts, 3, out);

    char *overwrite = get_key_word_tagged(tagbase, "overwrite", tag);
    apop_text_add(in_out_tab, ts, 4,
            (overwrite && (overwrite[0]=='y' || overwrite[0]=='Y'))
            || (!overwrite && Match(tagbase, "impute"))
            || (!overwrite && Match(tagbase, "edit"))
                ? "y" : "n");

    apop_data_free(rows); free(in); free(last_out);
    last_out = out;
    if (redo_join_tag) in_out_row_add(tag);
} 

static bool is_recode_line(int i){ 
    return Match(SegType(i), "recodes") || Match(SegType(i), "group recodes");
}

/* Recodes may depend on previous recodes. The last (only) must be named view<input>.
   The first intermediate will be mid<input> the next midmid<input> &c.
*/
void in_out_recode_fix(){
    in_out_tab->vector = gsl_vector_calloc(*in_out_tab->textsize);
    for (int i=0; i< *in_out_tab->textsize; i++){
        if (!is_recode_line(i)) continue;
        bool has_successors = false;
        for (int j=i+1; j < *in_out_tab->textsize; j++){
            if (!is_recode_line(j) || !Match(Input(j), Input(i))) 
                continue;
            has_successors = true;
            apop_text_add(in_out_tab, j, 2, "mid%s", Input(i));
            (*gsl_vector_ptr(in_out_tab->vector, j))++;
        }
        if (!has_successors)
            apop_text_add(in_out_tab, i, 3, "view%s",
                             Input(i)+3*(int)apop_data_get(in_out_tab, i));//skip the 'mid's.
        else
            apop_text_add(in_out_tab, i, 3, "mid%s", Input(i));
    }
    gsl_vector_set_all(in_out_tab->vector, 0);
}

//find the output table matching a given input table
static int find_in_tab(char const *target, int col){
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (Match(in_out_tab->text[i][col], target)) return i;
    return -1;
}

char *in_out_get(char const *tag, char in_or_out){
    int row = find_in_tab(tag, 0);
    return row < 0 ? NULL : in_out_tab->text[row][in_or_out=='i'? 2 : 3];
}

bool run_one_tag(int row, char **active_tab, void *aux_info, bool *rebuild){
    int prev = find_in_tab(Input(row), 3);
    bool OK = true, use_this_join=false;

    bool r2=false;
    if (row+1 < *in_out_tab->textsize && Match(SegType(row), "join")
            && Match(SegType(row+1), "join") && Match(Tag(row+1), Tag(row))){
        OK = run_one_tag(row+1, active_tab, run_one_tag, &r2);
        *rebuild = *rebuild || r2;
        Tea_stopif(!OK, return false,
                0, "Trouble building predecessor table for %s.", Output(row+1));
        use_this_join=true;
    }

    r2 = false;
    if (prev>=0 && prev < row) OK = run_one_tag(prev, active_tab, aux_info, &r2);
    *rebuild = *rebuild || r2;
    Tea_stopif(!OK, /*return false*/,
            0, "Trouble building predecessor table for %s.", Output(row));

    if (!Match(SegType(row), "impute") && !Match(SegType(row), "edit")) {
        if (Overwrite(row)[0]=='y' || *rebuild) {
            apop_table_exists(Output(row), 'd');
            *rebuild=true;
            Overwrite(row)[0]='n'; //only overwrite once.
        } else 
            if (apop_table_exists(Output(row))) return true;
    }

    *active_tab = Output(row);
    Tea_stopif(Match(SegType(row), "edit"),
                return an_edit(Input(row), Output(row), Tag(row)),
                0, "Doing edits for %s.", Input(row));
    Tea_stopif(Match(SegType(row), "input"),
                return text_in_by_tag(Tag(row)),
                0, "Doing input for %s.", Output(row));
    Tea_stopif(Match(SegType(row), "impute"),
                return do_impute(in_out_tab->text[row], active_tab, aux_info),
                0, "Doing imputations for %s%s%s."
                , Input(row), Output(row) ? "; writing to fill table ":""
                , Output(row));
    Tea_stopif(Match(SegType(row), "join") && use_this_join,
                return join_tables(Tag(row)),
                0, "Joining tables to produce %s.", Output(row));
    Tea_stopif(is_recode_line(row),
                return make_recode_view(Tag(row)),
                0, "Doing recodes for %s.", Input(row));
    return true;
}

bool run_all_tags(char *type, char **active_tab, void* aux_info){
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (Match(SegType(i), type)
            ||(Match("RRR", type) && is_recode_line(i)))
            {Tea_stopif(!run_one_tag(i, active_tab, aux_info, (bool[]){false}),
                return false, 0,
                "Trouble doing %s segment with tag '%s'.", type, Tag(i));}
    return true;
}

void build_one_table(char **tab){ //char ** to be R-friendly
    Tea_stopif(!tab || !*tab, return, 0, "No input table given");
    Tea_stopif(!in_out_tab, return, 0, "No table generation info. Have you run readSpec?");
    int row=-1;
    for (int i=0; i< *in_out_tab->textsize; i++)
        if (!strcmp(Output(i), *tab)) {row=i; break;}
    Tea_stopif(row==-1, return, 0, "Could not find %s in the list of output tables.", *tab);

    char *active_tab=NULL;
    run_one_tag(row, &active_tab, NULL, (bool[]){true});
}
