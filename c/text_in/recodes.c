#include "discrete.h"
#include "tea.h"

void generate_indices(char const *tablename); //in text_in.c
extern int file_read;

/* Generate the recode view using the recodes segment.

We're also going to create triggers for those variables which have an imputation
section. So far, imputation is the only way in which variables can change, so if the
variable isn't mentioned in the imputes, it doesn't need a trigger.


\key {group recodes/group id} The column with a unique ID for each group (e.g., household number).
\key {group recodes/recodes} A set of recodes like the main set, but each calculation of the recode will be grouped by the group id, so you can use things like {\tt max(age)} or {\tt sum(income)}.
 */
void make_recode_view(char **tag, char **first_or_last){
    //first_or_last may be "first", "last", "both", or "middle"
    char *q = strdup("select distinct key from keys where "
            " (key like 'recodes%' or key like 'group recodes/recodes%')");
    if (tag && *tag) asprintf(&q, "%s and tag like '%%%s%%'", q, *tag);
	apop_data *test_for_recodes = apop_query_to_text("%s", q);
    free(q);
	if (!test_for_recodes) return;
	apop_data_free(test_for_recodes);

    if (!file_read && get_key_word("input", "input file")) text_in();

    //Names are a pain: the first input should be the intab,
    //the last output should be view[intab]
    //but in the middle, we may chain an arbitrary number of views together,
    //so we'll need to chain this in = last out and make up intermediate names.
    static char *intab =NULL;
    static char *out_name =NULL;
    if (apop_strcmp(*first_or_last, "first") 
           || apop_strcmp(*first_or_last, "both"))
        intab = get_key_word("input", "output table");
    else {//chaining from before
        free(intab);
        intab = strdup(out_name);
    }
    if (apop_strcmp(*first_or_last, "last") 
           || apop_strcmp(*first_or_last, "both"))
         asprintf(&out_name, "view%s", get_key_word("input", "output table"));
    else {
        free(out_name);
        asprintf(&out_name, "mid%s", intab);
    }

    char *recodestr, *group_recodestr;
    char *overwrite = get_key_word("input", "overwrite");
    if (!overwrite || !strcasecmp(overwrite,"n") 
                || !strcasecmp(overwrite,"no") 
                || !strcasecmp(overwrite,"0") )
        {free(overwrite), overwrite = NULL;}
    /*Apop_assert_c (!(!overwrite && apop_table_exists(out_name)), , 0,
                    "Recode view %s exists and input/overwrite tells me to not recreate it.", out_name);
*/
    apop_table_exists(out_name, 'd');
    char *rgroup="recodes", *grgroup="group recodes/recodes";
    recodes(&rgroup, tag, &recodestr, &intab);
    recodes(&grgroup, tag, &group_recodestr, &intab);
    if (group_recodestr){
        char *group_id= get_key_word("group recodes", "group id column");
        Apop_assert(group_id, "There's a group recodes section, but no \"group id column\" tag.");
        apop_table_exists("tea_group_stats", 'd');
        apop_table_exists("tea_record_recodes", 'd');
        apop_query("create view tea_group_stats as "
                   "select %s %s from %s group by %s; ",
                     group_id, group_recodestr, intab, group_id);
        apop_query("create view tea_record_recodes as select * %s from %s", XN(recodestr), intab);
        apop_query("create view %s as select * from tea_record_recodes r, tea_group_stats g "
                   "where r.%s=g.%s", out_name, group_id, group_id);
    }
    else 
        apop_query("create view %s as select * %s from %s", out_name, XN(recodestr), intab);

    //OK! Now let's set up triggers so that users can update the view without thinking
    //about it.
    
    apop_data *imputables= get_variables_to_impute(NULL);
    if (!imputables) return;

    intab = get_key_word("input", "output table");
    char *idcol= get_key_word(NULL, "id");
    apop_query("create index %sidcol on %s(%s)", intab, intab, idcol);
    apop_data *o = apop_query_to_data("select * from %s limit 1", intab);
    for (int i=0; i< o->names->colct; i++){
        int is_imputable=0; //only variables in the impute list need triggers.
        char *col= o->names->column[i];
        for (int j=0; !is_imputable && j< imputables->textsize[0]; j++)
            if (apop_strcmp(*imputables->text[j], col))
                is_imputable++;
        if (is_imputable)
            apop_query("drop trigger if exists trig%s%s; "
                "create trigger trig%s%s instead of update of %s on view%s  "
                "begin update %s set %s = new.%s where %s=old.%s; end",
                 intab, col, intab, col, col, intab, intab, col, col, idcol, idcol);
    }
    apop_data_free(o);
    apop_data_free(imputables);

//	generate_indices(out_name); //exactly the same indices as the main tab.
}
