#include "discrete.h"
#include "tea.h"

void generate_indices(char const *tablename); //in text_in.c
extern int file_read;

/* \key {group recodes/group id} The column with a unique ID for each group (e.g., household number).
 \key {group recodes/recodes} A set of recodes like the main set, but each calculation of the recode will be grouped by the group id, so you can use things like {\tt max(age)} or {\tt sum(income)}.
 */

void make_recode_view(){
	apop_data *test_for_recodes = apop_query_to_text("select distinct key from keys where "
            " key like 'recodes%%' or key like 'group recodes/recodes%%'");
	if (!test_for_recodes) return;
	apop_data_free(test_for_recodes);

    if (!file_read && get_key_word("input", "input file")) text_in();
    char *intab = get_key_word("input", "output table");
    char *recodestr, *group_recodestr, *viewname;
    asprintf(&viewname, "view%s", intab);
    char *overwrite = get_key_word("input", "overwrite");
    if (!overwrite || !strcasecmp(overwrite,"n") 
                || !strcasecmp(overwrite,"no") 
                || !strcasecmp(overwrite,"0") )
        {free(overwrite), overwrite = NULL;}
    Apop_assert_c (!(!overwrite && apop_table_exists(viewname)), , 0,
                    "Recode view %s exists and input/overwrite tells me to not recreate it.", viewname);

    apop_table_exists(viewname, 'd');
    char *rgroup="recodes", *grgroup="group recodes/recodes";
    recodes(&rgroup, NULL, &recodestr);
    recodes(&grgroup, NULL, &group_recodestr);
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
                   "where r.%s=g.%s", viewname, group_id, group_id);
    }
    else 
        apop_query("create view %s as select * %s from %s", viewname, XN(recodestr), intab);
//	apop_query("create table %s as select * %s from %s", viewname, XN(recodestr), intab);
//	generate_indices(viewname); //exactly the same indices as the main tab.
}
