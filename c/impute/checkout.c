#include "tea.h"
#include "internal.h" //begin_transaction; commit_transaction;
extern char *datatab;

/* Our imputation system gave us a fill tab, and there's the original tab with its NaN or
   otherwise bad values still in place. This here will generate a new table that uses the
   right values.

   \param origin The name of a table to be filled in.
   \param dest If \c NULL, then in place, else make a copy and fill that.
   \param imputation_number We're doing multiple imputation, so we'll need this.
   \param subset I'll put this in the where clause of the query, should you just want a subset of the main table
*/

void check_out_impute(char **origin, char **destin, int *imputation_number, char **subset, char **filltabin){
    char *filltab = (filltabin && *filltabin) ? *filltabin : "filled";
    Apop_stopif(!origin || !*origin, return, 0, "NULL origin table, but I need that.");
    char *id_column= get_key_word(NULL, "id");
    const char *dest = destin ? *destin : NULL;
    if (!id_column) 
        id_column= get_key_word("impute", "id");
    int use_rowids = 0;
    if (!id_column) {
        use_rowids++;
        id_column = strdup("id_col");
    }
    sprintf(apop_opts.db_name_column, "%s",  id_column);
    if (dest){
        apop_table_exists(dest, 'd');
        apop_query("create table %s as select %s * from %s %s %s", 
                        dest, 
                        use_rowids ? "rowid as id_col, " : " ", *origin,
                        (subset && *subset) ? "where" : " ",
                        (subset && *subset) ? *subset : " "
                        );
        apop_query("create index idxidx%s on %s(%s)", dest, dest, id_column);
    } else dest = *origin;
    apop_assert_c(apop_table_exists(filltab), , 0, "No table named '%s'; did you already doMImpute()?", filltab);
    apop_data *fills = apop_query_to_text("select %s, field, value from %s where draw+0.0=%i"
                                              , id_column, filltab, *imputation_number);
    Apop_stopif(!fills || fills->error, return, 0, "Expected fill-in table "
                "%s, but couldn't query it.", filltab);
    begin_transaction();
    if (fills)
        for(int i=0; i< *fills->textsize; i++)
            apop_query("update %s set %s = '%s' "
                       "where %s = %s", 
                          dest, fills->text[i][0], fills->text[i][1], 
                          id_column, fills->names->row[i]);
    commit_transaction();
    apop_data_free(fills);
    free(id_column);
}

/* There used to be a check_in_impute, so you could modify the table and then 
  put the appropriate bits back in the fill-in table. We never used it, so I 
  removed it. It last appeared in revision b8a03fe3740eb6c07c . */

void test_check_out_impute(){
    apop_table_exists("testcheckoutbase", 'd');
    apop_table_exists("testcheckoutfill", 'd');
    apop_table_exists("testcheckoutbase_copy", 'd');
    apop_table_exists("tcb", 'd');
    apop_query("create table testcheckoutbase (id, a, b); "
            " insert into testcheckoutbase values(0, 3, 0./0.);"
            " insert into testcheckoutbase values(1, 0./0., 3);"
            " insert into testcheckoutbase values(2, 3, 3);"
            "create table testcheckoutfill (draw, value, id, field); "
            " insert into testcheckoutfill values(0, 3, 1, 'a');"
            " insert into testcheckoutfill values(0, 3, 0, 'b');"
            " insert into testcheckoutfill values(1, 9, 1, 'a');"
            " insert into testcheckoutfill values(1, 6, 0, 'b');"
            );
    char *strings[] = {"testcheckoutbase", //0
                       "testcheckoutfill", //1
                       "testcheckoutbase_copy", //2
                       "tcb" //3
    };
    set_key_text("id", NULL, "id");
    check_out_impute( strings+0, strings+2, (int[]){0}, NULL, strings+1);
    assert(apop_query_to_float("select avg(a) from testcheckoutbase_copy")==3);
    assert(apop_query_to_float("select avg(b) from testcheckoutbase_copy")==3);
    check_out_impute(strings+0, strings+3, (int[]){1}, NULL, strings+1);
    assert(apop_query_to_float("select avg(a) from tcb")==5);
    assert(apop_query_to_float("select avg(b) from tcb")==4);
    apop_table_exists("testcheckoutbase", 'd');
    apop_table_exists("testcheckoutfill", 'd');
    apop_table_exists("testcheckoutbase_copy", 'd');
    apop_table_exists("tcb", 'd');
}
