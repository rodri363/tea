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

void check_out_impute(char **origin, char **destin, int *imputation_number, char **subset){
    char *id_column= get_key_word(NULL, "id");
    const char *dest = destin ? *destin : NULL;
    if (!id_column) 
        id_column= get_key_word("impute", "id");
    int use_rowids = 0;
    if (!id_column) {
        use_rowids++;
        id_column = strdup("id_col");
    }
    //sprintf(apop_opts.db_name_column, "%s",  id_column);
    if (dest){
        apop_table_exists(dest, 'd');
        apop_query("create table %s as select %s * from %s", dest, use_rowids ? "rowid as id_col, " : " ", *origin);
        apop_query("create index icwwww on %s(%s)", dest, id_column);
    } else
        dest = *origin;
    apop_assert_c(apop_table_exists("filled"), , 0, "No table named 'filled'; did you already doMImpute()?");
    apop_data *fills = apop_query_to_text("select %s, field, value from filled where draw=%i and %s"
                                                     , id_column, *imputation_number, (subset && *subset) ? *subset : "1");
    begin_transaction();
    if (fills)
        for(int i=0; i< *fills->textsize; i++)
            apop_query("update %s set %s = '%s' "
                       "where %s = '%s'", 
                          dest, fills->text[i][1], fills->text[i][2], 
                          id_column, fills->text[i][0]);
    commit_transaction();
    apop_data_free(fills);
    free(id_column);
}

void check_in_impute(char *origin, int serialno, int imputation_number){
    apop_data *fields = apop_query_to_text("select field from filled where serialno=%i and draw = %i",
                                        serialno, imputation_number);
    char *id_column= get_key_word(NULL, "id");
    for (int i=0; i< fields->textsize[0]; i++)
        apop_query("update filled set value = (select %s from %s where %s = %i) where "
                "serialno=%i and draw=%i and field=%s",
                fields->text[i][0], origin, id_column,serialno,
                serialno, imputation_number, fields->text[i][0]);
    free(id_column);
}
