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
    char *filltab = *filltabin ? *filltabin : "filled";
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
    } else dest = *origin;
    apop_assert_c(apop_table_exists(filltab), , 0, "No table named '%s'; did you already doMImpute()?", filltab);
    apop_data *fills = apop_query_to_text("select %s, field, value from %s where draw+0.0=%i and %s"
                                                     , id_column, filltab, *imputation_number, (subset && *subset) ? *subset : "1");
    begin_transaction();
    if (fills)
        for(int i=0; i< *fills->textsize; i++)
            apop_query("update %s set %s = '%s' "
                       "where %s = %s", 
                          dest, fills->text[i][1], fills->text[i][2], 
                          id_column, fills->text[i][0]);
    commit_transaction();
    apop_data_free(fills);
    free(id_column);
}

/* There used to be a check_in_impute, so you could modify the table and then 
  put the appropriate bits back in the fill-in table. We never used it, so I 
  removed it. It last appeared in revision b8a03fe3740eb6c07c . */
