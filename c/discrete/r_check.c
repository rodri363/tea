#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "tea.h"
#include <rapophenia.h>

data_frame_from_apop_data_type *rapop_df_from_ad;//alloced in PEPedits.c:R_init_tea
apop_data_from_frame_type *rapop_ad_from_df;//alloced in PEPedits.c:R_init_tea

double check_one_row(apop_data *row, void *colnames_in){
   char **colnames= colnames_in;
   int record_count = 0;
   for ( ; colnames[record_count][0]!='\0'; record_count++)
        ;
   char *values[record_count];
   for (int i=0; colnames[i][0]!='\0'; i++){
       int datacol = apop_name_find(row->names, colnames[i], 'c');
       if (datacol > -2)
           asprintf(values[i], "%g", apop_data_get(row, .row=0, datacol));
       else {
           datacol = apop_name_find(row->names, colnames[i], 't');
           Apop_assert(datacol > -2, "I can't find %s in the names list.", colnames[i])
           values[i] =strdup(row->text[0][datacol]);
       }
   }

   int id = 0, fails_edits;
   char *pf = "passfail";

   consistency_check(colnames, values, &record_count, &pf, &id, &fails_edits, NULL);
   return fails_edits;
}


/**

  \param adata A data frame.
  \return A SEXP with one vector, zero=passed; one=failed. This is allocated here, so 
             free it somewhere else.

\todo Check where real variables get checked

 */
SEXP r_check_a_table(SEXP in){
    apop_data *d  =rapop_ad_from_df(in);

    //get column names that are in the edits. 
    apop_data *colnames = apop_query_to_text("select * from variables");
    int varcount = *colnames->textsize;
    char *recordnames[varcount+1];
    for (int i=0; i< varcount; i++)
        recordnames[i] = *colnames->text[i];
    recordnames[varcount] = strdup("");

    apop_data *outvector= apop_map(d, .fn_rp=check_one_row, .param=recordnames);
    apop_data_free(colnames);
    apop_data_free(d);
    return rapop_df_from_ad(outvector);
}
