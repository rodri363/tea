#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "tea.h"
#include <rapophenia.h>

char **(*TEA_R_STRSXP_TO_C)(SEXP s);
data_frame_from_apop_data_type *rapop_df_from_ad;//alloced in PEPedits.c:R_init_tea
apop_data_from_frame_type *rapop_ad_from_df;//alloced in PEPedits.c:R_init_tea

int using_r = 0; //r_init handles this. If zero, then it's a standalone C library.

void R_init_tea(DllInfo *info){
    using_r=1;
	TEA_R_STRSXP_TO_C = (char **(*)(SEXP)) R_GetCCallable("Rapophenia","R_STRSXP_TO_C");
    rapop_df_from_ad =  (void*) R_GetCCallable("Rapophenia", "data_frame_from_apop_data");
    rapop_ad_from_df =  (void*) R_GetCCallable("Rapophenia", "apop_data_from_frame");
}

// There used to be an RCheckConsistency function here; it last appeared in commit c11ad596

SEXP RCheckData(SEXP df){
    apop_data *d  =rapop_ad_from_df(df);
    apop_data *outd = checkData(d, 0); //1=do preedits. To do: let users decide.
    SEXP out = rapop_df_from_ad(outd);
    apop_data_free(d);
    apop_data_free(outd);
    return out;
}
