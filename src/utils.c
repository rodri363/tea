#include "internal.h"


//take in a table name and a null-terminated list of fields.
//Check that the index on those fields exists; if not, create it.
//return 0 if index created ok
//return 1 if index already exists
//return -1 if index creation failed
int create_index_base(char const *tab, char const**fields){
    char *idxname=NULL;
    Asprintf(&idxname, "idx_%s", tab);
    for(char const**f=fields; *f; f++)
       xprintf(&idxname, "%s_%s", idxname, *f); 

    int out=1;
    apop_data *tabs= apop_query_to_text("select name from sqlite_master");
    for (int i=0; i< *tabs->textsize; i++)
        if (!strcmp(idxname, *tabs->text[i])) goto go;

    char *flist=strdup(*fields);
    for(char const**f=fields+1; *f; f++)
        xprintf(&flist, "%s, %s", flist, *f);

    out=apop_query("create index %s on %s (%s)", idxname, tab, flist);

    go:
    free(idxname);
    apop_data_free(tabs);
    return out;
}
