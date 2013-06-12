#include "internal.h"

int file_read = 0;
char * gnu_c_basename(char *);

/** Use SQLite3's pragmas to determine whether a column in a table is indexed. 

  \param table The table name.
  \param column The column name.
  \param make_idx If nonzero, and the index doesn't exist, create it now, with the form IDX_yrtable_yrcol. Barring errors, return 1, because an index now exists.

  \return 1==yes, is indexed. \\
          0==no, not indexed.\\
          -2==no such table.\\
          -1==query error.
*/
int has_sqlite3_index(char const *table, char const *column, char make_idx){
    if (!apop_table_exists(table)) return -2;
    apop_data *indices = apop_query_to_mixed_data("mtm", "pragma index_list(%s)", table);
    int has_it = 0;
    if (!indices) goto out;
    Apop_stopif(indices->error, apop_data_free(indices); return -1, 0, "error running "
            " 'pragma index_list(%s)'. Check table name? Running SQLite?", table);
    for (int i=0; i<*indices->textsize; i++){
        apop_data *coldata = apop_query_to_mixed_data("mmt", "pragma index_info(%s)", *indices->text[i]);
        if (!coldata || !*coldata->textsize) continue;
        Apop_stopif(coldata->error, apop_data_free(coldata); return -1, 0, "error running "
                " 'pragma index_info(%s)'. Check table/index name?", *indices->text[i]);
        if (!strcasecmp(*coldata->text[0], column)) { 
            has_it=1; 
            apop_data_free(coldata);
            goto out;
        }
        apop_data_free(coldata);
    }
out:
    if(has_it==0 && make_idx){
       apop_query("create index IDX_%s_%s on %s(%s)"
                            , table, column
                            , table, column);
        has_it = has_sqlite3_index(table, column, 0);
    }
    apop_data_free(indices);
    return has_it;
}

void test_has_sqlite3_index(){
    apop_query("create table ab(a, b)");
    apop_query("create index abi on ab(a)");
    assert(has_sqlite3_index("ab", "a", 0));
    assert(!has_sqlite3_index("ab", "b", 0));
    assert(!has_sqlite3_index("ab", "c", 0));
    assert(has_sqlite3_index("ac", "b", 1)==-2);
    apop_query("drop table ab");
}

apop_data *make_type_table(){
    //apop_data *types = get_key_text("input", "types");
    apop_data *types = apop_text_alloc(NULL, 1, 2);
    for (int i=0; used_vars && used_vars[i].name; i++){
        apop_text_alloc(types, types->textsize[0]+1, 2);
        apop_text_add(types, types->textsize[0]-2, 0, used_vars[i].name);
        apop_text_add(types, types->textsize[0]-2, 1, 
        used_vars[i].type=='r' || used_vars[i].type=='i' ? "numeric" : "text");
    }
    //set the default type at the end of the table to character
    apop_text_add(types, types->textsize[0]-1, 0, ".*");
    apop_text_add(types, types->textsize[0]-1, 1, "text");
    return types;
}

void generate_indices(char const *tag){
    char const *table_holder = get_key_word_tagged("input", "output table", tag);
    char *table_out;

    asprintf(&table_out, "view%s", table_holder);
     
    apop_data *indices = get_key_text("input", "indices");
    char *id_column = get_key_word(NULL, "id");
   if (id_column) has_sqlite3_index(table_out, id_column, 'y');
   else id_column = "rowid"; //SQLite-specific.
   if (indices)
       for (int i = 0; i< *indices->textsize; i++){
           if (apop_strcmp(*indices->text[i], id_column)) continue;
           has_sqlite3_index(table_out, *indices->text[i], 'y');
       }
}

/* TeaKEY(join/host, <<<The main data set to be merged with.>>>)
TeaKEY(join/add, <<<The set to be merged in to join/host.>>>)
TeaKEY(join/output table, <<<The name of the table (actually, a view) with the join of both tables. Use this as the basis for subsequent steps.>>>)
TeaKEY(join/field, <<<The name of the field appearing in both tables on which the join takes place. If you don't provide this, use the id key.>>>)
*/
int join_tables(){
    char *jointo = get_key_word("join", "host");
    if (!jointo) return 0;

    char *addtab = get_key_word("join", "add");
    char *specid = get_key_word("join", "field");
    char *idcol = specid ? specid : get_key_word("id", NULL);
    char *outview = get_key_word("join", "output table");
    Apop_stopif(!jointo || !addtab || !outview, return -1, 0, "If you have a 'join' segment in the spec, it has to have "
                    "a 'host' key, an 'add' key, and an 'output table' key.");
    Apop_stopif(!idcol, return -1, 0, "You asked me to join %s and %s, but I have no 'id' column name "
                        "on which to join (put it outside of all groups in the spec, "
                        "and until we get to implementing otherwise, it has to be the same for both tables).", addtab, jointo);
    has_sqlite3_index(jointo, idcol, 'y');
    has_sqlite3_index(addtab, idcol, 'y');
    return apop_query("create table %s as select * from "
               "%s join %s "
               "on %s.%s = %s.%s;",
                outview,
                addtab, jointo, 
                addtab, idcol, jointo, idcol);
}

/*
TeaKEY(input/types, <<<Specifies the type and range of variables (which is used later in consistency checking).>>>)

TeaKEY(input, <<<The key where much of the database/input related subkeys are defined.
Descriptions of these subkeys can be found elsewhere in the appendix.>>>)

TeaKEY(input/input file, <<<The text file from which to read the data set. This should be in
the usual comma-separated format (CSV) with the first row of the file listng column names. We recommend separating|fields|with|pipes, because pipes rarely appear in addresses or other such data.>>>)

TeaKEY(input/output table, <<<Name for the database table generated from the input file.>>>)

TeaKEY(input/overwrite, <<<If {\tt n} or {\tt no}, I will skip the input step if
the output table already exists. This makes it easy to re-run a script and only wait
through the input step the first time. Otherwise, the default is to overwrite.  >>>)

TeaKEY(input/primary key, <<<The name of the column to act as the primary key. Unlike other indices, the primary key has to be set on input.>>>)

TeaKEY(input/indices, <<<Each row specifies another column of data that needs an index. Generally, if you expect to select a subset of the data via some column, or join to tables using a column, then give that column an index. The {\tt id} column you specified at the head of your spec file is always indexed, so listing it here has no effect. Remark, however, that we've moved the function generate_indices(table_out) to bridge.c:428 to after the recodes.>>>)

TeaKEY(input/missing marker, <<<How your text file indicates missing data. Popular choices include "NA", ".", "NaN", "N/A", et cetera.>>>)
*/
static int text_in_by_tag(char const *tag){
    char *file_in   = get_key_word_tagged("input", "input file", tag);
    char *table_out = get_key_word_tagged("input", "output table", tag);
    char *nan_marker = get_key_word_tagged("input", "missing marker", tag);
    if (nan_marker) sprintf(apop_opts.db_nan, "%s", nan_marker);
    if (!nan_marker) nan_marker=apop_opts.db_nan;
    char *overwrite = get_key_word_tagged("input", "overwrite", tag);
    if (!overwrite  || !strcasecmp(overwrite,"n") 
                    || !strcasecmp(overwrite,"no") 
                    || !strcasecmp(overwrite,"0") )
            free(overwrite), overwrite = NULL;

    Apop_stopif(!file_in, return -1, 0,  "I don't have an input file name");

    char *file_in_copy;
    char *sas_post_script;
    asprintf(&file_in_copy, file_in);
    asprintf(&sas_post_script, "sas7bdat");
    file_in_copy += strlen(file_in) - 8;

    Apop_stopif(!table_out, return -1, 0, "I don't have a name for the output table.");
    Apop_stopif(!overwrite && apop_table_exists(table_out), return 0, 0,
                        "Table %s exists; skipping the input from file %s.", table_out, file_in);

    // Script that converts a sas input file into a regular text file
    if(strcmp(file_in_copy, sas_post_script) == 0){
        /* Below we write a shell script to convert the user's SAS input file into a text
         * file that can be parsed just as any other file by TEA. The file gets written to
         * the database given by the user in their spec file. Remark that we don't
         * need to check that the given file exists because this is already handled by the
         * Apop_stopif statement above.
         */

        char *basename = gnu_c_basename(strndup(file_in, strlen(file_in)-9));
        char *directory = dirname(file_in);
        if(!strcmp(directory, ".")) asprintf(&directory, " ");

        if (overwrite) apop_table_exists(table_out, 'd');

       return apop_system(
           "sas -noterminal -stdio <<XXXXXX| apop_text_to_db -d',' - %s %s;\n"
           "libname indata '%s';    \n"
           "PROC EXPORT             \n"
           "DATA=indata.%s          \n"
           "OUTFILE='STDOUT'        \n"
           "DBMS=CSV REPLACE;       \n"
           "PUTNAMES=YES;           \n"
           "run;                    \n"
           "XXXXXX\n", table_out, get_key_word("database", NULL), directory, basename
       );
    }

	printf("Reading text file %s into database table %s.\n", file_in, table_out);

    if (overwrite) apop_table_exists(table_out, 'd');

    apop_data *types = make_type_table();


    char *table_key = NULL;
    char comma = ' ';
    apop_data *primary_key = get_key_text("input", "primary_key");
    if (primary_key){
        asprintf(&table_key, "primary key(");
        for (int i=0; i< primary_key->textsize[0]; i++){
            asprintf(&table_key, "%s%c%s", table_key, comma, primary_key->text[i][0]);
            comma=',';
        }
        asprintf(&table_key, "%s)", table_key);
    }

    //OK, got all the config info. Go.
    if (verbose) fprintf(stderr, "Reading in file %s to table %s.", file_in, table_out);
    begin_transaction();
    apop_text_to_db(.text_file=file_in,  .tabname=table_out, 
                    .field_params=types, .table_params=table_key);
    commit_transaction();
    //We've moved generate_indices(table_out) to after recodes at bridge.c:428
    file_read ++;
    apop_data_free(types);
    return 0;
}

void text_in(){
    apop_data *tags=apop_query_to_text("%s", "select distinct tag from keys where key like 'input/%' order by count");
    if (!tags) return;
    for (int i=0; i< *tags->textsize;i++)
        text_in_by_tag(*tags->text[i]);
    apop_data_free(tags);
}

// Hilariously, including libgen.h for dirname gives us the POSIX version of
// basename instead of GNU C. So I just instantiated it here.
char *gnu_c_basename(char *file_input)
{
            char *basename = strrchr(file_input, '/');
                        return basename ? basename+1 : file_input;
}



/* TeaKEY(database, <<<The database to use for all of this. It must be the first line in your spec file because all the rest of the keys get written to the database you specify. If you don't specify a database than the rest of the keys have nowhere to be written and your spec file will not get read correctly.>>>)

TeaKEY(id, <<<Provides a column in the data set that provides a unique identifier for each observation.
Some procedures need such a column; e.g., multiple imputation will store imputations in a
table separate from the main dataset, and will require a means of putting imputations in
their proper place. Other elements of Tea, like flagging for disclosure avoidance, use the
same identifier. This identifier may be built by a recode.>>>) 
The function that creates an index for the key in the specified output table is located at bridge.c:428

TeaKEY(recodes, <<<New variables that are deterministic functions of the existing data sets.
There are two forms, one aimed at recodes that indicate a list of categories, and one
aimed at recodes that are a direct calculation from the existing fields.
For example (using a popular rule that you shouldn't date anybody who is younger than
(your age)/2 +7),

\begin{lstlisting}[language=]
recodes { 
    pants {
        yes | leg_count = 2
        no  |                   #Always include one blank default category at the end.
    }

    youngest_date {
        age/2 + 7
    }
}
\end{lstlisting}

You may chain recode groups, meaning that recodes may be based on previous recodes. Tagged
recode groups are done in the sequence in which they appear in the file. [Because the
order of the file determines order of execution, the tags you assign are irrelevant, but
I still need distinct tags to keep the groups distinct in my bookkeeping.]

\begin{lstlisting}
recodes [first] {
    youngest_date: (age/7) +7        #for one-line expressions, you can use a colon.
    oldest_date: (age -7) *2
}

recodes [second] {
    age_gap {
        yes | spouse_age > youngest_date && spouse_age < oldest_date
        no  | 
    }

}
\end{lstlisting}
>>>)

TeaKEY(group recodes,<<<Much like recodes (qv), but for variables set within a group, like
eldest in household.
For example,
\begin{lstlisting}[language=]
group recodes { 
    group id : hh_id
    eldest: max(age)
    youngest: min(age)
    household_size: count(*)
    total_income: sum(income)
    mean_income: avg(income)
}
\end{lstlisting}>>>)
*/
