#include "internal.h"
#include "tea.h"

int file_read = 0;

apop_data *make_type_table(){
    //apop_data *types = get_key_text("input", "types");
    apop_data *types = apop_text_alloc(NULL, 1, 2);
    for (int i=0; *used_vars[i].name != '\0'; i++){
        apop_text_alloc(types, types->textsize[0]+1, types->textsize[1]);
        apop_text_add(types, types->textsize[0]-2, 0, used_vars[i].name);
        apop_text_add(types, types->textsize[0]-2, 1, 
        used_vars[i].type=='r' || used_vars[i].type=='i' ? "numeric" : "text");
    }
    //set the default type at the end of the table to character
    apop_text_add(types, types->textsize[0]-1, 0, ".*");
    apop_text_add(types, types->textsize[0]-1, 1, "text");
    return types;
}

void generate_indices(char const *tablename){
    apop_data *indices = get_key_text("input", "indices");
    char *id_column = get_key_word(NULL, "id");
   if (id_column){
       apop_query("create index IDX_%s_%s on %s(%s)"
                            , tablename, id_column
                            , tablename, id_column);
   } else id_column = "rowid"; //SQLite-specific.
    if (indices)
        for (int i = 0; i< indices->textsize[0]; i++){
            if (apop_strcmp(indices->text[i][0], id_column)) continue;
            apop_query("create index IDX_%s_%s on %s(%s)"
                             , tablename, indices->text[i][0]
                             , tablename, indices->text[i][0]);
        }
}

int join_tables(){
    char *jointo = get_key_word("join", "host");
    if (!jointo) return 0;

    char *thistab = get_key_word("join", "add");
    char *idcol = get_key_word("id", NULL);
    Apop_assert(jointo && thistab, "If you have a 'join' segment in the spec, it has to have "
                    "a 'host' key and an 'add' key.");
    Apop_assert(idcol, "You asked me to join %s and %s, but I have no 'id' column name "
                        "on which to join (put it outside of all groups in the spec, "
                        "and until we get to implementing otherwise, it has to be the same for both tables).", thistab, jointo);
    apop_query("create index j%sidx on %s(%s);\n"
               "create index j%sidx on %s(%s);\n",
                thistab, thistab, idcol,
                jointo, jointo, idcol);
    return apop_query("create table tea_temptab as select * from "
               "%s, %s "
               "where %s.%s = %s.%s; \n"
               "drop table %s;\n"
               "create table %s as select * from tea_temptab;\n"
               "drop table tea_temptab;",
                thistab, jointo,
                jointo, idcol, thistab, idcol,
                jointo,
                jointo);
}


/* \key{input/input file} The text file from which to read the data set. This should be in
the usal comma-separated format with the first row listng column names.
\key{input/output table} The name of the table in the database to which to write the data read in.
\key input/overwrite If {\tt n} or {\tt no}, I will skip the input step if the output table already exists. This makes it easy to re-run a script and only sit through the input step the first time.
\key{input/primarky key} The name of the column to act as the primary key. Unlike other indices, the primary key has to be set on input.
\key input/indices Each row specifies another column of data that needs an index. Generally, if you expect to select a subset of the data via some column, or join to tables using a column, then give that column an index. The {\tt id} column you specified at the head of your spec file is always indexed, so listing it here has no effect.
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
    Apop_stopif(!table_out, return -1, 0, "I don't have a name for the output table.");
    Apop_assert_c (!(!overwrite && apop_table_exists(table_out)), 0, 0,
                        "Table %s exists; skipping the input from file %s.", table_out, file_in);
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
    commit_transaction();
    begin_transaction();
    apop_text_to_db(.text_file=file_in,  .tabname=table_out, 
                    .field_params=types, .table_params=table_key);
    commit_transaction();
	generate_indices(table_out);
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


/* \key database The database to use for all of this. It must be the first thing on your line.
 
I need it to know where to write all the keys to come.

\key id Provides a column in the data set that provides a unique identifier for each
observation.
Some procedure need such a column; e.g., multiple imputation will store imputations in a
table separate from the main dataset, and will require a means of putting imputations in
their proper place. Other elements of TEA, like flagging for disclosure avoidance, use the
same identifier.


\key recodes New variables that are deterministic functions of the existing data sets.
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

If you have edits based on a formula, then I'm not smart enough to set up the edit table
from just the recode formula. Please add the new field and its valid values in the \c
fields section, as with the usual variables.

If you have edits based on category-style recodes, I auto-declare those, because the
recode can only take on the values that you wrote down here.

\key {group recodes} Much like recodes (qv), but for variables set within a group, like
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
\end{lstlisting}
*/
