#include "discrete.h"
#include "tea.h"

int file_read = 0;


    
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

/* \key{input/input file} The text file from which to read the data set. This should be in
the usal comma-separated format with the first row listng column names.
\key{input/output table} The name of the table in the database to which to write the data read in.
\key input/overwrite If {\tt n} or {\tt no}, I will skip the input step if the output table already exists. This makes it easy to re-run a script and only sit through the input step the first time.
\key{input/primarky key} The name of the column to act as the primary key. Unlike other indices, the primary key has to be set on input.
\key input/indices Each row specifies another column of data that needs an index. Generally, if you expect to select a subset of the data via some column, or join to tables using a column, then give that column an index. The {\tt id} column you specified at the head of your spec file is always indexed, so listing it here has no effect.
*/

void text_in(){
    char *file_in   = get_key_word("input", "input file");
    char *table_out = get_key_word("input", "output table");
    char *nan_marker = get_key_word("input", "missing marker");
    if (nan_marker) sprintf(apop_opts.db_nan, "%s", nan_marker);
    if (!nan_marker) nan_marker=apop_opts.db_nan;
    Apop_assert_c(!file_read, , 0, "Already read in the input file %s; not doing it again.", file_in);
    Apop_assert(file_in, "I don't have an input file name");
    Apop_assert(table_out, "I don't have a name for the output table.");
	printf("Reading text file %s into database table %s.\n", file_in, table_out);

    char *overwrite = get_key_word("input", "overwrite");
    if (!overwrite  || !strcasecmp(overwrite,"n") 
                    || !strcasecmp(overwrite,"no") 
                    || !strcasecmp(overwrite,"0") )
            free(overwrite), overwrite = NULL;
    Apop_assert_c (!(!overwrite && apop_table_exists(table_out)), , 0,
                        "Table %s exists; skipping the input from file %s.", table_out, file_in);
    if (overwrite) apop_table_exists(table_out, 'd');

    //apop_data *types = get_key_text("input", "types");
    apop_data *types = apop_query_to_text("select key,value from keys where key like 'input/types/%%'");
    if (types){
        for (int i = 0; i< types->textsize[0]; i++)
            types->text[i][0] += strlen("input/types/");//step past this to the varname
        apop_text_alloc(types, types->textsize[0]+1, types->textsize[1]);
    } else
        types = apop_text_alloc(NULL, 1, 2);
    //set the default type at the end of the table to character
    apop_text_add(types, types->textsize[0]-1, 0, ".*");
    apop_text_add(types, types->textsize[0]-1, 1, "text");


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
    apop_text_to_db(.text_file=file_in,  .tabname=table_out, 
                    .field_params=types, .table_params=table_key);
	generate_indices(table_out);
    file_read ++;
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
