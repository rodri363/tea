#define _GNU_SOURCE
#include <apop.h>
#include "imputation_variance.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <libgen.h> // For use in text_in.c to find directory of file

//I can't believe that R claims these for its own:
#undef error
//#undef warning

#define apop_strcmp(a, b) (((a)&&(b) && !strcmp((a), (b))) || (!(a) && !(b)))

#undef Apop_assert_c
#define Apop_assert_c(test, returnval, level, ...) if (!(test)) \
			{if (apop_opts.verbose >= level) warning(__VA_ARGS__); return returnval;}

#undef Apop_stopif
#define Apop_stopif(test, returnop, level, ...) if (test) \
			{if (apop_opts.verbose >= level) Rf_warning(__VA_ARGS__); Apop_maybe_abort(level);  returnop;}


/*
\ref init_edit_list : Here, we read in the peptalk file declaring the variables and the
edits, and then set up the one-zero matrix encoding all edits (explicit and implicit). As
long as your parent program is running, the em matrix will be there waiting to be used.


On threading: \c init_edit_list doesn't. It produces a single global em table, and sets
some auxiliary global vars as well. consistency_check is written to thread freely. That
is, the setup is write-once, query as often and as concurrently as you'd like.
*/


/** Read in a config file

  \param infile A pointer-to-string giving the filename to open and read
  \param dbname Returns a pointer-to-string with the name of the database that should be
  the first line of the config file.

  \ingroup config
  */
void read_spec(char **infile, char **dbname);

/** \defgroup edit */


/** Initialize the \c em matrix used for \ref consistency_check. You need to run this
  before you run \ref consistency_check; you need to run \c read_spec before you run
  this.

  \ingroup edit
*/
void init_edit_list();

extern int verbose;
void get_verbosity(int *out);
void set_verbosity(int *in);

/** There are three intents to this function, which you could read as three steps in a full
analysis:

\li Simply inquiring whether an input record passes edits.
\li Getting a list of fields that cause the edit to fail.
\li Getting a full list of alternatives that pass the edit.

Inputs:

\param record_name_in The list of variables that will be considered in checking the
constraints. This can be a subset of the full list of variables. Those not listed are ignored and can not cause edit failures.
\param ud_values The values you want the variables to have. A list of strings as long as
the \c record_name_in list.
\param record_in_size A pointer-to-int giving the number of records in the previous two
elements.
\param what_you_want A pointer-to-string (which is what R requires) that is one of
<tt>"passfail"</tt>, <tt>"failed_fields"</tt>, or <tt>"find_alternatives"</tt>.
\param id pointer-to-int giving a unique ID where I can file the output alternatives. The
row number is probably a fine choice.

Because there are three ways of running this function, there are three types of output:

\li In all cases, the \c fails_edits variable will be set to zero or one on output (where
1 means that the record does fail the edits).
\li For the second and third cases, I will fill \c record_fails, an array of equal length
as the input array of record names, with zeros or ones indicating whether the
corresponding field failed. If \c fails_edits is nonzero, then one of these elements will
be nonzero.
\li The full list of valid alternatives is not returned, because this is intended
to be called from R, and you can't return nontrivial data structures to R. Instead,
I save the result as <tt>alternatives[id]</tt>, where \c id is the unique id you input.
Any existing table at that point in the alternatives list gets freed when I write the new table.
  */
apop_data * consistency_check(char * const *record_name_in, char * const *ud_values, 
			int const *record_in_size, char const *const *what_you_want, 
			int const *id, int *fails_edits, int *failed_fields);

apop_data *checkData(apop_data *data);

//key-getting functions for the C side. Notice that get_text returns an apop_data set.
apop_data * get_key_text(char const *part1, char const *part2);
apop_data * get_key_text_tagged(char const *part1, char const *part2, char const *tag);
double get_key_float(char const *part1, char const * part2);
double get_key_float_tagged(char const *part1, char const * part2, char const *tag);
char* get_key_word(char const *part1, char const *part2);
char* get_key_word_tagged(char const *part1, char const *part2, char const *tag);
void set_key_text(char const *group, char const *key, char const *value);

void impute(char **active_tab);
void text_in();

void do_preedits(char **datatab);

/* Once you've made your filled_tab, you'll want an image of one draw or another.
   */
void check_out_impute(char **origin, char **dest, int *imputation_number, char **subset, char** filltabin);

//peptalk.y, public mostly for the recodes.
char * strip(const char *in); //Remove leading/trailing white space
void add_to_num_list(char *v);
void add_var(char const *var, int, char);

int genbnds_();
