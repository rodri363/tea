#include <apop.h>


#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
/*
#undef Apop_assert
#undef Apop_assert_c
#define Apop_assert(test, ...) if (!(test)) error(__VA_ARGS__);

#define Apop_assert_c(test, returnval, level, ...) if (!(test)) \
			{if (apop_opts.verbose >= level) warning(__VA_ARGS__); return returnval;}
*/


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
apop_data * consistency_check(char const **record_name_in, char const **ud_values, 
			int const *record_in_size, char const **what_you_want, 
			int const *id, int *fails_edits, int *failed_fields);

#include <apop.h>

/** \defgroup config Reading configuration files


Given that \ref read_spec has read in one or more config files, this function will pull
individual values.

This function can take zero through three arguments. Keys are typically of the form
"category/subcat/key", and if you have several in the same group, you can reduce
redundancy by splitting off the repetitive part. All of the following are valid:
\code
apop_data *val0 = get_key_text("category/subcat/key0");

char *base = "category/subcat";
apop_data *val1 = get_key_text(base, "key1", .optional = 'y');
apop_data *val2 = get_key_text(base, "key2");
\endcode


  */

/** \fn double get_key_float(char *part1, char * part2, char optional);

Given that \ref read_spec has read in one or more config files, this function will pull
individual values.

  \param part1
  \param part2
  \param default_val If blank, then I'll throw an error and halt if I don't
  find what you're looking for.  Else, I return this value if no key is found in the config file.

  \return An \c apop_data set with a list of text elements. E.g.
  \code
  apop_data *vals = get_key_text("keyset/key", .default_val=18);
for (int i=0; i < vals->textsize[0]; i++)
   printf("%s\n", vals->text[i][0]);
   \endcode
   \ingroup config

A small warning: the internals can't distinguish between no default value and
<tt>.default_val=0</tt>, so design your keys with the knowledge that zero basically can't
be the default.

  */

/** \fn apop_data * get_key_text(char *part1, char * part2, char optional);


  \param part1 The base of the key
  \param part2 The end of the key
  \param default_val If not given, then I'll throw an error and halt if I don't
  find what you're looking for.  Else, I return this value if no key is found in the config file.

  \return An \c apop_data set with a list of text elements. E.g.
  \code
  apop_data *vals = get_key_text("keyset/key", .optional='n');
for (int i=0; i < vals->textsize[0]; i++)
   printf("%s\n", vals->text[i][0]);
   \endcode

   \ingroup config
  */

//key-getting functions for the C side. Notice that get_text returns an apop_data set.
apop_data * get_key_text(char *part1, char *part2);
apop_data * get_key_text_tagged(char *part1, char *part2, char *tag);
double get_key_float(char *part1, char * part2);
apop_data* get_sub_key(char *part1);
char* get_key_word(char *part1, char *part2);
char* get_key_word_tagged(char *part1, char *part2, char *tag);
void set_key_text(char *group, char *key, char *value);


/* the key is a list of numbers, delimeted by any of " \t,;". Outlist is allocated and
 filled; returns the element count. */
int get_key_float_list(char *part1, char * part2, double **outlist);
int get_key_float_list_tagged(char *part1, char * part2, char *tag, double **outlist);

void impute(char **tag, char **active_tab);
void text_in();

void recodes(char **key, char** tag, char **outstring, char **intable); //just the recode string
void make_recode_view(char **tag, char **first_or_last);//query to generate the view

void do_preedits(char **datatab);

/* Once you've made your filled_tab, you'll want an image of one draw or another.
   */
void check_out_impute(char **origin, char **dest, int *imputation_number, char **subset);
void check_in_impute(char *origin, int serialno, int imputation_number);


//convert a name like "normal" to a model, like apop_normal.
apop_model tea_get_model_by_name(char *name);
