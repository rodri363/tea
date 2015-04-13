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

//If running as a C library without R, warning messages have to be treated differently.
extern _Bool CTea;

#define Tea_stopif(test, returnop, level, ...)      \
    if (CTea)                                       \
        {Apop_stopif(test, returnop, level, __VA_ARGS__);}    \
    else if (test){                                 \
        if (apop_opts.verbose >= level)             \
            Rf_warning(__VA_ARGS__);                \
        returnop;                                   \
    }



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

typedef struct { //info primarily used for writing to the db.
    char const *tabname;
    int draw_number;
    char const *id_col;
    char const *id;
    _Bool autofill;
} tabinfo_s;

//in discrete/consistency.c
/** This function will either return a pass/fail vote as to whether an input record
passes edits, or provide a score counting how often each field caused an edit to fail.

Inputs:
\param oext_values An ordered list of fields to be checked. If a field is NULL here, its edits don't get checked. Use order_things in discrete/ext_to_em.c to put things in the right order.
\param what_you_want One of <tt>"passfail"</tt> or <tt>"failed fields"</tt>.
\param id A record id, only for error reporting.
\param ofailed_fields If you want a pass/fail, NULL. If you want the list of failed fields, a vector of ints to record the failure cout. Use order_things_int to set this up.

\param do_preedits A true/false variable.

Outputs:
\li For the <tt>"failed_fields"</tt> case, I will fill \c ofailed_fields with a count indicating how many edits the
corresponding field failed (and zero if it is OK). If the return value is nonzero, then one of these elements will
be nonzero.
\return 0=passes all edits. If "failed fields", the count of failed fields. If "passfail", something nonzero if there are failed fields.
*/
int consistency_check(char ***oext_values, char const * what_you_want, 
			          int **ofailed_fields, _Bool do_preedits, tabinfo_s);

apop_data *checkData(apop_data *data, _Bool do_preedits, tabinfo_s);
_Bool an_edit(char const *in_tab, char const *out_tab, char const *tag);

//key-getting functions for the C side. Notice that get_text returns an apop_data set.
apop_data * get_key_text(char const *part1, char const *part2);
apop_data * get_key_text_tagged(char const *part1, char const *part2, char const *tag);
double get_key_float(char const *part1, char const * part2);
double get_key_float_tagged(char const *part1, char const * part2, char const *tag);
char* get_key_word(char const *part1, char const *part2);
char* get_key_word_tagged(char const *part1, char const *part2, char const *tag);
void set_key_text(char const *group, char const *key, char const *value);

void impute(char **active_tab, int *autofill);
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
