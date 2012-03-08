/** \file discrete.h */
#include <apop.h>

typedef struct{ //redundant with the .y file
    char *name;
    double weight;
    int last_query;
    char type;
} used_var_t;
extern used_var_t *used_vars;

typedef struct{
	char *clause;
	used_var_t *vars_used;
	int var_ct;
} edit_t;
extern edit_t *edit_list;
extern int query_ct;

void write_edit_candidate(int id, int col, int val);

extern apop_data *edit_grid;
extern int *find_b, *find_e, *optionct, verbose, nflds, edit_ct, total_var_ct, total_option_ct;

int ri_from_ext(char const *varname, char const* ext_val);
char * ext_from_ri(char const *varname, int const ri_val);
int pull_index(char const *in_name);

void xprintf(char **q, char *format, ...); //impute/parse_sql.c
#define XN(in) ((in) ? (in) : "")  //same.

void begin_transaction();
void commit_transaction();


apop_data * get_variables_to_impute(char *tag); //impute/impute.c
