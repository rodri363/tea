#include <apop.h>

typedef struct {
    apop_data *base_data, *fill_ins; 
    apop_data *(*stat)(apop_data *); 
    char const *row_name, *col_name, *value_name, *imputation_name;
} multiple_imputation_variance_t;

apop_data* multiple_imputation_variance_base(multiple_imputation_variance_t in);

#define multiple_imputation_variance(...) \
    multiple_imputation_variance_base((multiple_imputation_variance_t){ \
    .row_name = "row", .col_name="col", .value_name="value", .imputation_name="imputation", __VA_ARGS__});
