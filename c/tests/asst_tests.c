#include "internal.h"

//Just for the fun of it.
#define foreach(s, ...) for (char **s = (char*[]){__VA_ARGS__, NULL}; *s; s=&s[1])

void write_a_file(char *name, char *onestring){
    FILE *f = fopen(name, "w");
    fprintf(f, "%s", onestring);
    fclose(f);
}

char *db_dummy;

void test_consistency(){
    int seven=7, fails_edits;
    char const *passfail="passfail";
    apop_data *d = apop_query_to_text("select * from d");
    int size_as_int = d->textsize[1];
    for (int i=0; i< d->textsize[0]; i++){
        consistency_check(d->names->text, d->text[i], &size_as_int,
                &passfail, &seven, &fails_edits, NULL
                );
        assert(((d->text[i][0][0]=='2' || (d->text[i][0][0]=='1' && d->text[i][1][0]=='2')) 
                            && fails_edits)
                 || !fails_edits);
    }
}

//Generates a two-column list of elements, write to 'onetwo.data'.
void write_one_two_dataset(){
    write_a_file("onetwo.data", 
    "\"one\" |two   \n"
    "1\t1           \n"
    "1|2            \n"
    "1| 3           \n"
    "2 | 1          \n"
    "2| 2           \n"
    "3|2            \n"
    "2|	2           \n"
    "3|1            \n"
    "1 | 1          \n"
    "3|2            \n"
    "2 |2           \n"
    "3|3            \n"
    );
}

void snowman_test(){
    char *specname = "snowman.spec";
    write_one_two_dataset();

    write_a_file(specname,
    "\n"
    "database: d.db\n"
    "\n"
    "input {\n"
    "    input file: onetwo.data\n"
    "    output table: d \n\ "
    "    overwrite: y}\n"
    " \n\
    fields { \n\
    one: int 1-4 \n\
    two: int 1-3 \n\
    }  "
    "checks{\n\
    one=2 => one=4  \n\
    \n\
    one =1  and two = 2 }\n"
    "impute [one]{ \n\
    input table: d \n\
      method: normal \n\
      output vars:one} \n"
    "impute [two]{ \n\
    input table: d \n\
      method: normal \n\
      output vars:two}"
    );

    read_spec(&specname, &db_dummy);
    text_in();
    char *d="d";
    impute(&d);

    test_consistency();
    apop_db_close();
    foreach(f, "snowman.spec", "snowman.data", "d.db")
        remove(*f);
}

void recode_test(){
    char *specname = "recodes.spec";
    write_one_two_dataset();

    write_a_file(specname,
"           database: test.db                     \n"
"           input {\n"
"               input file: onetwo.data\n"
"               output table: d \n "
"               overwrite: y \n "
"}     \n "
"\n"
"            recodes [first] {               \n"
"                abc {                       \n"
"                    A | one =1 and two=1    \n"
"                    B | one = 1 and two=2   \n"
"                    C |                     \n"
"                }                           \n"
"                                           \n"
"                def {                       \n"
"                    D | one=2 and two=1     \n"
"                    E | one=2 and two=2     \n"
"                    F | one=2 and two=3     \n"
"                    G |                     \n"
"                }                           \n"
"            }                               \n"
"\n"
"            recodes [second] {              \n"
"                L21 {                       \n"
"                    L1 | abc = 'A'          \n"
"                    L2 | abc = 'B'          \n"
"                    L3 |                    \n"
"                }                           \n"
"            }                               \n"
            );

    read_spec(&specname, &db_dummy);
    text_in();
    assert(apop_query_to_float("select count(*) from viewd where abc=='A'")==2);
    assert(apop_query_to_float("select count(*) from viewd where L21=='L1'")==2);
}

void group_recode_test(){
    char *specname = "recodes.spec";
    write_one_two_dataset();

    write_a_file(specname,
"           database: test.db                       \n"
"           input {                                 \n"
"               input file: onetwo.data             \n"
"               output table: d                     \n "
"               overwrite: y                        \n "
"}                                                  \n "
"                                               \n"
"            group recodes [first] {               \n"
"             group id: one                  \n"
"             twomax: max(two)                       \n"
"             twomin: min(two)                       \n"
"            }                               \n"
            );

    read_spec(&specname, &db_dummy);
    text_in();
    assert(apop_query_to_float("select count(*) from viewd where twomax+0.0==2")==4);
    assert(apop_query_to_float("select count(*) from viewd where twomin+0.0==1")==12);
}

////////////////////////////////

void just_like_the_R_test(){
    char *specname = "spec";
    write_a_file(specname, "database:t.db\n"
                "input { input file: indata \n"
                "output table: data} \n"
                "fields {  \n"
                "age: int 1-100  \n"
                "sex: cat m, f } \n"
                "#sex: 0, 1 } \n"
                "#checks { age > 50 and sex =\"f\" \n"
                "#age <50 and sex=\"m\"} \n"
                "checks { age > 50 and sex ='f' \n"
                "age <50 and sex='f' } \n"
                "impute [a]{ input table: data \n"
                "output vars: age\n"
                "method: normal }\n"
                "impute [b]{ input table: data \n"
                "output vars: sex\n"
                "method: hot deck }\n");

    write_a_file("indata", 
        "age|sex\n"
        "23|m\n"
        "83|m\n"
        "|\n"
        "3|f\n"
        "83|f\n"
        "|f\n"
        "83|\n");

    read_spec(&specname, &db_dummy);
    text_in();
    //assert(apop_query_to_float("select count(*) from data where age is null") == 2);
    //assert(apop_query_to_float("select count(*) from data where sex is null") == 2);
    char *d="data";
    impute(&d);

    char *checkout = "checkout";
    int zero =0;
    check_out_impute(&d, &checkout, &zero, NULL, NULL);

    //this is not just like the R test:
    checkData(apop_query_to_text("select * from %s", checkout));
    apop_db_close();
    foreach(s, "spec", "t.db", "indata"){ remove(*s); }
}

void test_ols(gsl_rng *r){
    apop_model *xlist = apop_model_stack(
                apop_model_stack(
                apop_model_set_parameters(apop_uniform, 1, 1), //a constant.
                apop_model_set_parameters(apop_normal, 2, 1)),
                apop_model_set_parameters(apop_poisson, 2));
    int len=1e3;
    apop_data *observations = apop_data_alloc(len, 3);
    for (int i=0; i< len; i++){
        Apop_row(observations, i, onerow); 
        apop_draw(onerow->data, r, xlist);
    }
    apop_data *betas= apop_data_fill(apop_data_alloc(3), 3, -2, 1);
    apop_data *y = apop_dot(observations, betas);
    observations->vector= y->vector;
    apop_model *olsest = apop_estimate(observations, apop_ols);
    //apop_model_print(olsest);
    assert(fabs(apop_data_get(olsest->parameters, 0, -1) -  3) < 1e-5);
    assert(fabs(apop_data_get(olsest->parameters, 1, -1) - -2) < 1e-5);
    assert(fabs(apop_data_get(olsest->parameters, 2, -1) -  1) < 1e-5);

    //blank, make rowids
    char *str;
    for (int i=0; i< len; i++){
        if (gsl_rng_uniform(r) < .04)
            for (int j=-1; j < 3; j++)
                if (gsl_rng_uniform(r) < 0.5) apop_data_set(observations, i, j, NAN);
        asprintf(&str, "%i", i);
        apop_name_add(observations->names, str, 'r');
    }

    //write to db
    apop_data_add_names(observations, 'c', "1", "norm", "fish");
    apop_data_add_names(observations, 'v', "Y");
    apop_data_prune_columns(observations, "norm", "fish"); //vector "y" always kept.
    sprintf(apop_opts.db_name_column, "id_col");
    apop_db_open("olsdb.db");
    begin_transaction();
    apop_data_print(observations, .output_file="olsdata", .output_type='d');
    commit_transaction();
    //apop_data_show(observations);


    char *specname = "spec";
    write_a_file(specname, "database:olsdb.db\n"
                "id: id_col \n"
                "input { output table: olsdata} \n"
                "fields {  \n"
                "Y: real \n"
                "fish: int \n"
                "norm: real  } \n"
                "impute { input table: olsdata \n"
                "output table: olsfills\n"
                "output vars: norm\n"
                "method: normal }\n"
                "\n"
                "impute { input table: olsdata \n"
                "output vars: fish\n"
                "earlier output table: olsfills\n"
                "output table: olsfills\n"
                "method: poisson }\n"
                "\n"
                "impute { input table: olsdata \n"
                "output vars: Y\n"
                "input vars: norm, fish\n"
                "output table: olsfills\n"
                "earlier output table: olsfills\n"
                "method: ols }\n"
                );

    read_spec(&specname, &db_dummy);
    char *d="olsdata";
    impute(&d);

    
 /*   apop_model *stacked = apop_model_stack(apop_multivariate_normal,
                                           apop_wishart);
    apop_settings_add(stacked, apop_stack, splitpage, "for wishart");

    apop_data_add_page(norm_data, wishart_data, "for wishart");
    apop_model *estimated = apop_estimate(norm_data, stacked);

    apop_model *updated = 
    apop_update(.prior=estimated_stacked, .likelihood=apop_multivariate_normal,
                .data=mvn_appropriate_data);
                */

}

    //generate random data
    //punch holes
    //run
    //check out inpute
    //re-est; compare the reestimated parameters.

    /*
    stacked models: same data set; different data sets?

    with the same data set for estimation, it's e-z.
    with different ones, where're you gonna put `em?
    use a startsat page thing.
    */


void tea_c_tests(){
   gsl_rng *r = apop_rng_alloc(134124);
   test_ols(r);
   test_check_out_impute();
   group_recode_test();
   recode_test();
   just_like_the_R_test();
   snowman_test();
   levenshtein_tests();
}

#ifdef TESTING
int main(){ tea_c_tests(); }
#endif
