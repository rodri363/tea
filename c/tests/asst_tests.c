#include "tea.h"

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
    "    output table: d \n "
    "    overwrite: y \n "
    "} \n "
    " \n\
    fields { \n\
    one 1-4 \n\
    two 1-3 \n\
    }  "
    "checks{\n\
    one=2 => one=4  \n\
    \n\
    one =1  and two = 2 }\n"
    "impute { \
    input table: d \n\
    models { \n\
      one { method: normal} \n\
      two { method: normal} }}"
    );

    read_spec(&specname, &db_dummy);
    text_in();
    char *d="d";
    impute(NULL, &d);

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
                "age <50 and sex='f'} \n"
                "impute{ input table: data \n"
                "models{ \n"
                "age { method: normal } \n" 
                "sex { method: hot deck } \n"
                "}}");

    write_a_file("indata", 
        "age|sex\n"
        "23|m\n"
        "83|m\n"
        "|\n"
        "3|f\n"
        "83|f\n"
        "|f\n"
        "83|");

    read_spec(&specname, &db_dummy);
    text_in();
    assert(apop_query_to_float("select count(*) from data where age is null") == 2);
    assert(apop_query_to_float("select count(*) from data where sex is null") == 2);
    char *d="data";
    impute(NULL, &d);


    //this is not just like the R test:
    apop_data_show(checkData(apop_query_to_text("select * from data")));
    apop_db_close();
    foreach(s, "spec", "t.db", "indata"){ remove(*s); }
}


void tea_c_tests(){
    group_recode_test();
    recode_test();
    just_like_the_R_test();
    snowman_test();
}
