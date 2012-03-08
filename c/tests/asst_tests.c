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
    char *passfail="passfail";
    apop_data *d = apop_query_to_text("select * from d");
    for (int i=0; i< d->textsize[0]; i++){
        consistency_check(d->names->text, d->text[i], &d->textsize[1],
                &passfail, &seven, &fails_edits, NULL
                );
        assert(((d->text[i][0][0]=='2' || (d->text[i][0][0]=='1' && d->text[i][1][0]=='2')) 
                            && fails_edits)
                 || !fails_edits);
    }
}

void snowman_test(){
    char *specname = "snowman.spec";
    write_a_file("snowman.data", 
    "\"one\" |two\n"
    "1\t1\n"
    "1|2\n"
    "1| 3\n"
    "2 | 1 \n"
    "2| 2\n"
    "3|2 \n"
    "2|	3 \n"
    "3|1 \n"
    "1 | 1 \n"
    "3|2 \n"
    "2 |2 \n"
    "3|3 \n"
    );

    write_a_file(specname,
    "\n"
    "database: d.db\n"
    "\n"
    "input {\n"
    "    input file: snowman.data\n"
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
                "checks { age > 50 and sex =1 \n"
                "age <50 and sex=1} \n"
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
    apop_db_close();
    foreach(s, "spec", "t.db", "indata"){ remove(*s); }
}


void tea_c_tests(){
    just_like_the_R_test();
    snowman_test();
}
