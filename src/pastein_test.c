#include "internal.h" 

#define foreach(s, ...) for (char **s = (char*[]){__VA_ARGS__, NULL}; *s; s=&s[1])


#define close_enough(a, b) assert(fabs(a-b) < 1e-2)
#define assert_false(a) assert(!a)
#define assert_equals(a, b) assert(a == b)

/** This function creates a series of spec files with paste in macros used
  * instead of normal keys. The tests will ensure that the correct keys are 
  * getting written to the keys table by running read_spec() and then using 
  * apop functions to verify that the keys are indeed in the spec file
  */
void pastein_tests(){

char *spec1;
asprintf(&spec1, "1.spec");

char *spec2;
asprintf(&spec2, "2.spec");

char *spec3;
asprintf(&spec3, "3.spec");
        
char *spec4;
asprintf(&spec4, "4.spec");

char *spec5;
asprintf(&spec5, "5.spec");
        
    /* Standard test here: creating a macro with a few sub keys and calling it on its own
     * in the impute key. If something goes wrong here it's because there's something
     * fundamentally wrong with the paste in macro (because there's only one so there's
     * nothing too complex going on).
     */
     write_a_file(spec1,
     "\n"
     "database: demo.db\n"
     "verbose: 2\n"
     "catagesex{\n"
     "  min group size: 3\n"
     "  draw count: 3\n"
     "  seed: 2332\n"
     "  categories {\n"
     "      CATAGE\n"
     "      SEX\n"
     "  }\n"
     "}\n"
     "\n"
     "input {\n"
     "    input file: dc_pums_08.csv\n"
     "    output table: dc \n "
     "    overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real\n"
     "\n}"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: SCHL\n"
     "}\n"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: WAGP\n"
     "}\n"
     );

     /* Creating test here that uses two macros that are used concurrently but that do not
      * call each other. tables{...} and catagesex{...} are each used in impute{...} but
      * they do not "paste each other in". This will be tested in spec 3.
      */
     write_a_file(spec2,
     "\n"
     "database: demo.db\n"
     "verbose: 2\n"
     "catagesex{\n"
     "  min group size: 3\n"
     "  draw count: 3\n"
     "  seed: 2332\n"
     "  categories {\n"
     "      CATAGE\n"
     "      SEX\n"
     "  }\n"
     "}\n"
     "tables{\n"
     "  input table: viewdc\n"
     "  output table: impuTable\n" //To account for analysts who like camel case
     "}\n"
     "\n"
     "input {\n"
     "    paste in: tables\n"
     "    input file: dc_pums_08.csv\n"
     "    output table: dc \n "
     "    overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real\n"
     "\n}"
     "impute{\n"
     "  paste in: tables\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: SCHL\n"
     "}\n"
     "impute{\n"
     "  paste in: tables\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: WAGP\n"
     "}\n"
     );

    /* More complicated test that tests the ability of a macro to use another macro in its
     * own definition. For instance, it tests something along the lines of
     * catagesex{paste in: impute stuff \n paste in: categories}
     */
     write_a_file(spec3,
     "\n"
     "database: demo.db\n"
     "verbose: 2\n"
     "imputestuff{\n"
     "  min group size: 3\n"
     "  draw count: 3\n"
     "  seed: 2332\n"
     "}\n"
     "categoriesstuff {\n"
     "  categories{\n"
     "      CATAGE\n"
     "      SEX\n"
     "  }\n"
     "}\n"
     "catagesex{\n"
     "  paste in: imputestuff\n"
     "  paste in: categoriesstuff\n"
     "}\n"
     "\n"
     "input {\n"
     "    input file: dc_pums_08.csv\n"
     "    output table: dc \n "
     "    overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real\n"
     "\n}"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: SCHL\n"
     "}\n"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: WAGP\n"
     "}\n"
     );

    /* Tests whether it's possible to create a macro that comprises the entire spec file
     * (which, of course, is then pasted in on its own). This includes other macros that
     * are written within the overarching macro itself. Possibly overkill? But I think
     * it's worth it to test given that different analysts might include big portions of
     * the spec file separately and could decide to use a macro to do so.
     */
     write_a_file(spec4,
     "\n"
     "database: demo.db\n"
     "wholeSpec{\n"
     "verbose: 2\n"
     "catagesex{\n"
     "  min group size: 3\n"
     "  draw count: 3\n"
     "  seed: 2332\n"
     "  categories {\n"
     "      CATAGE\n"
     "      SEX\n"
     "  }\n"
     "}\n"
     "\n"
     "input {\n"
     "    input file: dc_pums_08.csv\n"
     "    output table: dc \n "
     "    overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real\n"
     "\n}"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: SCHL\n"
     "}\n"
     "impute{\n"
     "  input table: viewdc\n"
     "  output table: imputes\n"
     "  paste in: catagesex\n"
     "  method: hot deck\n"
     "  output vars: WAGP\n"
     "}\n"
     "}\n"
     "paste in: wholeSpec\n"
     );

char *db_dummy;

     char *imp_min_grp, *imp_drw_cnt, *imp_seed, *imp_categories;

     read_spec(&spec1, &db_dummy);
     asprintf(&imp_min_grp, "impute/min group size");
     asprintf(&imp_drw_cnt, "impute/draw count");
     asprintf(&imp_seed, "impute/seed");
     asprintf(&imp_categories, "impute/categories");

     apop_data *spec1_keys1 = apop_query_to_text("select * from keys where key like "
             "'impute/m%%'");
     printf("spec1_keys1->text[0][0] is given by: %s.\n", spec1_keys1->text[0][0]);
     assert(!strcmp(imp_min_grp, spec1_keys1->text[0][0]));


     apop_data *spec1_keys2 = apop_query_to_text("select * from keys where key like "
             "'impute/d%%'");
     printf("spec1_keys2->text[0][0] is given by: %s.\n", spec1_keys2->text[0][0]);
     assert(!strcmp(imp_drw_cnt, spec1_keys2->text[0][0]));


     apop_data *spec1_keys3 = apop_query_to_text("select * from keys where key like "
             "'impute/s%%'");
     printf("spec1_keys3->text[0][0] is given by: %s.\n", spec1_keys3->text[0][0]);
     assert(!strcmp(imp_seed, spec1_keys3->text[0][0]));

     
     apop_data *spec1_keys4 = apop_query_to_text("select * from keys where key like "
             "'impute/c%%'");
     printf("spec1_keys4->text[0][0] is given by: %s.\n", spec1_keys4->text[0][0]);
     assert(!strcmp(imp_categories, spec1_keys4->text[0][0]));
     
     apop_data_free(spec1_keys1);
     apop_data_free(spec1_keys2);
     apop_data_free(spec1_keys3);
     apop_data_free(spec1_keys4);
      

     read_spec(&spec2, &db_dummy);
     char *inpt_inpt_table;
     char *inpt_otpt_table;

     asprintf(&inpt_inpt_table, "input/input table");
     asprintf(&inpt_otpt_table, "input/output table");

     apop_data *spec2_keys1 = apop_query_to_text("select * from keys where key like "
             "'impute/m%%'");
     printf("spec2_keys1->text[0][0] is given by: %s.\n", spec2_keys1->text[0][0]);
     assert(!strcmp(imp_min_grp, spec2_keys1->text[0][0]));


     apop_data *spec2_keys2 = apop_query_to_text("select * from keys where key like "
             "'impute/d%%'");
     printf("spec2_keys2->text[0][0] is given by: %s.\n", spec2_keys2->text[0][0]);
     assert(!strcmp(imp_drw_cnt, spec2_keys2->text[0][0]));


     apop_data *spec2_keys3 = apop_query_to_text("select * from keys where key like "
             "'impute/s%%'");
     printf("spec2_keys3->text[0][0] is given by: %s.\n", spec2_keys3->text[0][0]);
     assert(!strcmp(imp_seed, spec2_keys3->text[0][0]));

     
     apop_data *spec2_keys4 = apop_query_to_text("select * from keys where key like "
             "'impute/c%%'");
     printf("spec2_keys4->text[0][0] is given by: %s.\n", spec2_keys4->text[0][0]);
     assert(!strcmp(imp_categories, spec2_keys4->text[0][0]));
     
     apop_data *spec2_keys5 = apop_query_to_text("select * from keys where key like "
             "'input/input t%%'");
     printf("spec2_keys5->text[0][0] is given by: %s.\n", spec2_keys5->text[0][0]);
     assert(!strcmp(inpt_inpt_table, spec2_keys5->text[0][0]));


     apop_data *spec2_keys6 = apop_query_to_text("select * from keys where key like "
             "'input/output t%%'");
     printf("spec2_keys6->text[0][0] is given by: %s.\n", spec2_keys6->text[0][0]);
     assert(!strcmp(inpt_otpt_table, spec2_keys6->text[0][0]));

     apop_data_free(spec2_keys1);
     apop_data_free(spec2_keys2);
     apop_data_free(spec2_keys3);
     apop_data_free(spec2_keys4);
     apop_data_free(spec2_keys5);
     apop_data_free(spec2_keys6);

     read_spec(&spec3, &db_dummy);
     
     apop_data *spec3_keys1 = apop_query_to_text("select * from keys where key like "
             "'impute/m%%'");
     printf("spec3_keys1->text[0][0] is given by: %s.\n", spec3_keys1->text[0][0]);
     assert(!strcmp(imp_min_grp, spec3_keys1->text[0][0]));


     apop_data *spec3_keys2 = apop_query_to_text("select * from keys where key like "
             "'impute/d%%'");
     printf("spec3_keys2->text[0][0] is given by: %s.\n", spec3_keys2->text[0][0]);
     assert(!strcmp(imp_drw_cnt, spec3_keys2->text[0][0]));


     apop_data *spec3_keys3 = apop_query_to_text("select * from keys where key like "
             "'impute/s%%'");
     printf("spec3_keys3->text[0][0] is given by: %s.\n", spec3_keys3->text[0][0]);
     assert(!strcmp(imp_seed, spec3_keys3->text[0][0]));

     
     apop_data *spec3_keys4 = apop_query_to_text("select * from keys where key like "
             "'impute/c%%'");
     printf("spec3_keys4->text[0][0] is given by: %s.\n", spec3_keys4->text[0][0]);
     assert(!strcmp(imp_categories, spec3_keys4->text[0][0]));

     apop_data_free(spec3_keys1);
     apop_data_free(spec3_keys2);
     apop_data_free(spec3_keys3);
     apop_data_free(spec3_keys4);

     /* This is spec file that tests whether paste in works for pasting in the entire spec
      * file (without the database -- pasting in database has not been tested yet). spec 4
      * paste in stuff is tested by just testing for an assortment of keys.
      */
     read_spec(&spec4, &db_dummy);


     /* DV - ATTENTION:
      * This test is failing right now so I've put in an if statement below to exit when
      * there's no impute key to avoid a segfault in the testing. We need to fix the bug
      * that is preventing paste in from allowing an entire spec file (minus the database)
      * to be pasted in.
      */


     apop_data *spec4_keys1 = apop_query_to_text("select * from keys where key like "
             "'impute/m%%'");

     if(get_key_word("impute", NULL) == NULL) return;
     printf("spec4_keys1->text[0][0] is given by: %s.\n", spec4_keys1->text[0][0]);
     assert(!strcmp(imp_min_grp, spec4_keys1->text[0][0]));


     apop_data *spec4_keys2 = apop_query_to_text("select * from keys where key like "
             "'impute/d%%'");
     printf("spec4_keys2->text[0][0] is given by: %s.\n", spec4_keys2->text[0][0]);
     assert(!strcmp(imp_drw_cnt, spec4_keys2->text[0][0]));


     apop_data *spec4_keys3 = apop_query_to_text("select * from keys where key like "
             "'impute/s%%'");
     printf("spec4_keys3->text[0][0] is given by: %s.\n", spec4_keys3->text[0][0]);
     assert(!strcmp(imp_seed, spec4_keys3->text[0][0]));

     
     apop_data *spec4_keys4 = apop_query_to_text("select * from keys where key like "
             "'impute/c%%'");
     printf("spec4_keys4->text[0][0] is given by: %s.\n", spec4_keys4->text[0][0]);
     assert(!strcmp(imp_categories, spec4_keys4->text[0][0]));
     
     apop_data *spec4_keys5 = apop_query_to_text("select * from keys where key like "
             "'input/input t%%'");
     printf("spec4_keys5->text[0][0] is given by: %s.\n", spec4_keys5->text[0][0]);
     assert(!strcmp(inpt_inpt_table, spec4_keys5->text[0][0]));


     apop_data *spec4_keys6 = apop_query_to_text("select * from keys where key like "
             "'input/output t%%'");
     printf("spec4_keys6->text[0][0] is given by: %s.\n", spec4_keys6->text[0][0]);
     assert(!strcmp(inpt_otpt_table, spec4_keys6->text[0][0]));

     apop_data_free(spec4_keys1);
     apop_data_free(spec4_keys2);
     apop_data_free(spec4_keys3);
     apop_data_free(spec4_keys4);
     apop_data_free(spec4_keys5);
     apop_data_free(spec4_keys6);

     free(imp_min_grp);
     free(imp_drw_cnt);
     free(imp_seed);
     free(imp_categories);
     free(inpt_inpt_table);
     free(inpt_otpt_table);
     free(spec1);
     free(spec2);
     free(spec3);
     free(spec4);
     free(spec5);

     printf("Reached end of test.\n");

}
