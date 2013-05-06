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
 
     char *key_check1, *key_check2, *key_check3, *key_check4, *key_check5, *key_check6;

     read_spec(&spec1, &db_dummy);
     asprintf(&key_check1, "impute/min group size");
     asprintf(&key_check2, "impute/draw count");
     asprintf(&key_check3, "impute/seed");
     asprintf(&key_check4, "impute/categories");
     asprintf(&key_check5, "impute/categories/CATAGE");
     asprintf(&key_check6, "impute/categories/SEX");

     apop_data *spec1_keys1 = apop_query_to_text("select * from keys where key like "
             "'impute/m%%'");
     printf("spec1_keys1->text[0][0] is given by: %s.\n", spec1_keys1->text[0][0]);
     assert(!strcmp(key_check1, spec1_keys1->text[0][0]));


     apop_data *spec1_keys2 = apop_query_to_text("select * from keys where key like "
             "'impute/d%%'");
     printf("spec1_keys2->text[0][0] is given by: %s.\n", spec1_keys2->text[0][0]);
     assert(!strcmp(key_check2, spec1_keys2->text[0][0]));


     apop_data *spec1_keys3 = apop_query_to_text("select * from keys where key like "
             "'impute/s%%'");
     printf("spec1_keys3->text[0][0] is given by: %s.\n", spec1_keys3->text[0][0]);
     assert(!strcmp(key_check3, spec1_keys3->text[0][0]));

     
     apop_data *spec1_keys4 = apop_query_to_text("select * from keys where key like "
             "'impute/c%%'");
     printf("spec1_keys4->text[0][0] is given by: %s.\n", spec1_keys4->text[0][0]);
     printf("spec1_keys4->text[1][0] is given by: %s.\n", spec1_keys4->text[2][0]);
     printf("spec1_keys4->text[2][0] is given by: %s.\n", spec1_keys4->text[3][0]);
     assert(!strcmp(key_check4, spec1_keys4->text[0][0]));
     assert(!strcmp(key_check5, spec1_keys4->text[2][0]));
     assert(!strcmp(key_check5, spec1_keys4->text[3][0]));
     
      

     //read_spec(&spec2, &db_dummy);

     //read_spec(&spec3, &db_dummy);
     
     //read_spec(&spec4, &db_dummy);

}
