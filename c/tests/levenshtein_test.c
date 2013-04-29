#include "internal.h" 

#define foreach(s, ...) for (char **s = (char*[]){__VA_ARGS__, NULL}; *s; s=&s[1])


#define close_enough(a, b) assert(fabs(a-b) < 1e-2)
#define assert_false(a) assert(!a)
#define assert_equals(a, b) assert(a == b)

/** This function creates a series of spec files with typos in the standard 
  * keys and checks to see whether the number of typos returned by \
  * check_levenshtein_distances() (run through read_spec()) is as expected.
  */
void levenshtein_tests(){

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

     write_a_file(spec1,
     "\n"
     "database: demo.db\n"
     "\n"
     "input {\n"
     "    input file: dc_pums_08.csv\n"
     "    output table: dc \n "
     "    overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real}"
     );

     write_a_file(spec2,
     "\n"
     "database: demo.db\n"
     "\n"
     "input {\n"
     "inpt file: dc_pums_08.csv\n"
     "output table: dc \n "
     "overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "AGEP: int 0-116} \n"
     "\n"
     "recodes{\n"
     "CATAGE {\n"
     "1|AGEP between 0 and 15\n"
     "2|AGEP between 16 and 64\n"
     "3|} \n"
     "}"
     );

     write_a_file(spec3,
     "\n"
     "databassee: demo.db\n"
     "\n"
     "inptu{\n"
     "input file: dc_pums_08.csv\n"
     "outpt table: dc \n "
     "overwrite: y} \n "
     " \n"
     "fields { \n"
     "SCHL: int 0-24 \n"
     "WAGP: real}"
     );


     write_a_file(spec4,
     "\n"
     "database: demo.db\n"
     "\n"
     "input {\n"
     "input file: dc_pums_08.csv\n"
     "output table: dc \n "
     "overwrite: y \n "
     "} \n "
     " \n"
     "fields { \n"
     "AGEP: int 0-116} \n"
     "\n"
     "checkkkks {\n"
     "AGEP < 0 \n"
     "\n"
     "AGEP > 95 => AGEP = 95 }"
     );


     write_a_file(spec5,
     "\n"
     "impute {\n"
     "inupt tabl: dc \n"
     "draw counn: 3\n"
     "seed:2332\n"
     "methd: hot deck\n}"
     );

    
char *db_dummy;

     /* Will have to re-adjust this test. Right now, read_spec doesn't support
        returning the number of typos so this code will not execute.
     */
     read_spec(&spec1, &db_dummy);
     int num_typos_spec1 = get_num_typos();
     printf("There were %d typos when reading %s\n", num_typos_spec1, spec1);
     //assert_equals(get_num_typos(), 0);

     read_spec(&spec2, &db_dummy);
     int num_typos_spec2 = get_num_typos();
     printf("There were %d typos when reading %s\n", num_typos_spec2, spec2);
     //assert_equals(get_num_typos(), 1);

     read_spec(&spec3, &db_dummy);
     int num_typos_spec3 = get_num_typos();
     printf("There were %d typos when reading %s\n", num_typos_spec3, spec3);
     //assert_equals(get_num_typos(), 5);
     
     read_spec(&spec4, &db_dummy);
     int num_typos_spec4 = get_num_typos();
     printf("There were %d typos when reading %s\n", num_typos_spec4, spec4);
     //assert_equals(get_num_typos(), 3);

     read_spec(&spec5, &db_dummy);
     int num_typos_spec5 = get_num_typos();
     printf("There were %d typos when reading %s\n", num_typos_spec5, spec5);
     //assert_equals(get_num_typos(), 6);
}
