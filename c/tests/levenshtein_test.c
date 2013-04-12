#include "internal.h" 

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
        consistency_check(d->names->text, d->text[i],
        &size_as_int,
        &passfail, &seven,
        &fails_edits, NULL);

        assert(((d->text[i][0][0]=='2' || (d->text[i][0][0]=='1' && d->text[i][1][0]=='2')) && \
                 fails_edits) || !fails_edits);
    }
}


#ifdef TESTING
int main(){
    #define close_enough(a, b) assert(fabs(a-b) < 1e-2)
    #define assert_false(a) assert(!a)
    #define equals(a, b) assert(a == b)

   void snowman_test(){
        char *specname = "snowman.spec";
         write_one_two_dataset();

         write_a_file(specname,"\n"
           "database: d.db\n"
           "\n"
           "input {\n"
           "    input file: onetwo.data\n"
           "    output table: d \n "
           "    overwrite: y \n "
           "} \n "
           " \n\
           fields { \n\
           one: int 1-4 \n\
           two: int 1-3 \n\
           }  "
           "checks{\n\
           one=2 => one=4  \n\
           \n\
           one =1  and two = 2 }\n"
           "impute [one]{ \
           input table: d \n\
           method: normal \n\
           output vars:one} \n"
           "impute [two]{ \
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


}
#endif
