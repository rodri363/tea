#include <apop.h>

/*extend a string. this prevents a minor leak you'd get if you did
 asprintf(&q, "%s is a teapot.", q);
 q may be NULL, which prints the string "null", so use the little macro below when using this function.
*/
void xprintf(char **q, char *format, ...){ 
    va_list ap; 
    char *r = *q; 
    va_start(ap, format); 
    vasprintf(q, format, ap);
    va_end(ap);
    free(r);
}

#define XN(in) ((in) ? (in) : "")

/* The variable list in the regressions are really just SQL select statements. A statement
   like "select age, race from data_table" is valid SQL, so "age, race" is valid for the
   vars part of the model specification.

   SQL is pretty restrictive, so we've added a few additional tricks. Many systems, ours
   included, will include standard math functions, so given age, feel free to try any of:
    pow(age,2), sqrt(age), exp(age), ln(age), ran()
    --Note that we square using the power function
    --get a random number via ran(). 
    
    These functions are valid anywhere that TEA wants an SQL variable list. In other
    contexts, you may get use from stddev(age), var(age), skew(age), or kurtosis(age).

    There are two more things that make sense only in the context of regression: the
    cross of two terms, and the generation of dummy variables.

Crosses: if you have data with values like (white, black) and (male, female), then 
   vars: race||sex
   will produce values like white|male, white|female, black|male, black|female.
   Multiple crossing, like "race | sex | county", is fine
   By analogy to controlled experiment design, this is typically called an interaction term.

Dummies: Regression on text categories makes no sense: you need to convert the text to
    dummy variables for them to be understood. In a nod to what surveys typically look
    like, every variable you specify gets converted to a factor unless you specify
    otherwise.  Put a # before numeric data that should not be a dummy. E.g.,
    vars: #age, county, race|sex
*/


/* OK, I'm preprocessing SQL. There are two things we're looking for:

   A||B == the 'interaction' between A and B. SQLite uses this form, so I can pass A||B
   through, although I want to add an "as A_B" clause.
           Be sure to accept A||B||C.
    #A == Treat this as a dummy variable. query it, then blow it up using apop_text_to_factors.
            Be sure to accept #A||B||C.
*/
char *process_string(char *inquery, char **typestring){//in which we lament C's lousy string processing
    char *string_so_far=NULL;
    apop_data *clauses_d;
    apop_regex(inquery, "([^,]+)(,|\n|$)", &clauses_d);  //hey---verify that there's a \n at the end of input.
    char ***clauses = clauses_d->text;
    for (int i=0; i < clauses_d->textsize[0]; i++){
        int done=0;
        char *name=NULL, *cat=NULL, *octo;
        if ((octo=strchr(clauses[i][0], '#'))){ //It's numeric. cut the # and move on. Tiny memory leak.
            clauses[i][0] = octo+1;
            xprintf(typestring, "%sm", XN(*typestring));
            xprintf(&string_so_far, "%s%s ", XN(string_so_far),clauses[i][0]);
        } else {
            apop_data *capture=NULL; int ct=0;
            // ||==interaction terms. Really just forming a nice name and fixing things for mySQL.
            if (apop_regex(clauses[i][0], "([^|]+)(\\|\\||$)", &capture)){
                ct=capture->textsize[1];capture->textsize[1]=1;//just first col. for paste
                char *pipes="||'|'||"; //constants
                if (!apop_opts.db_engine || apop_opts.db_engine=='s')
                    cat = apop_text_paste(capture, .between=pipes);
                else if (apop_opts.db_engine=='m'){
                    for (int i =0 ; i < capture->textsize[0]; i++){
                        if (i==0)
                            cat = strdup(clauses[i][0]);
                        else
                            xprintf(&cat, "concat(concat(%s,'|'), %s)", XN(cat), capture->text[i][0]);
                    }
                }
                name = apop_text_paste(capture, .between="X");
                done++;
                xprintf(&string_so_far, "%s%s ", XN(string_so_far), XN(cat));
            }
            xprintf(typestring, "%st", XN(*typestring));
            capture->textsize[1]=ct;
            apop_data_free(capture);
        }
        if (name)
            xprintf(&string_so_far, "%s as %s ", string_so_far, name);
        xprintf(&string_so_far, "%s%s ", string_so_far, clauses[i][1]); //the comma, if any
        free(name); name=NULL;
    }
    printf("Q=%s\n",string_so_far);
    printf("typestring=%s\n", *typestring);
    return string_so_far;
}


//Most of a test. I should add some assertions in there...
void inspect_string_processing(){
    apop_query("create table t(age, race, sex);"
               "insert into t values(10, 'A', 'M');"
               "insert into t values(20, 'A', 'F');"
               "insert into t values(30, 'B', 'F');"
               "insert into t values(40, 'B', 'F');"
               "insert into t values(40, 'B', 'M');"
    );
    char *typestring=NULL;
    char *q = process_string("#age", &typestring);
    apop_data_show(apop_query_to_mixed_data(typestring, "select %s from t", q));
    free(q); free(typestring); typestring=NULL;
    q = process_string("#age, race", &typestring);
    apop_data_show(apop_query_to_mixed_data(typestring, "select %s from t", q));
    free(q); free(typestring); typestring=NULL;
    q = process_string("#age, sex||race", &typestring);
    apop_data *data;
    apop_data_show(data=apop_query_to_mixed_data(typestring, "select %s from t", q));
    for(int i=0; i< data->textsize[1]; i++)
        apop_data_to_dummies(data, i, .append='y');
    /*apop_data_show(data);
    apop_model_print(apop_estimate(data, apop_ols));*/
    free(q); free(typestring); typestring=NULL;
}
