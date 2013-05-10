#include "internal.h"

/* check_levenshtein_distances checks every element in the table of keys 
   for similarity to one of the known keys, which we generated from the 
   in-code documentation. If we find a user key too close (but not exactly
   equal to) to a known key, then warn the user. 

    test_levenshtein_distance, for the test suite, is at the bottom of this file.
   */


#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
 
/** Computes Levenshtein distance between two input strings. Used to check whether 
 *  author of spec file possibly made a typo when typing out an accepted key
 *  by checking whether a user's key is within Min_ham_distance of an accepted 
 *  key.
 *
 *  If num_differences == 0 || num_differences > max_lev_distance then Apop_stopif 
 *  won't get executed above in check_levenshtein_distances. Otherwise, if the keys have at most
 *  max_lev_distance differences then it will.
 */
//cut/pasted/modified from https://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#C

static int levenshtein_distance(char *s1, char *s2) {
    unsigned int s1len, s2len, x, y, lastdiag, olddiag;
    s1len = strlen(s1);
    s2len = strlen(s2);
    if (!s1len) return s2len;
    if (!s2len) return s1len;
    unsigned int column[s1len+1];
    for (y = 1; y <= s1len; y++)
        column[y] = y;
    for (x = 1; x <= s2len; x++) {
        column[0] = x;
        for (y = 1, lastdiag = x-1; y <= s1len; y++) {
            olddiag = column[y];
            column[y] = MIN3(column[y] + 1, column[y-1] + 1, lastdiag + (s1[y-1] == s2[x-1] ? 0 : 1));
            lastdiag = olddiag;
        }
    }
    return(column[s1len]);
}


/** Function gets a list of all of the possible expected keys in the spec file and 
  * calls hamming_distance to find the hamming distance for each of the keys in 
  * userkeys in comparison with the list of acceptable keys. 
  * If hamming_distance > 0 && < 3 then we alert user that they might have meant to 
  * put in the correct key using Apop_stopif
  */

#include "keylist"
int check_levenshtein_distances(int max_lev_distance){
    int typo_counter=0;
    int min_distance;
    char *closest;
    if (!apop_table_exists("keys")) return 0;
    apop_data *userkeys = apop_query_to_text("select key from keys");
    for (int i=0; i < *userkeys->textsize; i++){
        min_distance = 100;
        for (char **keyptr=ok_keys; strlen(*keyptr); keyptr++){
            int ld = levenshtein_distance(*keyptr, *userkeys->text[i]);

            if (ld < min_distance){
                if(ld == 0) {min_distance=0; break;}
                min_distance=ld;
                closest = *keyptr;    
            }
        }
        Apop_stopif(min_distance > 0 && min_distance <= max_lev_distance, typo_counter++ , 0, 
                            "You wrote %s for one of the keys in your spec file. Did you "
                            "mean to write %s?", *userkeys->text[i], closest)
    }
    return typo_counter;
}

void test_levenshtein_distance(){
    char *string_one, *string_two;

    asprintf(&string_one, "impute/recodes");
    asprintf(&string_two, "impute/recdes");

    assert(levenshtein_distance(string_one, string_two) == 1);

    asprintf(&string_one, "input/input table");
    asprintf(&string_two, "niput/input table");

    assert(levenshtein_distance(string_one, string_two) == 2);

    asprintf(&string_one, "database");
    asprintf(&string_two, "database");
    assert(levenshtein_distance(string_one, string_two) == 0); 

    asprintf(&string_one, "mthod");
    asprintf(&string_two, "method");
    assert(levenshtein_distance(string_one, string_two) == 1);

    free(string_one);
    free(string_two);
    printf("Leaving test_levenshtein_distance successfully!\n");
}
