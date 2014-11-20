#include "internal.h"

char *blank = "";
int negone = -1;

/* This function takes in a list of records and their values in arbitrary order, and produes a list that matches the order in the edit grid.

   You send in a list of strings with the arbitrary-order inputs, and an allocated
   but not filled list for this function to fill (oext_vals = ordered, external values).
   The list of strings is filled with pointers to your arbitrary-order list, so changes
   carry back. If a field is not to be considered in this edit check, then the string
   is given a "".

   Because all of the strings involved are pointers to other locations, when you're done with oext_vals, free the list, but not the strings in the list.
  */
void order_things(char * const * record_in, char *const *record_names, int record_size, char **oext_vals){
    for (int j=0; j< total_var_ct; j++)
        oext_vals[j] = blank;
    for (int i=0; i< record_size; i++)
        for (int j=0; j< total_var_ct; j++)
            if (!strcmp(used_vars[j].name, record_names[i]) && used_vars[j].type != 'r'){
                oext_vals[j] = record_in[i];
                break;
            }
}

void order_things_int(int * ints_in, char *const *record_names, int record_size, int **oint_vals){
    for (int j=0; j< total_var_ct; j++)
        oint_vals[j] = &negone;
    for (int i=0; i< record_size; i++)
        for (int j=0; j< total_var_ct; j++)
            if (!strcmp(used_vars[j].name, record_names[i]) && used_vars[j].type != 'r'){
                oint_vals[j] = &ints_in[i];
                break;
            }
}
