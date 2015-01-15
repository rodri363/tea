#include <apop.h>
#include <stdbool.h>

/*
This is an EM algorithm to fill in a distribution using both full and partial records. It is cell-based; our goal is to assign a complete-data cell probability to every cell. 

Send the function a data set to be completed and an optional tolerance; it will return a completed data set. E.g.:

apop_data *complete = em_weight(data_with_NaNs, 1e-3);
apop_data *complete = em_weight(data_with_NaNs);

If you don't provide a tolerance, the EM algorithm stops when the sum of changes in weights from one iteration to the next is 1e-5.

Initial data need not have any complete records; see below.

1. Start with initial cell probabilities, probably based on the complete data, but see below.

2. For each observation with incomplete data, distribute its weight over all cells that are compatible with the cell, based on the current cell weights. E.g., given the observation (3, NaN) with weight 8, and probabilities π(3, 1)=.25 and π(3,2)=.75, we'd assign 2 units to (3, 1) and six to (3, 2). Values in other rows are irrelevant; we know this observation is somewhere on row 3.

3. Recalculate the overall cell weights, given the redistribution of all partially missing data.

4. Cycle between 2 & 3 until the cell weights converge.

Implementation is made easier because cells are represented as rows in an apop_data
set, with the weights vector set to the appropriate cell probability or count. 
∙ Copy all nonmissing data to the new candidate set.
∙ For each row that has partially missing data:
    ∙ Cull the set of weights down to remove all rows that 
      don't match some nonmissing aspect of the row. All the 
      program has to do at this point is set incompatible 
      rows' weights to zero.
    ∙ Renormalize the weights to sum to one; multiply by the 
      observation's weight. We've now reallocated the row 
      weights as per the last iteration's candidate weights.
∙ Join any redundant columns; renormalize weights to sum to one.

Repeat until convergence.


Setting the prior
-----------------

The initial values of the πs can be interpreted as the prior parameters for a
Dirichlet distribution. As with any prior, it is somewhat arbitrary as to what the
priors should be.

The textbook treatment prefers to use the complete data as the prior. Being that this is custom, I follow it. Here is the textbook test set:


y1 | y2 | weights
1 | 1|  100
1 | 2| 50 
2|1| 75
2|2| 75
1|Nan|30
2|Nan|60
NaN|1|28
NaN|2|60

In this case, the initial values of π consist entirely of the first four rows, normalized to sum to one.

What if there are no complete values?

y1 | y2 | weights
1|Nan|30
2|Nan|60
NaN|1|28
NaN|2|60

In this case, the sensible thing to do is to use the cross of the available data:

y1 | y2 | weights
1|1|30*28
1|2|30*60
2|1|60*28
2|2|60*60

Thus, in the case of no data, we need to generate new rows, such as (1, 2) from (1, NaN) and (NaN, 2).


There are also intermediate cases, e.g.

y1 | y2 | weights
1|Nan| 30
NaN|1| 28
NaN|2| 60
1 | 1| 100

In this case, we retain 100 as the weight for (1, 1), ignoring the partial observations (1, NaN) and (NaN, 1). But we do want to generate the observation (1, 2), because otherwise the 60 observations  with y2=2 would have nowhere to go.



Here are the rules: 

For each observation E, make a copy of the original data set, then cull away incompatible rows and potentially add new rows. For each row C in the copy set:

--If a field has a different not-NaN value in E and a C, the rows are incompatible; set C's weight to zero.

--If a field in C is NaN but has a not-NaN value in E, then set the field in C to the value given in E.

--If C has NaNs, and none get filled in, then this row either has less information than E or is E itself. In either case, set C's weight to zero.

--If E has no NaNs and is compatible with C (i.e., it has nonzero weight after all of the above), then set has_complete_compatible_data=true.

--If E has NaNs, and has_compatible_data=true, then set E's weight to zero. In the algorithm, this will require a second for loop running through the copy.


This set of rules accommodates the examples above, using only the complete data if there is enough of it, using the cross-product of the missing data if that is all that is available, and using a combination of the two when appropriate.
*/
#define em_weight(...) em_weight_base((em_weight_s){__VA_ARGS__})

typedef struct {
    apop_data const *d;
    double tolerance;
} em_weight_s;

apop_data *em_weight_base(em_weight_s in);
