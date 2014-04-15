/* Unbiased controlled rounding of a fractional matrix

   input   mat - fractional matrix, assumed  0 <= mat <= 1
           imat - storage for an integer matrix of size (nrow+1,ncol+1)
	   rowparity - storage for integer matrix of size (nrow+1,2) for parity info
           nrow - number of rows of mat
	   ncol - number of columns of mat

   output mat - 0,1 rounded version of input

   This file by Rob Creecy, with a few modifications (primarily switching from
   R-dependence to GSL-dependence) by Ben Klemens.

 */
#include <stdlib.h>
#include <math.h>
#include <apop.h> //my lazy way of including the GSL headers.

typedef struct {
  int val;
  int row;
  int col;
  int row_neighbor;
  int col_neighbor;
} cell;

int bits=16;
int twobits=65536;   /* always set to 2**bits */

void roundeven( int *imat,int *imat1, int *rowparity,int *nr, int *nc, gsl_rng *rng) {
  int i,j,k,i1,k1,allzero,nrow,ncol,nentry,up;
  cell *g;
  /*   Rprintf("Entering roundeven, nrow = %d , ncol = %d \n",nrow,ncol); */

  nrow = *nr;
  ncol = *nc;
  allzero=1;
  nentry=0;
  for (i=0, k=0; i < nrow; i++) {
    rowparity[i] = 0;         /* first column (1:nrow) contains parity */
    rowparity[i+nrow] = -1;   /* 2nd column will contain pointer to row_neighbor in g */
    for (j=0; j < ncol; j++, k++) {
      imat1[k] = imat[k]%2;
      if (imat1[k]) allzero = 0;            //valgrind says no.
    }
  }
  if (allzero) return;
  
  /* add an extra column so there are an even number of 1's in each row */
  imat1[nrow*ncol-1] = 0;   /* grand total cell */
  for (i=0; i < (nrow - 1); i++) {
    k1 = i + nrow * (ncol-1);
    imat1[k1] = 0;
    for (j=0; j < (ncol-1); j++) {
      imat1[k1] ^= imat1[i + nrow * j];
    }
    imat1[nrow*ncol-1] ^=  imat1[k1];
  }
  /* Add an extra row so there are an even number of 1's in each column */
  for (j=0; j < (ncol-1); j++) {
    k1 = (nrow - 1) +j * nrow ;
   imat1[k1] = 0;
   for (i=0; i < (nrow-1); i++) {
     imat1[k1] ^= imat1[i + nrow * j];
    }
 }  

  /* Count the number of 1's into nentry */
  for (k=0, i=0; i < nrow; i++) {
    for (j=0; j < ncol; j++, k++) {
      if (imat1[k]) nentry +=1;
    }
  }

  if ( nentry %2) {
    //Rprintf(" Error in roundeven - uneven number of 1's ");
    printf(" Error in roundeven - uneven number of 1's ");
  }
  //g = (cell *)R_alloc(nentry,sizeof(cell));
  g = malloc(nentry*sizeof(cell));
  /* initialize graph entries to -1 to indicate not set */
  for (k=0; k < nentry; k++) {
    g[k].val = g[k].row = g[k].col = g[k].row_neighbor = g[k].col_neighbor = -1;
  }
  k = 0;
  k1 = 0;
  for (j=0; j < ncol ; j++) {
    for (i=0; i < nrow ; i++) {
      if (imat1[k1] == 1) {      /*consider changing to if (imat1[k1]) */
        g[k].val = 1;
        g[k].row = i;
        g[k].col = j;
        g[k].col_neighbor = (k %2) == 0 ? k+1 : k-1;
        if (rowparity[i]) {
          i1 = rowparity[i+nrow];
          g[k].row_neighbor = i1;
          g[i1].row_neighbor = k;
          rowparity[i+nrow] = -1;
        }
        else {
          rowparity[i+nrow] = k;
        }
        rowparity[i] =  ! rowparity[i];
        k += 1;
      }
      k1 += 1;
    }
  }
  /* original code that works but is an inefficient search for row neighbors 
  for (k=0; k < nentry; k++) {
    if (g[k].row_neighbor == -1) {
      i1 = g[k].row;
      for (k1 = k+1; k1 < nentry; k1++) {
	if (g[k1].row == i1) {
	  g[k].row_neighbor = k1;
	  g[k1].row_neighbor = k;
	  break;
	}
      }
    }
  }
  */
  /* This loop finds the begining of each cycle */
  for (k1=0; k1 < nentry; k1++) {
    up = gsl_rng_uniform(rng) > .5 ? 1 : 0;
    k = k1;
    /* The following loop follows a cycle in the graph,
     first along a row, then a column, alternately assigning
     values of 0 or 2 to the node, stopping when back to the first node
    in the cycle */
    while (g[k].val == 1) {   
      if (up) {
        g[k].val = 2;
        imat1[nrow*g[k].col+g[k].row] = g[k].val;
        k = g[k].row_neighbor;
      }
      else {
        g[k].val = 0;
        imat1[nrow*g[k].col+g[k].row] = g[k].val;
        k = g[k].col_neighbor;
      }
      up = !up;
    }
  }
  /* Add the rounded matrix to the original matrix */
  for (k=0, i=0; i < nrow; i++) {
    for (j=0; j < ncol; j++, k++) {
      if (imat[k]) {
        imat[k] = imat1[k] + 2*(imat[k]/2);
      }
    }
  }
  free(g);
}


void unbiasround( double *mat, int *imat, int * imat1, int * rowparity,int *nrow, int *ncol, gsl_rng *r) {
  int i,j,k,k1;
  int  nr,nc;
  static int first_use = 1;
  gsl_rng *rng;
  if (r) rng = r;
  else {
      if (first_use){
          gsl_rng_env_setup();
          first_use=0;
      }
      rng  =  gsl_rng_alloc(gsl_rng_taus2);
  }
  gsl_rng_set(rng, 27);
  /*  Rprintf("Entering unbiasround, nrow = %d , ncol = %d \n",*nrow,*ncol); */

  nr = (*nrow) + 1;
  nc = (*ncol) + 1;

  /* Make an integer array
    Also  add an extra column so there are an even number of 1's in each row */
    for (i=0; i < *nrow; i++) {
      k1 = i + nr * *ncol;
      imat[k1] = 0;
      for (j=0; j < *ncol; j++) {
        imat[i + nr * j] = lrint( mat[i + *nrow * j]*(twobits));
      }
    }
    /* Add an extra row so there will be an even number of 1's in each column */
    for (j=0; j <= *ncol; j++) { //don't forget the lower-right corner.
        k1 = *nrow +j * nr ;
        imat[k1] = 0;
    }  

    for (k1=0; k1< bits; k1++) {
        roundeven(imat,imat1,rowparity,&nr,&nc, rng); 
        for (k=0, i=0; i < nr; i++) {
            for (j=0; j < nc; j++, k++) {
                imat[k] >>= 1;   /* Divide entire matrix by 2 using right shift 1 bit */
            }
        }
    }
}
