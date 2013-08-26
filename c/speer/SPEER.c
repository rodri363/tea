/* SPEER.c */
/* *** THE SUBROUTINES IN THIS FILE WILL BE USED FOR **** */
/* *** EVERY APPLICATION OF SPEER                    **** */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ************************************************************* */

#include <stdio.h>
#include <apop.h>

const int BFLD   = 9;
const int NEDIT  = ( 11 ) * 10 / 2;                       
const int TOTSIC = 2;

extern int speer_(void);

/* BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
struct { double basitm[10]; } comed1_;

#define comed1_1 comed1_

struct { double bm[10][10];
         int cntdel[10], frcomp[55][3];
       } comed2_;

#define comed2_1 comed2_

struct { double lwbd[3][10][10], upbd[3][10][10];
         int nblank, nf, numdel, numsic;
         double tm[10][10];
       } comed3_;

#define comed3_1 comed3_



int speer_(void)
/******************************************************/ 
/**** THIS IS THE DRIVER PORTION OF THE SPEER EDIT ****/
{
  extern int edchek_(void), locate_(void), impsub_(void);

  edchek_();
  if (comed3_1.nf > 0) { locate_(); }
  if (comed3_1.nf > 0 || comed3_1.nblank > 0) { impsub_(); }

  return 0;
}



int edchek_(void)
/******************************************************/ 
/* *** DETERMINE WHICH FIELDS FAIL EDIT RATIOS AND  **** */
/* ***  FLAG THOSE FIELDS  **** */
{
  static int i, j;
  static double lower, upper;
  static int bblank[10];

  comed3_1.nf = 0;
  comed3_1.nblank = 0;

  /* *** EDIT BASIC ITEMS **** */
  for (i = 1; i <= BFLD-1; ++i) {

     /* *** CHECK FOR BLANK BASIC ITEMS **** */
	 if (comed1_1.basitm[i] < (double)0.0) {
	    ++comed3_1.nblank;
	    bblank[comed3_1.nblank] = i;
     
	 /* *** FLAG EACH RATIO THAT FAILS AN EDIT **** */
	 } else {
	    for (j = i+1; j <= BFLD; ++j) {
		   lower = comed3_1.lwbd[comed3_1.numsic][i][j] * comed2_1.bm[i][j];
		   upper = comed3_1.upbd[comed3_1.numsic][i][j] * comed3_1.tm[i][j];
	       if ((comed1_1.basitm[j] == (double)0.0  
			   &&  lower > (double)0.0  &&  upper < (double)99999.0 )
			        || 
			   comed1_1.basitm[j] > (double)0.0 
			   && (lower > (double)0.0  &&  comed1_1.basitm[j]*lower > comed1_1.basitm[i] 
			   || upper < (double)99999.0  &&  comed1_1.basitm[j]*upper < comed1_1.basitm[i]))
		   {
		    ++comed3_1.nf;
		    comed2_1.frcomp[comed3_1.nf][1] = i;
		    comed2_1.frcomp[comed3_1.nf][2] = j;
		   }
	    }
	 }
  }

/* *** CHECK LAST BASIC ITEM FOR BLANKS **** */
  if (comed1_1.basitm[BFLD] < (double)0.0) {
    ++comed3_1.nblank;
    bblank[comed3_1.nblank] = BFLD;
  }

  return 0;
}



int impsub_(void)
/******************************************************/ 
/* *** CALCULATE A MISSING/DELETED FIELD'S IMPUTATION RANGE AND **** */
/* *** IMPUTE A VALUE USING THAT FIELD'S IMPUTATION HIERARCHY.  **** */
{
  static int i, k;
  static double bl, bu, bup, blow;
  static int nimp;
  static double bwgt[10];

/* Basic item weights */
/******  FIX ME:   put in .spec file  ******/
  bwgt[1] = (double)0.06;
  bwgt[2] = (double)0.25;
  bwgt[3] = (double)0.3;
  bwgt[4] = (double)0.75;
  bwgt[5] = (double)0.5;
  bwgt[6] = (double)1.4;
  bwgt[7] = (double)1.0;
  bwgt[8] = (double)1.9;
  bwgt[9] = (double)1.6;

  nimp = 0;
/* *** BASE THE RANGE OF THE DELETED FIELD ON ITS RELATIONSHIP **** */
/* *** TO ALL OTHER EXISTING BASIC ITEMS.                      **** */
  for (k = 1; k <= BFLD; ++k) {
	blow = (double)-1.0;
	bup = (double)99999999.9;
	if (comed1_1.basitm[k] <= (double)-1.0) {
	    ++nimp;

        /* *** CALCULATE IMPUTATION RANGE/REGION **** */
	    for (i = 1; i <= BFLD; ++i) {
	 	   if (comed1_1.basitm[i] >= (double)0.0) {
		      bl = comed1_1.basitm[i] * comed2_1.bm[k][i] * comed3_1.lwbd[comed3_1.numsic][k][i];
		      if (bl > blow) { blow = bl; }
		   }
		   if (comed1_1.basitm[i] > (double)0.0) {
		      bu = comed1_1.basitm[i] * comed3_1.tm[k][i] * comed3_1.upbd[comed3_1.numsic][k][i];
		      if (bu < bup) { bup = bu; }
		   }

           /* *** IF IMPUTATION RANGE IS NOT AVAILABLE, EXIT LOOP **** */
		   if (blow >= bup) { goto L800; }
	    }

/* ****************************************** */
/* *** CALCULATE IMPUTATION OPTIONS HERE **** */
/* ****************************************** */
/*          IMPOPT(K) = <value> */

/* ********************************************************************** */
/* *** DETERMINE WHICH IMPUTATION OPTION IS CONSISTENT STARTING HERE **** */
/* ********************************************************************** */
/* *** CHECK IF IMPUTE OPTION IS ACCEPTABLE **** */
/*         IF( IMPOPT(K) .GE. BLOW .AND. IMPOPT(K) .LE. BUP )THEN */
/*           BASITM(K) = IMPOPT(K) */
/*           GO TO 800 */
/*         ENDIF */
    }
    L800:;
  }

  return 0;
}



int locate_(void)
/******************************************************/ 
/*  LOCATE BASIC ITEMS TO BE DELETED AND FLAG THEM.   */
{
  /* System generated locals */
  int tmp;

  /* Local variables */
  static int i, j, bdel[10];
  static double wdeg[10], bwgt[10], wmax;
  static int narcs, remoov;
  static int bdeg[10];    /* bdeg[] may actually use the '0' cell */

/* Basic item weights */
/******  FIX ME:   put in .spec file  ******/
  bwgt[1] = (double)0.06;
  bwgt[2] = (double)0.25;
  bwgt[3] = (double)0.3;
  bwgt[4] = (double)0.75;
  bwgt[5] = (double)0.5;
  bwgt[6] = (double)1.4;
  bwgt[7] = (double)1.0;
  bwgt[8] = (double)1.9;
  bwgt[9] = (double)1.6;

  comed3_1.numdel = 0;
  narcs = comed3_1.nf;

  /*****  FIX ME:  have to find a better way than looping back to L100:  *****/
  /*****           perhaps a for ()                                      *****/
L100:

  /* ZERO OUT BASIC ITEM FAILURE COUNTERS */
  for (i = 1; i <= 9; ++i) {
	bdeg[i] = 0;
	wdeg[i] = (double)0.0;
  }

  /* COUNT NO. OF TIMES A BASIC ITEM IS INVOLVED IN A RATIO FAILURE */
  tmp = comed3_1.nf;
  for (i = 1; i <= tmp; ++i) {
	for (j = 1; j <= 2; ++j) {
	    ++bdeg[ comed2_1.frcomp[i][j] ];
	}
  }

  /* CALCULATE EACH WEIGHTED DEGREE              */
  /* DETERMINE WHICH BASIC ITEM HAS LARGEST WDEG */
  wmax = (double)0.0;
  for (i = 1; i <= BFLD; ++i) {
	wdeg[i] = bwgt[i] * bdeg[i];
	if (wdeg[i] > (double)0.0  &&  wdeg[i] >= wmax) {
	    wmax = wdeg[i];
	    remoov = i;
	}
  }

  /* FLAG THE BASIC ITEM TO BE DELETED AND REMOVE ALL FAILURE */
  /* RATIOS ATTACHED TO IT.                                   */
  ++comed3_1.numdel;
  bdel[comed3_1.numdel] = remoov;
  /* tmp = comed3_1.nf; */
  for (i = 1; i <= comed3_1.nf; ++i) {
    if (comed2_1.frcomp[i][1] == remoov  ||  comed2_1.frcomp[i][2] == remoov) {
        comed2_1.frcomp[i][1] = 0;
	    comed2_1.frcomp[i][2] = 0;
	    --narcs;
	}
  }

  /* CONTINUE TO DELETE FIELDS                    */
  /* UNTIL THEIR ARE NO MORE FAILURE RATIOS LEFT. */
  if (narcs > 0) { goto L100; }

  /* WHEN ALL ITEMS HAVE BEEN SUCCESSFULLY BEEN FLAGGED FOR  */
  /* DELETION, DELETE THEM AND COUNT THE OCCURENCE FOR EACH. */
  /* tmp = comed3_1.numdel; */
  for (i = 1; i <= comed3_1.numdel; ++i) {
	++comed2_1.cntdel[bdel[i]];
	comed1_1.basitm[bdel[i]] = (double)-1.0;
  }

  return 0;
}



int readlm_(void)
/******************************************************/ 
/* READ AND STORE ALL LOWER & UPPER IMPLICIT RATIOS */
/* AND CENTRAL VALUES                               */
{
  static int i, j, k;

  /* OPEN AND READ FILE CONTAINING IMPLICIT RATIOS */
  /* OPEN( 12, FILE = 'RATIOS.BND' ) */
  for (i = 1; i <= comed3_1.numsic; ++i) {
	for (j = 1; j <= BFLD; ++j) {
	  for (k = 1; k <= BFLD; ++k) {
         /* READ(12,2000) comed3_1.lwbd[i][j][k], comed3_1.upbd[i][j][k] */
	  }
	}
  }

  return 0;
}

