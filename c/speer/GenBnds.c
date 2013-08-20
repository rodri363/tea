/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */
/* *** WRITE DIAGNOSTICS TO FILE 13 WHEN PROBLEMS OCCUR. **** */

#include <stdio.h>

/* Common Block Declarations */

struct { double lower[9][9], upper[9][9]; } comgen_;

#define comgen_1 comgen_

/* Table of constant values
static integer c__1 = 1; */

  extern int genbnds_(void);


int genbnds_(void)
/******************************************************/ 
{
  static int i;
  
  /* Subroutines */
  extern int readpa_(void), genlim_(void), rattab_(void);

  /* *** DERIVE RATIOS FOR EACH CATEGORY. **** */
  for( i = 1; i <= 2; ++i ) {
        /* printf("    ****  Calling readpa_ in genbnds_ ****.\n");  */
    readpa_();
    genlim_();
    rattab_();
  }

  return 0;
}


int genlim_(void)
/******************************************************/ 
{
  static int i, j, k;
  static double temp;

/* *** MODIFY UPPER BOUNDS BASED ON OTHER RATIOS RELATIONSHIPS. **** */
  for (i = 1; i <= 9; ++i) {
    for (j = 1; j <= 9; ++j) {
      if (comgen_1.upper[i][j] < (double)99999.0) {
		for (k = 1; k <= 9; ++k) {
		    if (comgen_1.upper[i][j] * comgen_1.upper[k][i] < (double)99999.0) {
              temp = comgen_1.upper[i][j] * comgen_1.upper[k][i];
			  if (temp < comgen_1.upper[k][j]) { comgen_1.upper[k][j] = temp;}
		    }
		}
	  }
	}
  }

/* *** MODIFY LOWER BOUNDS. **** */
  for (i = 1; i <= 9; ++i) {
	for (j = 1; j <= 9; ++j) {
	  if (comgen_1.upper[j][i] >= (double)99999.0) {
	       	temp = (double)0.0;
	  } else {
		    temp = (double)1.0 / comgen_1.upper[j][i];
	  }
	  if (temp > comgen_1.lower[i][j]) { comgen_1.lower[i][j] = temp; }
	}
  }

  return 0;
}


int rattab_(void)
/******************************************************/ 
/* *** WRITE OUT FINAL BOUNDS. **** */
{
  /* *** BASIC ITEM NAMES **** */
  static char bnames[5*9] = "FEMP " "FAPR " "FQPR " "FFBR " "FSLS " 
   	                        "FAET " "FTOT " "FRPT " "FADE ";

  /* Local variables */
  static int i, j, incon;
 /*  static real cat; */

/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
  incon = 0;
  for (i = 1; i <= 9; ++i) {
	for (j = 1; j <= 9; ++j) {
      if (i == j && comgen_1.lower[i][j] == comgen_1.upper[i][j]) { goto L200; }

      /* *** IF INCONSISTENCIES EXIST, PRINT MESSAGE **** */
      if (comgen_1.lower[i][j] >= comgen_1.upper[i][j]) {
        /* write(ofp13,5000) cat, bnames[i], bnames[j], i, j, 
		                     comgen_1.lower[i][j], comgen_1.upper[i][j]; */
		++incon;
	  }
      L200: ;
	}
  }

  if (incon > 0) {
    /* WRITE(ofp13,2000) CAT, INCON */
  }

  return 0;
}


/* Subroutine */ int readpa_(void)
/******************************************************/ 
/* *** DEFINE ALL EXPLICIT BOUNDS BY READING IN EXISTING BOUNDS **** */
/* *** AND FINDING THEIR INVERSE.                               **** */
{
    /* Local variables */
    static int i, j;
    static double cat, temp;

    /* Initialized data */
    static char bnames[5*9] = "FEMP " "FAPR " "FQPR " "FFBR " "FSLS " 
	                          "FAET " "FTOT " "FRPT " "FADE ";
    static int numff[13] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
    static int denff[13] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

	/* *** INITIALIZE BOUNDS FOR EACH CATEGOR **** */
    for (i = 1; i <= 9; ++i) {
	  for (j = 1; j <= 9; ++j) {
	    comgen_1.lower[i][j] = (double)0.0;
	    comgen_1.upper[i][j] = (double)99999.9;
	  }
    }

/* *** INITIALIZE IDENTITY DIAGONAL. **** */
    for (i = 1; i <= 9; ++i) {
	  comgen_1.lower[i][j] = (double)1.0;
	  comgen_1.upper[i][j] = (double)1.0;
    }

/*****************************************/
/* *** IMPORT EXPLICIT RATIOS HERE. **** */
/*****************************************/
comgen_1.lower[1][2] = (double).0212400;
comgen_1.upper[1][2] = (double).0711125;
comgen_1.lower[2][3] = (double)1.5369120;
comgen_1.upper[2][3] = (double)6.8853623;
comgen_1.lower[3][2] = (double).1670480;
comgen_1.upper[3][2] = (double).5273000;
comgen_1.lower[4][2] = (double).0202880;
comgen_1.upper[4][2] = (double).2717625;
comgen_1.lower[5][2] = (double).0019120;
comgen_1.upper[5][2] = (double)17.9646122;

comgen_1.lower[7][6] = (double).4114320;
comgen_1.upper[7][6] = (double)2.737300;
comgen_1.lower[8][5] = (double).0007280;
comgen_1.upper[8][5] = (double).0356625;
comgen_1.lower[9][6] = (double).1512560;
comgen_1.upper[9][6] = (double).7307250;
comgen_1.lower[1][3] = (double).0430320;
comgen_1.upper[1][3] = (double).3661875;
comgen_1.lower[2][1] = (double)13.6115752;
comgen_1.upper[2][1] = (double)46.3278246;

comgen_1.lower[3][1] = (double)2.9163521;
comgen_1.upper[3][1] = (double)15.6438375;
comgen_1.lower[5][1] = (double)37.1182704;
comgen_1.upper[5][1] = (double)559.9335861;

/* *** ASSIGN THE EXISTING RATIOS' INVERSE LOWER BOUND TO THE **** */
/* *** INVERSE RATIOS' UPPER BOUND, AND THE EXISTING RATIOS'  **** */
/* *** INVERSE UPPER BOUND TO THE INVERSE RATIOS' LOWER BOUND **** */
    for (i = 1; i <= 12; ++i) {
       /* printf("        ****  Running for12 i=%d numff[i]=%d denff[i]=%d\n", i,numff[i],denff[i]);  */
  	  if (comgen_1.lower[numff[i]][denff[i]] > (double)0.0) {
	      temp = (double)1.0 / comgen_1.lower[numff[i]][denff[i]];
	  } else {
	      temp = (double)99999.8;
	  }

	  if (temp < comgen_1.upper[denff[i]][numff[i]]) {
	             comgen_1.upper[denff[i]][numff[i]] = temp;
	  }

  	  if (comgen_1.upper[numff[i]][denff[i]] < (double)99999.9) {
	      temp = (double)1.0 / comgen_1.upper[numff[i]][denff[i]];
	  } else {
	      temp = (double)0.0;
	  }

	  if (temp > comgen_1.lower[denff[i]][numff[i]]) {
	             comgen_1.lower[denff[i]][numff[i]] = temp;
	  }
    }

    return 0;
}

