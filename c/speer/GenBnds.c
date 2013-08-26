/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */

#include <stdio.h>
#include <apop.h>

/* 10 = BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
struct { double lower[10][10]; double upper[10][10]; } bnds;

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

   /* 
  printf("** IMPLICITS: %11.6f  < fld1/fld4 < %11.6f \n", bnds.lower[1][4],bnds.upper[1][4]);
  printf("** IMPLICITS: %11.6f  < fld3/fld5 < %11.6f \n", bnds.lower[3][5],bnds.upper[3][5]);
  printf("** IMPLICITS: %11.6f  < fld7/fld9 < %11.6f \n", bnds.lower[7][9],bnds.upper[7][9]);
  */

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
      if (bnds.upper[i][j] < (double)99999.0) {
		for (k = 1; k <= 9; ++k) {
		    if (bnds.upper[i][j] * bnds.upper[k][i] < (double)99999.0) {
              temp = bnds.upper[i][j] * bnds.upper[k][i];
			  if (temp < bnds.upper[k][j]) { bnds.upper[k][j] = temp;}
		    }
		}
	  }
	}
  }

/* *** MODIFY LOWER BOUNDS. **** */
  for (i = 1; i <= 9; ++i) {
	for (j = 1; j <= 9; ++j) {
	  if (bnds.upper[j][i] >= (double)99999.0) {
	       	temp = (double)0.0;
	  } else {
		    temp = (double)1.0 / bnds.upper[j][i];
	  }
	  if (temp > bnds.lower[i][j]) { bnds.lower[i][j] = temp; }
	}
  }

  return 0;
}


int rattab_(void)
/******************************************************/ 
/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
{
  /* *** BASIC ITEM NAMES **** */
  /* 10 = BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
  static char bnames[5*10] = "     " "FEMP " "FAPR " "FQPR " "FFBR " 
   	                        "FSLS " "FAET " "FTOT " "FRPT " "FADE ";

  /* Local variables */
  static int i, j, incon;
 /*  static real cat; */

/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
  incon = 0;
  for (i = 1; i <= 9; ++i) {
	for (j = 1; j <= 9; ++j) {
      if (i == j && bnds.lower[i][j] == bnds.upper[i][j]) { goto L200; }

      /* *** IF INCONSISTENCIES EXIST, PRINT MESSAGE **** */
      if (bnds.lower[i][j] >= bnds.upper[i][j]) {
        printf("**** FATAL ERROR in GenBnds ****\n","     %11.6f  < fld %d / fld %d < %11.6f \n",
			   bnds.lower[i][j], i, j, bnds.upper[i][j]);
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
    /* 10 = BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
    static char bnames[5*10] = "     " "FEMP " "FAPR " "FQPR " "FFBR "  
	                           "FSLS " "FAET " "FTOT " "FRPT " "FADE ";
    /* 13 = NRAT (or NEDFF) + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
    static int numff[13] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
    static int denff[13] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

	/* *** INITIALIZE BOUNDS FOR EACH CATEGOR **** */
    for (i = 1; i <= 9; ++i) {
	  for (j = 1; j <= 9; ++j) {
	    bnds.lower[i][j] = (double)0.0;
	    bnds.upper[i][j] = (double)99999.9;
	  }
    }

/* *** INITIALIZE IDENTITY DIAGONAL. **** */
    for (i = 1; i <= 9; ++i) {
	  bnds.lower[i][j] = (double)1.0;
	  bnds.upper[i][j] = (double)1.0;
    }

/*****************************************/
/* *** IMPORT EXPLICIT RATIOS HERE. **** */
/*****************************************/

/*****************************************/
/**** temporary fix just to test code ****/
bnds.lower[1][2] = (double).0212400;
bnds.upper[1][2] = (double).0711125;
bnds.lower[2][3] = (double)1.5369120;
bnds.upper[2][3] = (double)6.8853623;
bnds.lower[3][2] = (double).1670480;
bnds.upper[3][2] = (double).5273000;
bnds.lower[4][2] = (double).0202880;
bnds.upper[4][2] = (double).2717625;
bnds.lower[5][2] = (double).0019120;
bnds.upper[5][2] = (double)17.9646122;

bnds.lower[7][6] = (double).4114320;
bnds.upper[7][6] = (double)2.737300;
bnds.lower[8][5] = (double).0007280;
bnds.upper[8][5] = (double).0356625;
bnds.lower[9][6] = (double).1512560;
bnds.upper[9][6] = (double).7307250;
bnds.lower[1][3] = (double).0430320;
bnds.upper[1][3] = (double).3661875;
bnds.lower[2][1] = (double)13.6115752;
bnds.upper[2][1] = (double)46.3278246;

bnds.lower[3][1] = (double)2.9163521;
bnds.upper[3][1] = (double)15.6438375;
bnds.lower[5][1] = (double)37.1182704;
bnds.upper[5][1] = (double)559.9335861;
/*****************************************/

/* *** ASSIGN THE EXISTING RATIOS' INVERSE LOWER BOUND TO THE **** */
/* *** INVERSE RATIOS' UPPER BOUND, AND THE EXISTING RATIOS'  **** */
/* *** INVERSE UPPER BOUND TO THE INVERSE RATIOS' LOWER BOUND **** */
    for (i = 1; i <= 12; ++i) {
       /* printf("        ****  Running for12 i=%d numff[i]=%d denff[i]=%d\n", i,numff[i],denff[i]);  */
  	  if (bnds.lower[numff[i]][denff[i]] > (double)0.0) {
	      temp = (double)1.0 / bnds.lower[numff[i]][denff[i]];
	  } else {
	      temp = (double)99999.8;
	  }

	  if (temp < bnds.upper[denff[i]][numff[i]]) {
	             bnds.upper[denff[i]][numff[i]] = temp;
	  }

  	  if (bnds.upper[numff[i]][denff[i]] < (double)99999.9) {
	      temp = (double)1.0 / bnds.upper[numff[i]][denff[i]];
	  } else {
	      temp = (double)0.0;
	  }

	  if (temp > bnds.lower[denff[i]][numff[i]]) {
	             bnds.lower[denff[i]][numff[i]] = temp;
	  }
    }

    return 0;
}

