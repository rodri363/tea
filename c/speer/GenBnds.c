/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */

#include <stdio.h>
#include <apop.h>
#include <string.h>

/* constant globals */
/* **** FIX ME:   constant globals should go into .spec file **** */
#define BFLD 9		/* # of basic items */
#define TOTSIC 2	/* # of categories of ratios */
#define NEDFF 12	/* # of explicit ratios per category */

/* BASIC ITEM NAMES */
/* BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */
//// **** FIX ME:  Item names should be in .spec file
static char bnames[5*(BFLD+1)] = "     " "FEMP " "FAPR " "FQPR " "FFBR " 
                                 "FSLS " "FAET " "FTOT " "FRPT " "FADE ";

/* NRAT (or NEDFF) + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
//// **** FIX ME:  should be in .spec file
static int numff[NEDFF+1] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
static int denff[NEDFF+1] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

/* BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
struct { float lower[BFLD+1][BFLD+1]; float upper[BFLD+1][BFLD+1]; } bnds;

extern int genbnds_(void);



int genbnds_(void)
/******************************************************/ 
{
  int i;

  /* Subroutines */
  extern int readpa_(void), genlim_(void), rattab_(void);

  /* *** DERIVE RATIOS FOR EACH CATEGORY. **** */
  for( i = 1; i <= TOTSIC; ++i ) {
    readpa_();
    genlim_();
    rattab_();

  /*  printf("  IMPLICITS: %11.6f  < fld1/fld4 < %11.6f \n", bnds.lower[1][4],bnds.upper[1][4]);
    printf("** IMPLICITS: %11.6f  < fld3/fld5 < %11.6f \n", bnds.lower[3][5],bnds.upper[3][5]);
    printf("** IMPLICITS: %11.6f  < fld7/fld9 < %11.6f \n", bnds.lower[7][9],bnds.upper[7][9]);
  */
  }

  return 0;
}


int genlim_(void)
/******************************************************/ 
{
  static int i, j, k;
  static double temp;

/* *** MODIFY UPPER BOUNDS BASED ON OTHER RATIOS RELATIONSHIPS. **** */
  for (i = 1; i <= BFLD; ++i) {
    for (j = 1; j <= BFLD; ++j) {
      if (bnds.upper[i][j] < (double)99999.0) {
		for (k = 1; k <= BFLD; ++k) {
		    if (bnds.upper[i][j] * bnds.upper[k][i] < (double)99999.0) {
              temp = bnds.upper[i][j] * bnds.upper[k][i];
			  if (temp < bnds.upper[k][j]) { bnds.upper[k][j] = temp;}
		    }
		}
	  }
	}
  }

/* *** MODIFY LOWER BOUNDS. **** */
  for (i = 1; i <= BFLD; ++i) {
	for (j = 1; j <= BFLD; ++j) {
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

  /* Local variables */
  static int i, j, incon;
 /*  static real cat; */

/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
  incon = 0;
  for (i = 1; i <= BFLD; ++i) {
	for (j = 1; j <= BFLD; ++j) {
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
    /* BFLD + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
////    static char bnames[5*(BFLD+1)] = "     " "FEMP " "FAPR " "FQPR " "FFBR "  
////	                                 "FSLS " "FAET " "FTOT " "FRPT " "FADE ";

    /* NRAT (or NEDFF) + 1, to compensate for the zero element in C ( not in FORTRAN ) */	
////    static int numff[NEDFF+1] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
////    static int denff[NEDFF+1] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

	/* *** INITIALIZE BOUNDS FOR EACH CATEGOR **** */
    for (i = 1; i <= BFLD; ++i) {
	  for (j = 1; j <= BFLD; ++j) {
	    bnds.lower[i][j] = (double)0.0;
	    bnds.upper[i][j] = (double)99999.9;
	  }
    }

/* *** INITIALIZE IDENTITY DIAGONAL. **** */
    for (i = 1; i <= BFLD; ++i) {
	  bnds.lower[i][j] = (double)1.0;
	  bnds.upper[i][j] = (double)1.0;
    }

/*****************************************/
/* *** IMPORT EXPLICIT RATIOS HERE. **** */
/*****************************************/

/*******************************************************************************/ 
  // FIX ME:  num[] & den[] probably shouldnt be hard-coded to [10].
  char num[10], den[10];
 
  /* Get Explicit bounds from .db */
  apop_data *ExpBnds = get_key_text("ExpRatios", NULL);

  ////printf("\n");
  for (i = 0; i <= NEDFF-1; ++i) {
	////printf("*** ExpBnds: %s  \n", ExpBnds->text[i][0]);

    /* Parse ExpBnds string */
    sscanf( ExpBnds->text[i][0], "%f %s %s %f", 
	      &bnds.lower[numff[i+1]][denff[i+1]], num, den, &bnds.upper[numff[i+1]][denff[i+1]] );
    ////printf("  bnds.lower: %f < %s/%s < bnds.upper: %f\n", 
	////      bnds.lower[numff[i+1]][denff[i+1]], num, den, bnds.upper[numff[i+1]][denff[i+1]] );
  }
/*******************************************************************************/ 

/* *** ASSIGN THE EXISTING RATIOS' INVERSE LOWER BOUND TO THE **** */
/* *** INVERSE RATIOS' UPPER BOUND, AND THE EXISTING RATIOS'  **** */
/* *** INVERSE UPPER BOUND TO THE INVERSE RATIOS' LOWER BOUND **** */
    for (i = 1; i <= NEDFF; ++i) {
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

