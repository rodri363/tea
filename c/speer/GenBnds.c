/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */

#include <stdio.h>
#include <apop.h>
#include <string.h>
#include <stdlib.h>

/*****************  FIX ME:  Id rather have TEA call GenBnds only once. ************/
int npass = 0;   

/* global constants */
int BFLD;	  /* # of basic items */
int TOTSIC;   /* # of categories of ratios */
int NEDFF;	  /* # of explicit ratios per category */
char bnames[30][100];  /* Basic item names [name length][# of fields] */

//// **** FIX ME:  should be in .spec file
static int numff[12+1] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
static int denff[12+1] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

struct { float lower[100][100]; float upper[100][100]; } bnds;

extern int genbnds_(void);



int genbnds_(void)
/******************************************************/ 
{
  int i, j, ncat, pos;
  char nam[30];

  /* Subroutines */
  extern int readpa_(void), genlim_(void), rattab_(void);

  ////char *bfld = get_key_word("SPEERparams", "BFLD");
  /* Incorporate SPEER parameters from .db/.spec file */
  apop_data *bfld_s = get_key_text("SPEERparams", "BFLD");
  apop_data *nedff_s = get_key_text("SPEERparams", "NEDFF");
  apop_data *totsic_s = get_key_text("SPEERparams", "TOTSIC");

  BFLD = atoi( bfld_s->text[0][0] );
  NEDFF = atoi( nedff_s->text[0][0] );
  TOTSIC = atoi( totsic_s->text[0][0] );

  /* Get field names from .db/.spec file & store them in an array */
  apop_data *Bnames_s = get_key_text("SPEERfields", NULL);
  for (i = 0; i <= BFLD-1; ++i) {
    sscanf( Bnames_s->text[i][0], "%s %d", nam, &pos );
    strcpy( bnames[pos], nam );
  }

  /* Derive implicit bounds for each category.  This only needs to been done once. */
  npass++;
  if( npass == 1 ) {
 ////   apop_table_exists( 'SPEERimpl', 'd' );  // Delete table SPEERimpl if it exists.
    apop_query( "drop table SPEERimpl;" );
    apop_query( "create table SPEERimpl( cat int, i int, j int, numer text, denom text, lower float, upper float );");
    for( ncat = 1; ncat <= TOTSIC; ++ncat ) {
      readpa_();
      genlim_();
      rattab_();

      /* Store implicit bounds in table:  SPEERimpl */
      for (i = 1; i <= BFLD; ++i) {
        for (j = 1; j <= BFLD; ++j) {
	      apop_query("insert into SPEERimpl values( %d, %d, %d, '%s', '%s', %f, %f);",
		             ncat+100, i, j, bnames[i], bnames[j], bnds.lower[i][j], bnds.upper[i][j]);
	    }
      }
 ////     printf( " *** ncat = %d:  i = %d, j = %d \n", ncat+100, i, j );
    }
    printf( " SPEER:  Implicit bounds -> table SPEERimpl. \n" );
  }

  /*  printf("     IMPLICITS: %11.6f  < fld1/fld4 < %11.6f \n", bnds.lower[1][4],bnds.upper[1][4]);
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
  static double temp;

  /* *** INITIALIZE BOUNDS FOR EACH CATEGOR **** */
  for (i = 1; i <= BFLD; ++i) {
    for (j = 1; j <= BFLD; ++j) {
        bnds.lower[i][j] = (double)0.0;
	    bnds.upper[i][j] = (double)99999.9;
	}
  }

/* *** INITIALIZE IDENTITY DIAGONAL. **** */
  for (i = 1; i <= BFLD; ++i) {
	  bnds.lower[i][i] = (double)1.0;
	  bnds.upper[i][i] = (double)1.0;
  }

/*****************************************/
/* *** IMPORT EXPLICIT RATIOS HERE. **** */
/*****************************************/

/*******************************************************************************/ 
  // FIX ME:  num[] & den[] probably shouldnt be hard-coded to [10].
  char num[30], den[30];
 
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

