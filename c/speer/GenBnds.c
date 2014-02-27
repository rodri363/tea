/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */

////#include "internal.h"     // apophenia v 0.995
#include <stdio.h>
#include <apop.h>
#include <string.h>
#include <stdlib.h>
#include <sqlite3.h>

#include <unistd.h>

/* global constants */
#define maxflds 100    /* maximum # of basic items/field */
#define maxfldlen 30   /* maximum field name length */
#define maxexps 100    /* maximum # of Explicit ratios (per category) */
#define maxcats 30     /* maximum # categories */
int BFLD;	           /* # of basic items */
char bnames[maxfldlen][maxflds];  /* Basic item names [name length][# of fields] */
int TOTSIC;            /* # of categories of ratios */
int ncat, cat;         /* Category loop counter, Current category code */
int NEDFF;	           /* # of explicit ratios per category */
int numff[maxexps];    /* Numerators of explicit ratios */
int denff[maxexps];    /* Denominators of explicit ratios */

/* global variables */
int incon = 0;         /* # of inconsistent ratios/bounds */
int namlen = 0;        /* Length of longest basic item name */
int npass = 0;   /*****  FIX ME:  Id rather have TEA call GenBnds only once. *****/

#define MIN(a,b) ( a < b ? a : b)
#define MAX(a,b) ( a > b ? a : b)

struct { float lower[maxflds][maxflds]; float upper[maxflds][maxflds]; } bnds;

extern int genbnds_(void);
extern apop_data *get_key_text( char*, char* );
extern char *get_key_word( char*, char* );


int genbnds_(void)
/******************************************************/ 
{
  int i, j, k, pos;
  float wgt;
  char nam[maxfldlen];

  /* Subroutines */
  extern int ReadExplicits(void), GenImplicits(void), CheckIncon(void),
	         PreChecks(void), PostChecks(void); 

/*****************  FIX ME:  Id rather have TEA call GenBnds only once. ************/
  /* Creating the implicit bounds needs to been done only once. */
  npass++;
  if( npass > 1 ) { return 0; }

/* TeaKEY( SPEERparams/BFLD, <<< BFLD = # of basic items >>>)
   TeaKEY( SPEERparams/NEDFF, <<< NEDFF = # of explicit ratios per category >>>)
   TeaKEY( SPEERparams/TOTSIC, <<< TOTSIC = # of explicit ratios per category >>>)
*/  
  /* Incorporate SPEER parameters from .db/.spec file */
  char *bfld_s = get_key_word("SPEERparams", "BFLD");
  //// if (!bfld_s) return -2; //no Speer segment in the spec file.
  char *nedff_s = get_key_word("SPEERparams", "NEDFF");
  char *totsic_s = get_key_word("SPEERparams", "TOTSIC");

  BFLD = atoi( bfld_s );
  NEDFF = atoi( nedff_s );
  TOTSIC = atoi( totsic_s );

  /* Check for potential pre-processing fatal problems. */
  PreChecks();

  /* TeaKEY( SPEERfields, <<< Basic item names:  listed in imputation order. >>>) */
  /* Get field names from .db/.spec file & store them in an array */
  /* Determine longest field name for check later on              */
  apop_data *Bnames_s = get_key_text("SPEERfields", NULL);
  for (i = 0; i <= BFLD-1; ++i) {
    sscanf( Bnames_s->text[i][0], "%s %d %f", nam, &pos, &wgt );
    strcpy( bnames[pos], nam );
	if( strlen(bnames[pos]) > namlen ) { namlen = strlen(bnames[pos]); }
  }

  /* Create output tables */
  /* SPEERimpl table contains Implicit ratios/bounds. */
  /* SPEERincon table contains inconsistent ratios/bounds, if any exist. */
  apop_query("drop table SPEERimpl;");   // FIX ME:  first check if SPEERimpl exists
  apop_query("create table SPEERimpl( cat int, i int, j int, numer text, denom text, lower float, upper float );");
  apop_query("drop table SPEERincon;");  // FIX ME:  first check if SPEERincon exists
  apop_query("create table SPEERincon( numer text, denom text, lower float, upper float );");

  /* Derive implicit bounds for each category. */
  for( ncat = 1; ncat <= TOTSIC; ++ncat ) {
    ReadExplicits();
    GenImplicits();
    CheckIncon();

    /* For each category, store implicit bounds in table:  SPEERimpl */
    for (i = 1; i <= BFLD; ++i) {
      for (j = 1; j <= BFLD; ++j) {
         apop_query("insert into SPEERimpl values( %d, %d, %d, '%s', '%s', %f, %f);",
	            cat, i, j, bnames[i], bnames[j], bnds.lower[i][j], bnds.upper[i][j]);
      }
    }
  }

  /* Check for potential post-processing fatal problems. */
  PostChecks();

  printf( " SPEER:  Implicit bounds -> table SPEERimpl. \n" );

  return 0;
}


int CheckIncon(void)
/******************************************************/ 
/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
{
  static int i, j;

  /* CHECK FOR BOUNDS INCONSISTENCIES.  Tab/store any inconsistencies. */
  for (i = 1; i <= BFLD; ++i) {
	for (j = 1; j <= BFLD; ++j) {
      if (i == j && bnds.lower[i][j] == bnds.upper[i][j]) { goto L200; }

      /* If inconsistencies exist, tab/store. */
      if( bnds.lower[i][j] >= bnds.upper[i][j] ) {
         incon++;
         apop_query("insert into SPEERincon values( '%s', '%s', %f, %f);",
		            bnames[i], bnames[j], bnds.lower[i][j], bnds.upper[i][j] );
	  }
      L200: ;
	}
  }
  return 0;
}


int GenImplicits(void)
/******************************************************/ 
/* Calculate the Implicit edits */
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


int PreChecks(void)
/******************************************************/ 
/* Potential pre-processing fatal problems. */
{
  /* Stop program if maximum # of fields is exceded. */
  /*   Just increase the value of maxflds variable.  */
  Apop_stopif( BFLD > maxflds, return 0, -5,
        "**** FATAL ERROR in GenBnds:  Maximum number of fields (%d) exceded. ****\n",
 	    maxflds ); 

  /* Stop program if max length of field names is exceded. */
  /*   Just increase the value of maxfldlen variable.      */
  Apop_stopif( namlen > maxfldlen, return 0, -5,
        "**** FATAL ERROR in GenBnds:  Maximum length of field name (%d) exceded. ****\n",
 	    maxfldlen ); 

  /* Stop program if number of Explicit ratios (per category) is exceded. */
  /*   Just increase the value of maxexps variable.  */
  Apop_stopif( NEDFF > maxexps, return 0, -5,
        "**** FATAL ERROR in GenBnds:  Maximum number of Expicit ratios (%d) exceded. ****\n",
 	    maxexps ); 
}



int PostChecks(void)
/******************************************************/ 
/* Potential post-processing fatal problems. */
{
  int i;
  double diff;
  #define margin .05   // margin of error

  /* Stop program if inconsistencies exist. */
  Apop_stopif( incon > 0, return 0, -5,
	       "**** FATAL ERROR in GenBnds:  %d inconsistent bounds exist. ****\n", 
	       incon); 

  /* Stop program if diagonals' bounds not = 1.0 */
  for (i = 1; i <= BFLD; ++i) {
    Apop_stopif( bnds.lower[i][i] != (double)1.0 | bnds.upper[i][i] != (double)1.0, return 0, -5,
        "**** FATAL ERROR in GenBnds:  %s/%s bounds not = 1.0 ****\n",
 	    bnames[i], bnames[i] ); 
  }

  /* Random checks. */
  /*
  diff = MIN( bnds.lower[1][2], (double).0215853 ) / MAX( bnds.lower[1][2], (double).0215853 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return 0, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s lower bound not = .0215853 ****\n", 
	       bnames[1], bnames[2] ); 

  diff = MIN( bnds.upper[3][8], (double)578.9274902 ) / MAX( bnds.upper[3][8], (double)578.9274902 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return 0, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s upper bound not = 578.9274902 ****\n", 
	       bnames[3], bnames[8] ); 

  diff = MIN( bnds.upper[2][6], (double)73072.4218750 ) / MAX( bnds.upper[2][6], (double)73072.4218750 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return 0, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s upper bound not = 73072.4218750 ****\n", 
	       bnames[2], bnames[6] ); 

  diff = MIN( bnds.lower[2][6], (double).0001406 ) / MAX( bnds.lower[2][6], (double).0001406 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return 0, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s lower bound not = .0001406 ****\n", 
	       bnames[2], bnames[6] ); 
*/
}

int ReadExplicits(void)
/******************************************************/ 
/* *** DEFINE ALL EXPLICIT BOUNDS BY READING IN EXISTING BOUNDS **** */
/* *** AND FINDING THEIR INVERSE.                               **** */
{
  /* Local variables */
  static int i, j;
  static double temp;
  float lo, up;

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

/*******************************************************************************/ 
/*****************************************/
/* *** IMPORT EXPLICIT RATIOS HERE. **** */
/*****************************************/
  char num[maxfldlen], den[maxfldlen];
 
  /* TeaKEY( ExpRatios, <<< User-supplied Explicit ratios for basic items. >>>) */
  /* Get Explicit bounds from .db */
  apop_data *ExpBnds = get_key_text("ExpRatios", NULL);

  /* Parse ExpBnds string */
  /* Determine/fill numff & denff arrays.  Then place Explicit ratios into matrices. */
  for (i = (ncat-1)*NEDFF; i <= (ncat*NEDFF)-1; ++i) {
    sscanf( ExpBnds->text[i][0], "%d %s %s %f %f", &cat, num, den, &lo, &up );
    numff[i+1] = 0;
    denff[i+1] = 0;
    for (j = 1; j <= BFLD; ++j) {
	  if( strcmp(num, bnames[j]) == 0 ) { numff[i+1] = j; }
	  if( strcmp(den, bnames[j]) == 0 ) { denff[i+1] = j; }
	}
    Apop_stopif( numff[i+1] == 0 | denff[i+1] == 0, return 0, -5,
		"**** FATAL ERROR in GenBnds:  Problem reading explicit ratio #%d. ****\n", i ); 

	/* Place Explicit ratios into matrices. */
    bnds.lower[numff[i+1]][denff[i+1]] = lo;
    bnds.upper[numff[i+1]][denff[i+1]] = up;
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

