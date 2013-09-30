/* ***  GENBNDS.C  **** */
/* *** DERIVE THE IMPLICIT UPPER & LOWER BOUNDS FROM THE **** */
/* *** BASIC ITEM'S EXPLICIT BOUNDS.                     **** */

#include "internal.h"

/* global constants */
#define maxflds 100    /* maximum # of basic items/field */
#define maxfldlen 30   /* maximum field name length */
int BFLD;	           /* # of basic items */
int TOTSIC;            /* # of categories of ratios */
int NEDFF;	           /* # of explicit ratios per category */
char bnames[maxfldlen][maxflds];  /* Basic item names [name length][# of fields] */

/* global variables */
int incon = 0;         /* # of inconsistent ratios/bounds */
int namlen = 0;        /* Length of longest basic item name */
int npass = 0;   /*****  FIX ME:  Id rather have TEA call GenBnds only once. *****/

#define MIN(a,b) ( a < b ? a : b)
#define MAX(a,b) ( a > b ? a : b)

//// **** FIX ME:  should be in .spec file
static int numff[12+1] = {0, 1,2,3,4,5, 7,8,9,1,2, 3,5 };
static int denff[12+1] = {0, 2,3,2,2,2, 6,5,6,3,1, 1,1 };

struct { float lower[maxflds][maxflds]; float upper[maxflds][maxflds]; } bnds;

extern int genbnds_(void);



int genbnds_(void)
/******************************************************/ 
{
  char *bfld_s = get_key_word("SPEERparams", "BFLD");
  if (!bfld_s) return -2; //no Speer segment in the spec file.
  int i, j, ncat, pos;
  char nam[maxfldlen];

  /* Subroutines */
  extern int ReadExplicits(void), GenImplicits(void), CheckIncon(void),
	         PreChecks(void), PostChecks(void);

/*****************  FIX ME:  Id rather have TEA call GenBnds only once. ************/
  /* Creating the implicit bounds needs to been done only once. */
  npass++;
  if( npass > 1 ) { return 0; }


  ////char *bfld = get_key_text("SPEERparams", "BFLD");
  /* Incorporate SPEER parameters from .db/.spec file */
  char *nedff_s = get_key_word("SPEERparams", "NEDFF");
  char *totsic_s = get_key_word("SPEERparams", "TOTSIC");

  BFLD = atoi( bfld_s );
  NEDFF = atoi( nedff_s );
  TOTSIC = atoi( totsic_s );

  /* Get field names from .db/.spec file & store them in an array */
  /* Determine longest field name for check later on              */
  apop_data *Bnames_s = get_key_text("SPEERfields", NULL);
  for (i = 0; i <= BFLD-1; ++i) {
    sscanf( Bnames_s->text[i][0], "%s %d", nam, &pos );
    strcpy( bnames[pos], nam );
	if( strlen(bnames[pos]) > namlen ) { namlen = strlen(bnames[pos]); }
  }

  /* Check for potential pre-processing fatal problems. */
  PreChecks();

  /* Create output tables */
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
	            ncat+100, i, j, bnames[i], bnames[j], bnds.lower[i][j], bnds.upper[i][j]);
      }
    }
  }

  /* Check for potential post-processing fatal problems. */
  PostChecks();

  printf( " SPEER:  Implicit bounds -> table SPEERimpl. \n" );

  return 0;
}


int checks(void)
/******************************************************/ 
{
  /* Stop program if maximum # of fields is exceded. */
  Apop_stopif( BFLD > maxflds, return -1, -5,
        "**** FATAL ERROR in GenBnds:  Maximum number of fields (%d) exceded. ****\n",
 	    maxflds ); 

  /* Stop program if max length of field names is exceded. */
  Apop_stopif( namlen > maxfldlen, return -1, -5,
        "**** FATAL ERROR in GenBnds:  Maximum length of field name (%d) exceded. ****\n",
 	    maxfldlen ); 
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
  Apop_stopif( BFLD > maxflds, return, -5,
        "**** FATAL ERROR in GenBnds:  Maximum number of fields (%d) exceded. ****\n",
 	    maxflds ); 

  /* Stop program if max length of field names is exceded. */
  Apop_stopif( namlen > maxfldlen, return, -5,
        "**** FATAL ERROR in GenBnds:  Maximum length of field name (%d) exceded. ****\n",
 	    maxfldlen ); 
}


int PostChecks(void)
/******************************************************/ 
/* Potential post-processing fatal problems. */
{
  int i;
  double diff;
  #define margin .05   // margin of error

  /* Stop program if inconsistencies exist. */
  Apop_stopif( incon > 0, return, -5,
	       "**** FATAL ERROR in GenBnds:  %d inconsistent bounds exist. ****\n", 
	       incon); 

  /* Stop program if diagonals' bounds not = 1.0 */
  for (i = 1; i <= BFLD; ++i) {
    Apop_stopif( bnds.lower[i][i] != (double)1.0 | bnds.upper[i][i] != (double)1.0, return, -5,
        "**** FATAL ERROR in GenBnds:  %s/%s bounds not = 1.0 ****\n",
 	    bnames[i], bnames[i] ); 
  }

  /* Random checks. */
  diff = MIN( bnds.lower[1][2], (double).0215853 ) / MAX( bnds.lower[1][2], (double).0215853 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s lower bound not = .0215853 ****\n", 
	       bnames[1], bnames[2] ); 

  diff = MIN( bnds.upper[3][8], (double)578.9274902 ) / MAX( bnds.upper[3][8], (double)578.9274902 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s upper bound not = 578.9274902 ****\n", 
	       bnames[3], bnames[8] ); 

  diff = MIN( bnds.upper[2][6], (double)73072.4218750 ) / MAX( bnds.upper[2][6], (double)73072.4218750 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s upper bound not = 73072.4218750 ****\n", 
	       bnames[2], bnames[6] ); 

  diff = MIN( bnds.lower[2][6], (double).0001406 ) / MAX( bnds.lower[2][6], (double).0001406 );
  diff = (double)1.0 - diff;
  Apop_stopif( diff > margin, return, -5,
	       "**** FATAL ERROR in GenBnds:  %s/%s lower bound not = .0001406 ****\n", 
	       bnames[2], bnames[6] ); 
}

int ReadExplicits(void)
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
  char num[maxfldlen], den[maxfldlen];
 
  /* Get Explicit bounds from .db */
  apop_data *ExpBnds = get_key_text("ExpRatios", NULL);

  /* Parse ExpBnds string */
  for (i = 0; i <= NEDFF-1; ++i) {
    sscanf( ExpBnds->text[i][0], "%f %s %s %f", 
	      &bnds.lower[numff[i+1]][denff[i+1]], num, den, &bnds.upper[numff[i+1]][denff[i+1]] );
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

