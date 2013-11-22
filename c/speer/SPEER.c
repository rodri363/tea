/* SPEER.c */
/* *** THE SUBROUTINES IN THIS FILE WILL BE USED FOR **** */
/* *** EVERY APPLICATION OF SPEER                    **** */

#include <stdio.h>
#include <apop.h>
#include <string.h>
//// #include <stdlib.h>
#include <sqlite3.h>

/* global constants */
//// #define BFLD 9
//// #define NEDIT ( BFLD+1 ) * BFLD / 2                 
//// #define TOTSIC 2
/* global constants */
#define maxflds 100     /* maximum # of basic items/field */
#define maxfldlen 30    /* maximum field name length */
#define maxexps 100     /* maximum # of Explicit ratios (per category) */
#define maxcats 100     /* maximum # of ratio categories */
#define NEDIT ( maxflds+1 ) * maxflds / 2
int BFLD;	            /* # of basic items */
char bnames[maxfldlen][maxflds];  /* Basic item names [name length][# of fields] */
float bwgt[maxflds];    /* basic item weights */
int TOTSIC;             /* # of categories of ratios */

/* global variables */
float basitm[maxflds];  /* basic items */
int namlen;             /* Length of longest basic item name */
int numsic;
int nblank;	            /* # of blank basic items */ 
int nf; 	            /* # of failed (deleted) basic items */
int numdel; 
int cntdel[maxflds]; 
int frcomp[NEDIT][maxcats];

extern int speer_(void);
extern apop_data *get_key_text( char*, char* );
extern char *get_key_word( char*, char* );

/* Implicit ratios and their multipliers */
struct { float lwbd[maxcats][maxflds][maxflds]; float upbd[maxcats][maxflds][maxflds];
         float bm[maxflds][maxflds]; float tm[maxflds][maxflds];
       } bnds;

/*struct { double basitm[maxflds];
         int nblank; int nf; int numdel; int numsic;
         int cntdel[maxflds]; int frcomp[NEDIT][maxcats];
       } commed;
*/


int speer_(void)
/******************************************************/ 
/**** THIS IS THE DRIVER PORTION OF THE SPEER EDIT ****/
{ 
  int i, pos;
  float wgt;
  char nam[maxfldlen];

  extern int edchek_(void), locate_(void), impsub_(void), PreChex(void);

/* TeaKEY( SPEERparams/BFLD, <<< BFLD = # of basic items >>>)
   TeaKEY( SPEERparams/NEDFF, <<< NEDFF = # of explicit ratios per category >>>)
   TeaKEY( SPEERparams/TOTSIC, <<< TOTSIC = # of explicit ratios per category >>>)
*/  
  /* Incorporate SPEER parameters from .db/.spec file */
  char *bfld_s = get_key_word("SPEERparams", "BFLD");
  //// if (!bfld_s) return -2; //no Speer segment in the spec file.
  char *totsic_s = get_key_word("SPEERparams", "TOTSIC");

  BFLD = atoi( bfld_s );
  TOTSIC = atoi( totsic_s );

  /* Check for potential pre-processing fatal problems. */
  PreChex();

  /* TeaKEY( SPEERfields, <<< Basic item names:  listed in imputation order. >>>) */
  /* TeaKEY( SPEERfields, <<< Basic item weights:  used in subroutine Locate_. >>>) */
  /* Get field names from .db/.spec file & store them in an array */
  /* Determine longest field name for check later on              */
  apop_data *Bnames_s = get_key_text("SPEERfields", NULL);
  namlen = 0;
  for (i = 0; i <= BFLD-1; ++i) {
    sscanf( Bnames_s->text[i][0], "%s %d %f", nam, &pos, &wgt );
    strcpy( bnames[pos], nam );
	if( strlen(bnames[pos]) > namlen ) { namlen = strlen(bnames[pos]); }
 	bwgt[pos] = wgt;
  }

  /* Determine which fields fail edit ratios */
  edchek_();

  /* Locate basic items to be deleted and flag them */ 
  if (nf > 0) { locate_(); }
  
  /* Calcualte missing/deleted field(s)' imputation range(s) */
  if (nf > 0 || nblank > 0) { impsub_(); }

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

  nf = 0;
  nblank = 0;

  /* *** EDIT BASIC ITEMS **** */
  for (i = 1; i <= BFLD-1; ++i) {

     /* *** CHECK FOR BLANK BASIC ITEMS **** */
	 if (basitm[i] < (double)0.0) {
	    ++nblank;
	    bblank[nblank] = i;
     
	 /* *** FLAG EACH RATIO THAT FAILS AN EDIT **** */
	 } else {
	    for (j = i+1; j <= BFLD; ++j) {
		   lower = bnds.lwbd[numsic][i][j] * bnds.bm[i][j];
		   upper = bnds.upbd[numsic][i][j] * bnds.tm[i][j];
	       if (( basitm[j] == (double)0.0  
			     &&  lower > (double)0.0  &&  upper < (double)99999.0 )
			          || 
			   ( basitm[j] > (double)0.0 
			     && (lower > (double)0.0  &&  basitm[j]*lower > basitm[i] 
			     || upper < (double)99999.0  &&  basitm[j]*upper < basitm[i])))
		   {
		    ++nf;
		    frcomp[nf][1] = i;
		    frcomp[nf][2] = j;
		   }
	    }
	 }
  }

/* *** CHECK LAST BASIC ITEM FOR BLANKS **** */
  if (basitm[BFLD] < (double)0.0) {
    ++nblank;
    bblank[nblank] = BFLD;
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

  nimp = 0;
/* *** BASE THE RANGE OF THE DELETED FIELD ON ITS RELATIONSHIP **** */
/* *** TO ALL OTHER EXISTING BASIC ITEMS.                      **** */
  for (k = 1; k <= BFLD; ++k) {
	blow = (double)-1.0;
	bup = (double)99999999.9;
	if (basitm[k] <= (double)-1.0) {
	    ++nimp;

        /* *** CALCULATE IMPUTATION RANGE/REGION **** */
	    for (i = 1; i <= BFLD; ++i) {
	 	   if (basitm[i] >= (double)0.0) {
		      bl = basitm[i] * bnds.bm[k][i] * bnds.lwbd[numsic][k][i];
		      if (bl > blow) { blow = bl; }
		   }
		   if (basitm[i] > (double)0.0) {
		      bu = basitm[i] * bnds.tm[k][i] * bnds.upbd[numsic][k][i];
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
  static double wdeg[10], wmax;
  static int narcs, remoov;
  static int bdeg[10];    /* bdeg[] may actually use the '0' cell */

  numdel = 0;
  narcs = nf;

  /*****  FIX ME:  have to find a better way than looping back to L100:  *****/
  /*****           perhaps a for ()                                      *****/
L100:

  /* ZERO OUT BASIC ITEM FAILURE COUNTERS */
  for (i = 1; i <= 9; ++i) {
	bdeg[i] = 0;
	wdeg[i] = (double)0.0;
  }

  /* COUNT NO. OF TIMES A BASIC ITEM IS INVOLVED IN A RATIO FAILURE */
  tmp = nf;
  for (i = 1; i <= tmp; ++i) {
	for (j = 1; j <= 2; ++j) {
	    ++bdeg[ frcomp[i][j] ];
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
  ++numdel;
  bdel[numdel] = remoov;
  /* tmp = comed3_1.nf; */
  for (i = 1; i <= nf; ++i) {
    if (frcomp[i][1] == remoov  ||  frcomp[i][2] == remoov) {
        frcomp[i][1] = 0;
	    frcomp[i][2] = 0;
	    --narcs;
	}
  }

  /* CONTINUE TO DELETE FIELDS                    */
  /* UNTIL THEIR ARE NO MORE FAILURE RATIOS LEFT. */
  if (narcs > 0) { goto L100; }

  /* WHEN ALL ITEMS HAVE BEEN SUCCESSFULLY BEEN FLAGGED FOR  */
  /* DELETION, DELETE THEM AND COUNT THE OCCURENCE FOR EACH. */
  /* tmp = comed3_1.numdel; */
  for (i = 1; i <= numdel; ++i) {
	++cntdel[bdel[i]];
	basitm[bdel[i]] = (double)-1.0;
  }

  return 0;
}



int PreChex(void)
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
  for (i = 1; i <= numsic; ++i) {
	for (j = 1; j <= BFLD; ++j) {
	  for (k = 1; k <= BFLD; ++k) {
         /* READ(12,2000) bnds.lwbd[i][j][k], bnds.upbd[i][j][k] */
	  }
	}
  }

  return 0;
}

