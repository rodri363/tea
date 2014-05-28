/* SPEER.c */
/*   5-27-2014   */

/* This version of Tea's SPEER runs all the way thru impsub, 
     including creating imputation ranges. */

#include <stdio.h>
#include <apop.h>
#include <string.h>
//// #include <stdlib.h>
#include <sqlite3.h>

/* global constants */
//// #define NEDIT ( BFLD+1 ) * BFLD / 2                 
#define maxflds 100     /* maximum # of basic items/field */
#define maxfldlen 30    /* maximum field name length */
#define maxexps 100     /* maximum # of Explicit ratios (per category) */
#define maxcats 100     /* maximum # of ratio categories */
#define NEDIT ( maxflds+1 ) * maxflds / 2
int BFLD;	            /* # of basic items */
char bnames[maxfldlen][maxflds];  /* Basic item names [name length][# of fields] */
float bwgt[maxflds];    /* basic item weights */
int TOTSIC;             /* # of user-supplied categories of ratios */

/* global variables */
int catindx;            /* Current record's category code index/position in catlist[] */
int catlist[maxcats];   /* List of category codes */
int cntdel[maxflds]; 
int frcomp[NEDIT][maxcats];
int initSPEER = 0;      /* initialization flag */
int namlen;             /* Length of longest basic item name */
int nblank;	            /* # of blank basic items */ 
int nf; 	            /* # of failed (deleted) basic items */
int nrec;               /* record loop counter */
int numdel; 


int nSPEERpass = 0;   /************  FIX ME:  just for testing. ************/



/* global variables - record vars */
float basitm[maxflds];  /* record's basic item values */
int cat;                /* record's category code */
int ID;                 /* record's id number */

extern int speer_( int IDnum, int CategCode, float FldVals[] ), tmp_speer_driver(void);
extern apop_data *get_key_text( char*, char* );
extern char *get_key_word( char*, char* );

/* Implicit ratios and their multipliers */
struct { float lwbd[maxcats][maxflds][maxflds]; float upbd[maxcats][maxflds][maxflds];
         float bm[maxflds][maxflds]; float tm[maxflds][maxflds];
       } bnds;



int tmp_speer_driver(void)
/************  FIX ME:  just for testing. ************/
/* This sub/driver will be replaced by a call in TEA */
{
  int totrec;
  int IDnum, CategCode;
  float FldVals[maxflds];

  /* Is SPEER a stand-alone, or called for every record? */
  nSPEERpass++;
  if( nSPEERpass > 1 ) { return 0; }
  
  ////apop_query("drop table SPEERaudit;");
  ////apop_query("create table SPEERaudit( ID int, cat int, num text, numval float, den text, denval float, lwbd float, upbd float, lo float, up float );");
  apop_query("drop table SPEERedited;");
  apop_query("create table SPEERedited( ID int, cat int, b1 float, b2 float, b3 float, b4 float, b5 float, b6 float );");
  apop_query("drop table SPEERimprange;");
  apop_query("create table SPEERimprange( ID int, cat int, num text, lo float, up float );");
 

  /* Read database containing data */ 
  apop_data *rec_data = apop_query_to_text("select * from SPEERdata;");
  totrec = apop_query_to_float("select count( ID ) from SPEERdata;");

  /* Parse database text string.  Store data in variables. */
  for (nrec = 0; nrec <= totrec-1; ++nrec) {
     sscanf( rec_data->text[nrec][0], "%d", &IDnum );
     sscanf( rec_data->text[nrec][1], "%d", &CategCode );
     sscanf( rec_data->text[nrec][2], "%f", &FldVals[1] );
     sscanf( rec_data->text[nrec][3], "%f", &FldVals[2] );
     sscanf( rec_data->text[nrec][4], "%f", &FldVals[3] );
     sscanf( rec_data->text[nrec][5], "%f", &FldVals[4] );
     sscanf( rec_data->text[nrec][6], "%f", &FldVals[5] );
     sscanf( rec_data->text[nrec][7], "%f", &FldVals[6] );

     speer_( IDnum, CategCode, FldVals );
  }
}



int speer_( int IDnum, int CategCode, float FldVals[] )
/******************************************************/ 
/**** THIS IS THE DRIVER PORTION OF THE SPEER EDIT ****/
{ 
  int i, j;
  char msg[150];

  extern int edchek_(void), locate_(void), impsub_(void), InitConstants(void),
	         PreChex(void), readlm_(void);

  /* Copy record's values into global vars */
  ID = IDnum;
  cat = CategCode;
  for (i = 1; i <= sizeof(FldVals)-1; ++i) { basitm[i] = FldVals[i]; }

  ////printf( "  In speer_: %d %d %f %f %f %f %f %f \n", IDnum, CategCode, 
  ////     basitm[1], basitm[2], basitm[3], basitm[4], basitm[5], basitm[6] );

  for (i = 0; i <= maxflds-1; ++i) {
   for (j = 0; j <= maxflds-1; ++j) {
      bnds.bm[i][j] = (double)1.0;
	  bnds.tm[i][j] = (double)1.0;
  }}

  /* These subs only need to be run once per data file. */
  if( initSPEER == 0 ) { 
	InitConstants();  /* Initialize constants and arrays that drive SPEER. */
    readlm_();        /* Read and store implicit ratios */
    initSPEER = 1;
  }
 
     /* Check for potential pre-processing fatal problems. */
     PreChex();        

     /* catindx is dependent on category code of current record */
     catindx = 0;
     for (i = 1; i <= TOTSIC; ++i) {
		if( cat == catlist[i] ) { catindx = i; }
	 }

     /* Check to ensure category code is valid. */
	 if( catindx == 0 ){
        strcpy( msg, " SPEER.c:  speer_:  Invalid category code: %d \n");
		strcat( msg, "    record ID %d  not processed \n");
		printf( msg, cat, ID );
		return 0;
	 }

     /* Determine which field(s) fail edit ratios */
     edchek_();

     /* Locate basic item(s) to be deleted and flag them */ 
     if (nf > 0) { locate_(); }
  
     /* Calcualte missing/deleted field(s)' imputation range(s) */
     if (nf > 0 || nblank > 0) { impsub_(); }

     /*** Create query msg, then send basic items to database ****/
     char qury[1000] = "insert into SPEERedited values( ";
     char qury_add[20];
     sprintf( qury_add, "%d, %d", ID, cat );    // Create tmp string containing integers
     strcat( qury, qury_add );
     for (i = 1; i <= BFLD; ++i) {
       sprintf( qury_add, ", %f", basitm[i] );  // Create tmp string containing reals
       strcat( qury, qury_add );
     }
     strcat( qury, " ); " );
     apop_query( qury );                        // Send basic items to database

  return 0;
}



int edchek_(void)
/******************************************************/ 
/* *** DETERMINE WHICH FIELDS FAIL EDIT RATIOS AND  **** */
/* ***  FLAG THOSE FIELDS  **** */
{
  static int i, j;
  static double lower, upper;
  static int bblank[maxflds];

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
		   lower = bnds.lwbd[catindx][i][j] * bnds.bm[i][j];
		   upper = bnds.upbd[catindx][i][j] * bnds.tm[i][j];

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

/***** FIX ME:    KILL me -- just for testing. ************/
////if( nrec == 3 ){
////apop_query("insert into SPEERaudit values( %d, %d, '%s', %f, '%s', %f, %f, %f, %f, %f);", 
////       ID, cat, bnames[i], basitm[i], bnames[j], basitm[j], lower, upper,
////   	    basitm[j]*lower, basitm[j]*upper );
////}
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
		      bl = basitm[i] * bnds.bm[k][i] * bnds.lwbd[catindx][k][i];
		      if (bl > blow) { blow = bl; }
		   }
		   if (basitm[i] > (double)0.0) {
		      bu = basitm[i] * bnds.tm[k][i] * bnds.upbd[catindx][k][i];
		      if (bu < bup) { bup = bu; }
		   }

           /* *** IF IMPUTATION RANGE IS NOT AVAILABLE, EXIT LOOP **** */
		   if (blow >= bup) { goto L800; }
	    }

        /* Send imputation ranges for missing fields to database */
        apop_query("insert into SPEERimprange values( %d, %d, '%s', %f, %f );", 
                   ID, cat, bnames[k], blow, bup );


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



int InitConstants(void)
/*****************************************************/ 
/* Initialize constants and arrays that drive SPEER. */
{
  int i, pos;
  float wgt;
  char nam[maxfldlen];
  /* TeaKEY( SPEERparams/BFLD, <<< BFLD = # of basic items >>>)
     TeaKEY( SPEERparams/TOTSIC, <<< TOTSIC = # of explicit ratios per category >>>)
  */

  /* Incorporate SPEER parameters from .db/.spec file */
  char *bfld_s = get_key_word("SPEERparams", "BFLD");
  //// if (!bfld_s) return -2; //no Speer segment in the spec file.
  BFLD = atoi( bfld_s );

  /* Store category codes in catlist[]*/
  TOTSIC = apop_query_to_float("select count( distinct cat) from SPEERimpl;");
  apop_data *categs = apop_query_to_text("select distinct cat from SPEERimpl;");
  for (i = 1; i <= TOTSIC; ++i) {
     sscanf( categs->text[i-1][0], "%d", &catlist[i] );
  }

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
}



int locate_(void)
/******************************************************/ 
/*  LOCATE BASIC ITEMS TO BE DELETED AND FLAG THEM.   */
{
  /* System generated locals */
  int tmp;

  /* Local variables */
  static int i, j, bdel[maxflds];
  static double wdeg[maxflds], wmax;
  static int narcs, remoov;
  static int bdeg[maxflds];    /* bdeg[] may actually use the '0' cell */

  numdel = 0;
  narcs = nf;

  /*****  FIX ME:  have to find a better way than looping back to L100:  *****/
  /*****           perhaps a for ()                                      *****/
L100:

  /* ZERO OUT BASIC ITEM FAILURE COUNTERS */
  for (i = 1; i <= maxflds-1; ++i) {
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

  /****  add array bdeg[] to Tea's array of field failures  ****/
  /*** Create query msg, then send array bdeg[] to database ****/
  char qury[1000] = "insert into SPEERedited values( ";
  char qury_add[20];
  sprintf( qury_add, "%d, %d", ID, nf );    // Create tmp string containing integers
  strcat( qury, qury_add );
  for (i = 1; i <= BFLD; ++i) {
    sprintf( qury_add, ", %d", bdeg[i] );   // Create tmp string containing integers
    strcat( qury, qury_add );
  }
  strcat( qury, " ); " );
  apop_query( qury );                       // Send bdeg[] to database


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
	  " SPEER.c:  Fatal Error:  Maximum number of fields (%d) exceded. \n",
 	    maxflds ); 

  /* Stop program if max length of field names is exceded. */
  /*   Just increase the value of maxfldlen variable.      */
  Apop_stopif( namlen > maxfldlen, return 0, -5,
        " SPEER.c:  Fatal Error:  Maximum length of field name (%d) exceded. ****\n",
 	    maxfldlen ); 

  return 0;
}



int readlm_(void)
/******************************************************/ 
/* READ AND STORE ALL LOWER & UPPER IMPLICIT RATIOS */
{
  static int i, c_indx, catcode, nimpl, numpos, denpos;
  float lower, upper;
  char numer[maxfldlen], denom[maxfldlen];

  /* Read database containing implicit ratios */ 
  apop_data *implieds = apop_query_to_text("select * from SPEERimpl;");

  /* Parse database text string.  Store implicits in matricies. */
  for (nimpl = 0; nimpl <= (TOTSIC * BFLD * BFLD)-1; ++nimpl) {
     sscanf( implieds->text[nimpl][0], "%d", &catcode );
     sscanf( implieds->text[nimpl][1], "%d", &numpos );
     sscanf( implieds->text[nimpl][2], "%d", &denpos );
     sscanf( implieds->text[nimpl][3], "%s", numer );
     sscanf( implieds->text[nimpl][4], "%s", denom );
     sscanf( implieds->text[nimpl][5], "%f", &lower );
     sscanf( implieds->text[nimpl][6], "%f", &upper );

	 c_indx = 0;
     for (i = 1; i <= TOTSIC; ++i) {
		 if( catcode == catlist[i] ){ c_indx = i; }
	 } 

     //// Stop program if current rec's cat code is not in catlist[]. 
     Apop_stopif( c_indx == 0, return 0, -5,
		 " SPEER.c: readlm_: Fatal Error:  Invalid category code:  %d \n",
 	     catcode ); 

     /* Store implicits in matricies. */
     bnds.lwbd[c_indx][numpos][denpos] = lower; 
	 bnds.upbd[c_indx][numpos][denpos] = upper;
  }

  return 0;
}

