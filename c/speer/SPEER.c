/* SPEER.f -- translated by f2c (version 20061008).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real basitm[9];
} comed1_;

#define comed1_1 comed1_

struct {
    real bm[81]	/* was [9][9] */;
    integer cntdel[9], cycle, frcomp[72]	/* was [36][2] */;
} comed2_;

#define comed2_1 comed2_

struct {
    real lwbd[162]	/* was [2][9][9] */;
    integer nblank, nf, numdel, numsic;
    real tm[81]	/* was [9][9] */, upbd[162]	/* was [2][9][9] */;
} comed3_;

#define comed3_1 comed3_

/* ***  SPEER.FOR  **** */
/* *** THE SUBROUTINES IN THIS FILE WILL BE USED FOR **** */
/* *** EVERY APPLICATION OF SPEER                    **** */
/* Subroutine */ int speer_(void)
{
    extern /* Subroutine */ int edchek_(void), locate_(void), impsub_(void);

/*     ********************************************************** */
/* *** THIS IS THE DRIVER PORTION OF THE SPEER EDIT **** */
/* *** INCREMENT RECORD CYCLE NUMBER -- USED IN INTERACTIVE MODE **** */
/* ***  PARAMS  **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
/* *** DECLARE VARIABLES THAT ARE USED THROUGHOUT THE PROGRAM. **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER  **** */
/* *** MOST WIDELY USED PARAMETER STATEMENTS. **** */
/* ************************************************************************ */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BASCUT( ) IN SUBROUTINE CHKSTA                  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ***      DATA STATEMENTS IN IMPSUBDATA PROC              **** */
/* ***      GO TO ( . . . ) K  IN SUBROUTINE IMPSUB         **** */
/* ***      FLGHI( ), FLGLO( ) & FLGMID( ) IN               **** */
/* ***                SUBROUTINE IMPDEF                     **** */
/* ************************************************************* */
/* *** DECLARE & INITIALIZE POINTERS FOR BASIC ITEM ARRAY. **** */
/* *** THIS IS THE ORDER OF IMPUTATION, ALSO.              **** */
    ++comed2_1.cycle;
/* ************************************ */
/*    START SPEER PROCESSING HERE    * */
/* ************************************ */
    edchek_();
    if (comed3_1.nf > 0) {
	locate_();
    }
    if (comed3_1.nf > 0 || comed3_1.nblank > 0) {
	impsub_();
    }
    return 0;
} /* speer_ */

/* Subroutine */ int edchek_(void)
{
    static integer i__, j;
    static real lower, upper;
    static integer bblank[9];

/*     ********************************************************** */
/* *** DETERMINE WHICH FIELDS FAIL EDIT RATIOS AND FLAG THOSE FIELDS **** */
/* ***  PARAMS  **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
/* *** DECLARE VARIABLES THAT ARE USED THROUGHOUT THE PROGRAM. **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER  **** */
/* *** MOST WIDELY USED PARAMETER STATEMENTS. **** */
/* ************************************************************************ */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BASCUT( ) IN SUBROUTINE CHKSTA                  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ***      DATA STATEMENTS IN IMPSUBDATA PROC              **** */
/* ***      GO TO ( . . . ) K  IN SUBROUTINE IMPSUB         **** */
/* ***      FLGHI( ), FLGLO( ) & FLGMID( ) IN               **** */
/* ***                SUBROUTINE IMPDEF                     **** */
/* ************************************************************* */
/* *** DECLARE & INITIALIZE POINTERS FOR BASIC ITEM ARRAY. **** */
/* *** THIS IS THE ORDER OF IMPUTATION, ALSO.              **** */
    comed3_1.nf = 0;
    comed3_1.nblank = 0;
/* *** EDIT BASIC ITEMS **** */
    for (i__ = 1; i__ <= 8; ++i__) {
/* *** CHECK FOR BLANK BASIC ITEMS **** */
	if (comed1_1.basitm[i__ - 1] < 0.f) {
	    ++comed3_1.nblank;
	    bblank[comed3_1.nblank - 1] = i__;
/* *** FLAG EACH RATIO THAT FAILS AN EDIT **** */
	} else {
	    for (j = i__ + 1; j <= 9; ++j) {
		lower = comed3_1.lwbd[comed3_1.numsic + (i__ + j * 9 << 1) - 
			21] * comed2_1.bm[i__ + j * 9 - 10];
		upper = comed3_1.upbd[comed3_1.numsic + (i__ + j * 9 << 1) - 
			21] * comed3_1.tm[i__ + j * 9 - 10];
		if (comed1_1.basitm[j - 1] == 0.f && lower > 0.f && upper < 
			99999.f || comed1_1.basitm[j - 1] > 0.f && (lower > 
			0.f && comed1_1.basitm[j - 1] * lower > 
			comed1_1.basitm[i__ - 1] || upper < 99999.f && 
			comed1_1.basitm[j - 1] * upper < comed1_1.basitm[i__ 
			- 1])) {
		    ++comed3_1.nf;
		    comed2_1.frcomp[comed3_1.nf - 1] = i__;
		    comed2_1.frcomp[comed3_1.nf + 35] = j;
		}
/* L300: */
	    }
	}
/* L400: */
    }
/* *** CHECK LAST BASIC ITEM FOR BLANKS **** */
    if (comed1_1.basitm[8] < 0.f) {
	++comed3_1.nblank;
	bblank[comed3_1.nblank - 1] = 9;
    }
    return 0;
} /* edchek_ */

/* Subroutine */ int impsub_(void)
{

    static integer i__, k;
    static real bl, bu, bup, blow;
    static integer nimp;
    static real bwgt[9];

/*     ********************************************************** */
/* *** CALCULATE A MISSING/DELETED FIELD'S IMPUTATION RANGE AND **** */
/* *** IMPUTE A VALUE USING THAT FIELD'S IMPUTATION HIERARCHY.  **** */
/* ***  PARAMS  **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
/* *** DECLARE VARIABLES THAT ARE USED THROUGHOUT THE PROGRAM. **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER  **** */
/* *** MOST WIDELY USED PARAMETER STATEMENTS. **** */
/* ************************************************************************ */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BASCUT( ) IN SUBROUTINE CHKSTA                  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ***      DATA STATEMENTS IN IMPSUBDATA PROC              **** */
/* ***      GO TO ( . . . ) K  IN SUBROUTINE IMPSUB         **** */
/* ***      FLGHI( ), FLGLO( ) & FLGMID( ) IN               **** */
/* ***                SUBROUTINE IMPDEF                     **** */
/* ************************************************************* */
/* *** DECLARE & INITIALIZE POINTERS FOR BASIC ITEM ARRAY. **** */
/* *** THIS IS THE ORDER OF IMPUTATION, ALSO.              **** */
/* *** INITIALIZE VARIABLES **** */
/* ***  DATADECL **** */
/* *** THIS PROC CONTAINS DATA STATEMENTS USED THROUGHTOUT SPEER.       **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
    bwgt[8] = 1.6f;
    bwgt[5] = 1.4f;
    bwgt[1] = .25f;
    bwgt[0] = .06f;
    bwgt[3] = .75f;
    bwgt[2] = .3f;
    bwgt[7] = 1.9f;
    bwgt[4] = .5f;
    bwgt[6] = 1.f;
    nimp = 0;
/* *** BASE THE RANGE OF THE DELETED FIELD ON ITS RELATIONSHIP **** */
/* *** TO ALL OTHER EXISTING BASIC ITEMS.                      **** */
    for (k = 1; k <= 9; ++k) {
	blow = -1.f;
	bup = 99999999.9f;
	if (comed1_1.basitm[k - 1] <= -1.f) {
	    ++nimp;
/* *** CALCULATE IMPUTATION RANGE/REGION **** */
	    for (i__ = 1; i__ <= 9; ++i__) {
		if (comed1_1.basitm[i__ - 1] >= 0.f) {
		    bl = comed1_1.basitm[i__ - 1] * comed2_1.bm[k + i__ * 9 - 
			    10] * comed3_1.lwbd[comed3_1.numsic + (k + i__ * 
			    9 << 1) - 21];
		    if (bl > blow) {
			blow = bl;
		    }
		}
		if (comed1_1.basitm[i__ - 1] > 0.f) {
		    bu = comed1_1.basitm[i__ - 1] * comed3_1.tm[k + i__ * 9 - 
			    10] * comed3_1.upbd[comed3_1.numsic + (k + i__ * 
			    9 << 1) - 21];
		    if (bu < bup) {
			bup = bu;
		    }
		}
/* *** IF IMPUTATION RANGE IS NOT AVAILABLE, EXIT LOOP **** */
		if (blow >= bup) {
		    goto L800;
		}
/* L100: */
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
L800:
	;
    }
    return 0;
} /* impsub_ */

/* Subroutine */ int locate_(void)
{

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, j, bdeg[10], bdel[9];
    static real wdeg[9], bwgt[9], wmax;
    static integer narcs, remoov;

/*     ********************************************************** */
/* *** LOCATE BASIC ITEMS TO BE DELETED AND FLAG THEM. **** */
/* ***  PARAMS  **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
/* *** DECLARE VARIABLES THAT ARE USED THROUGHOUT THE PROGRAM. **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER  **** */
/* *** MOST WIDELY USED PARAMETER STATEMENTS. **** */
/* ************************************************************************ */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BASCUT( ) IN SUBROUTINE CHKSTA                  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ***      DATA STATEMENTS IN IMPSUBDATA PROC              **** */
/* ***      GO TO ( . . . ) K  IN SUBROUTINE IMPSUB         **** */
/* ***      FLGHI( ), FLGLO( ) & FLGMID( ) IN               **** */
/* ***                SUBROUTINE IMPDEF                     **** */
/* ************************************************************* */
/* *** DECLARE & INITIALIZE POINTERS FOR BASIC ITEM ARRAY. **** */
/* *** THIS IS THE ORDER OF IMPUTATION, ALSO.              **** */
/* ***  DATADECL **** */
/* *** THIS PROC CONTAINS DATA STATEMENTS USED THROUGHTOUT SPEER.       **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
    bwgt[8] = 1.6f;
    bwgt[5] = 1.4f;
    bwgt[1] = .25f;
    bwgt[0] = .06f;
    bwgt[3] = .75f;
    bwgt[2] = .3f;
    bwgt[7] = 1.9f;
    bwgt[4] = .5f;
    bwgt[6] = 1.f;
    comed3_1.numdel = 0;
    narcs = comed3_1.nf;
/* *** ZERO OUT BASIC ITEM FAILURE COUNTERS **** */
L100:
    for (i__ = 1; i__ <= 9; ++i__) {
	bdeg[i__] = 0;
	wdeg[i__ - 1] = 0.f;
/* L200: */
    }
/* *** COUNT NO. OF TIMES A BASIC ITEM IS INVOLVED IN A RATIO **** */
/* *** FAILURE.                                               **** */
    i__1 = comed3_1.nf;
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (j = 1; j <= 2; ++j) {
	    ++bdeg[comed2_1.frcomp[i__ + j * 36 - 37]];
/* L300: */
	}
    }
/* *** CALCULATE EACH WEIGHTED DEGREE              **** */
/* *** DETERMINE WHICH BASIC ITEM HAS LARGEST WDEG **** */
    wmax = 0.f;
    for (i__ = 1; i__ <= 9; ++i__) {
	wdeg[i__ - 1] = bwgt[i__ - 1] * bdeg[i__];
	if (wdeg[i__ - 1] > 0.f && wdeg[i__ - 1] >= wmax) {
	    wmax = wdeg[i__ - 1];
	    remoov = i__;
	}
/* L400: */
    }
/* *** FLAG THE BASIC ITEM TO BE DELETED AND REMOVE ALL FAILURE **** */
/* *** RATIOS ATTACHED TO IT.                                   **** */
    ++comed3_1.numdel;
    bdel[comed3_1.numdel - 1] = remoov;
    i__1 = comed3_1.nf;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (comed2_1.frcomp[i__ - 1] == remoov || comed2_1.frcomp[i__ + 35] ==
		 remoov) {
	    comed2_1.frcomp[i__ - 1] = 0;
	    comed2_1.frcomp[i__ + 35] = 0;
	    --narcs;
	}
/* L500: */
    }
/* *** CONTINUE TO DELETE FIELDS UNTIL THEIR ARE NO MORE FAILURE **** */
/* *** RATIOS LEFT.                                              **** */
    if (narcs > 0) {
	goto L100;
    }
/* *** WHEN ALL ITEMS HAVE BEEN SUCCESSFULLY BEEN FLAGGED FOR **** */
/* *** DELETION, DELETE THEM AND COUNT THE OCCURENCE FOR EACH.**** */
    i__1 = comed3_1.numdel;
    for (i__ = 1; i__ <= i__1; ++i__) {
	++comed2_1.cntdel[bdel[i__ - 1] - 1];
	comed1_1.basitm[bdel[i__ - 1] - 1] = -1.f;
/* L600: */
    }
    return 0;
} /* locate_ */

/* Subroutine */ int readlm_(void)
{
    static integer i__, j, k;

/*     ********************************************************** */
/* *** READ AND STORE ALL LOWER & UPPER IMPLICIT RATIOS AND CENTRAL VALUES **** */
/* ***  PARAMS  **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER **** */
/* *** DECLARE VARIABLES THAT ARE USED THROUGHOUT THE PROGRAM. **** */
/* *** SOME FORM OF THIS PROC WILL BE USED IN ALL APPLICATIONS OF SPEER  **** */
/* *** MOST WIDELY USED PARAMETER STATEMENTS. **** */
/* ************************************************************************ */
/* ************************************************************* */
/* *** IF THE MAJOR PARAMETER VALUES IN PARAMS PROC CHANGE, **** */
/* *** DATA STATEMENTS THROUGHOUT THE PROGRAM MUST ALSO BE  **** */
/* *** CHANGED.  THEY ARE THE FOLLOWING:                    **** */
/* ***                                                      **** */
/* *** IF THE VALUE OF BFLD CHANGES ( OR THE IMPUTATION     **** */
/* *** ORDER OF THE BASIC ITEMS CHANGE ), CHANGE DATA FOR:  **** */
/* ***      BASCUT( ) IN SUBROUTINE CHKSTA                  **** */
/* ***      BNAMES( ) IN DATADECL PROC                      **** */
/* ***      DATA STATEMENTS IN IMPSUBDATA PROC              **** */
/* ***      GO TO ( . . . ) K  IN SUBROUTINE IMPSUB         **** */
/* ***      FLGHI( ), FLGLO( ) & FLGMID( ) IN               **** */
/* ***                SUBROUTINE IMPDEF                     **** */
/* ************************************************************* */
/* *** DECLARE & INITIALIZE POINTERS FOR BASIC ITEM ARRAY. **** */
/* *** THIS IS THE ORDER OF IMPUTATION, ALSO.              **** */
/* ***  OPEN AND READ FILE CONTAINING IMPLICIT RATIOS **** */
/*     ********************************************************** */
/*      OPEN( 12, FILE = 'RATIOS.BND' ) */
/*     ********************************************************** */
    for (i__ = 1; i__ <= 2; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    for (k = 1; k <= 9; ++k) {
/*     ********************************************************** */
/* 2000       FORMAT( 30X, 2F16.7 ) */
/*            READ(12,2000) LWBD(I,J,K), UPBD(I,J,K) */
/*     ********************************************************** */
/* L200: */
	    }
	}
    }
/*     ********************************************************** */
/*      CLOSE( 12 ) */
/*     ********************************************************** */
    return 0;
} /* readlm_ */

