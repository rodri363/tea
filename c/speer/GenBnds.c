/* GenBnds.f -- translated by f2c (version 20061008).
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
    real lower[81]	/* was [9][9] */, upper[81]	/* was [9][9] */;
} comgen_;

#define comgen_1 comgen_

/* ***  GENBNDS.F  **** */
/* ***    does not include any I/O (including reading explicit ratios ) */
/* ***          or front end ***** */
/* Subroutine */ int genbnds_(void)
{

    static integer i__;
    extern /* Subroutine */ int readpa_(void), genlim_(void), rattab_(void);

/*     ****************** */
/* *** DERIVE RATIOS FOR EACH CATEGORY. **** */
/* *** GenParams **** */
/* *** VARIABLES USED IN GENBND.FOR **** */
/* *** BASIC ITEM NAMES **** */
/* *** NUMFF() & DENFF() are pointers for reading in the lower   **** */
/* *** and upper limits of the basic items.  These pointers also **** */
/* *** correspond to the basic item names.                       **** */
    for (i__ = 1; i__ <= 2; ++i__) {
	readpa_();
	genlim_();
	rattab_();
/* L100: */
    }
    return 0;
} /* genbnds_ */

/* Subroutine */ int genlim_(void)
{

    static integer i__, j, k;
    static real temp;

/*     ***************** */
/* *** GENERATE NEW BOUNDS. **** */
/* *** MODIFY UPPER BOUNDS BASED ON OTHER RATIOS RELATIONSHIPS. **** */
/* *** GenParams **** */
/* *** VARIABLES USED IN GENBND.FOR **** */
/* *** BASIC ITEM NAMES **** */
/* *** NUMFF() & DENFF() are pointers for reading in the lower   **** */
/* *** and upper limits of the basic items.  These pointers also **** */
/* *** correspond to the basic item names.                       **** */
    for (i__ = 1; i__ <= 9; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    if (comgen_1.upper[i__ + j * 9 - 10] < 99999.f) {
		for (k = 1; k <= 9; ++k) {
		    if (comgen_1.upper[i__ + j * 9 - 10] * comgen_1.upper[k + 
			    i__ * 9 - 10] < 99999.f) {
			temp = comgen_1.upper[i__ + j * 9 - 10] * 
				comgen_1.upper[k + i__ * 9 - 10];
			if (temp < comgen_1.upper[k + j * 9 - 10]) {
			    comgen_1.upper[k + j * 9 - 10] = temp;
			}
		    }
/* L100: */
		}
	    }
/* L200: */
	}
    }
/* *** MODIFY LOWER BOUNDS. **** */
    for (i__ = 1; i__ <= 9; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    if (comgen_1.upper[j + i__ * 9 - 10] >= 99999.f) {
		temp = 0.f;
	    } else {
		temp = 1.f / comgen_1.upper[j + i__ * 9 - 10];
	    }
	    if (temp > comgen_1.lower[i__ + j * 9 - 10]) {
		comgen_1.lower[i__ + j * 9 - 10] = temp;
	    }
/* L300: */
	}
    }
    return 0;
} /* genlim_ */

/* Subroutine */ int rattab_(void)
{

    static integer i__, j, incon;

/*     ***************** */
/* *** WRITE OUT FINAL BOUNDS. **** */
/* *** CHECK FOR BOUNDS INCONSISTENCIES. **** */
/* *** GenParams **** */
/* *** VARIABLES USED IN GENBND.FOR **** */
/* *** BASIC ITEM NAMES **** */
/* *** NUMFF() & DENFF() are pointers for reading in the lower   **** */
/* *** and upper limits of the basic items.  These pointers also **** */
/* *** correspond to the basic item names.                       **** */
    incon = 0;
    for (i__ = 1; i__ <= 9; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    if (i__ == j && comgen_1.lower[i__ + j * 9 - 10] == 
		    comgen_1.upper[i__ + j * 9 - 10]) {
		goto L200;
	    }
	    if (comgen_1.lower[i__ + j * 9 - 10] >= comgen_1.upper[i__ + j * 
		    9 - 10]) {
		++incon;
	    }
L200:
	    ;
	}
    }
    return 0;
} /* rattab_ */

/* Subroutine */ int readpa_(void)
{
    /* Initialized data */

    static integer numff[12] = { 1,2,3,4,5,7,8,9,1,2,3,5 };
    static integer denff[12] = { 2,3,2,2,2,6,5,6,3,1,1,1 };

    static integer i__, j;
    static real temp;

/*     ***************** */
/* *** DEFINE ALL EXPLICIT BOUNDS BY READING IN EXISTING BOUNDS **** */
/* *** AND FINDING THEIR INVERSE.                               **** */
/* *** INITIALIZE BOUNDS FOR EACH CATEGOR **** */
/* *** GenParams **** */
/* *** VARIABLES USED IN GENBND.FOR **** */
/* *** BASIC ITEM NAMES **** */
/* *** NUMFF() & DENFF() are pointers for reading in the lower   **** */
/* *** and upper limits of the basic items.  These pointers also **** */
/* *** correspond to the basic item names.                       **** */
    for (i__ = 1; i__ <= 9; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    comgen_1.lower[i__ + j * 9 - 10] = 0.f;
	    comgen_1.upper[i__ + j * 9 - 10] = 99999.9f;
/* L100: */
	}
    }
/* *** INITIALIZE IDENTITY DIAGONAL. **** */
    for (i__ = 1; i__ <= 9; ++i__) {
	comgen_1.lower[i__ + i__ * 9 - 10] = 1.f;
	comgen_1.upper[i__ + i__ * 9 - 10] = 1.f;
/* L200: */
    }
/* ********************************** */
/* *** READ EXPLICIT RATIOS HERE **** */
/* ********************************** */
/* *** ASSIGN THE EXISTING RATIOS' INVERSE LOWER BOUND TO THE **** */
/* *** INVERSE RATIOS' UPPER BOUND, AND THE EXISTING RATIOS'  **** */
/* *** INVERSE UPPER BOUND TO THE INVERSE RATIOS' LOWER BOUND **** */
    if (comgen_1.lower[numff[i__ - 1] + denff[i__ - 1] * 9 - 10] > 0.f) {
	temp = 1.f / comgen_1.lower[numff[i__ - 1] + denff[i__ - 1] * 9 - 10];
    } else {
	temp = 99999.8f;
    }
    if (temp < comgen_1.upper[denff[i__ - 1] + numff[i__ - 1] * 9 - 10]) {
	comgen_1.upper[denff[i__ - 1] + numff[i__ - 1] * 9 - 10] = temp;
    }
    if (comgen_1.upper[numff[i__ - 1] + denff[i__ - 1] * 9 - 10] < 99999.9f) {
	temp = 1.f / comgen_1.upper[numff[i__ - 1] + denff[i__ - 1] * 9 - 10];
    } else {
	temp = 0.f;
    }
    if (temp > comgen_1.lower[denff[i__ - 1] + numff[i__ - 1] * 9 - 10]) {
	comgen_1.lower[denff[i__ - 1] + numff[i__ - 1] * 9 - 10] = temp;
    }
/* L600: */
    return 0;
} /* readpa_ */

