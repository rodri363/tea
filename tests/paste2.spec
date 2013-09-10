database: demo.db
id: SERIALNO

input {
    input file: dc_pums_08.csv 
    overwrite: yes
    output table: dc
	types {
		AGEP: integer
		CATAGE: integer
	}
}

catagesex {
    categories {
        CATAGE
        SEX
    }
}

deccas {
    input table: viewdc
    min group size: 3
    draw count: 3
    seed:2332
    paste in: catagesex
}


fields  {
    AGEP: int 0-116
    SEX: cat 1, 2, NA
    state: cat AL, AK, AS, AZ, AR, CA, CO, CT, DE, DC, FM, FL,     \
          GA, GU, HI, ID, IL, IN, IA, KS, KY, LA, ME, MH, MD, MA, \
          MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, MP, \
          OH, OK, OR, PW, PA, PR, RI, SC, SD, TN, TX, UT, VT, VI, \
          VA, WA, WV, WI, WY, AE, AA, AE
    WAGP: real

    # SCHL represents "educational attainment"
    SCHL: int 0-24
}

recodes  {
	CATAGE {
		1|AGEP between 0 and 15
		2|AGEP between 16 and 64
		3|
	}

        HAS_INCOME {
       	       1 | WAGP > 0
               0 |
        }

   	SSN: SERIALNO*100+SPORDER

	logWAGP: log(WAGP+1)
}

checks {
	AGEP < 0 
	AGEP > 95 => AGEP = 95

	SCHL < 0
	SCHL > 24

	WAGP+0.0 < 0
	WAGP+0.0 > 600000

}

impute{
    paste in: deccas

    method: hot deck
	output vars: SEX
	input vars: SEX
}


impute{
    paste in: deccas

    method: hot deck
	output vars: SCHL
	input vars: SCHL
}

impute{
    input table: viewdc
    min group size: 3
    draw count: 3
    seed:2332

    paste in: catagesex

    method:ols 
	output vars:logWAGP 
	input vars:SCHL, SEX  
}


impute{
    input table: viewdc
    min group size: 3
    draw count: 3
    seed:2332

    paste in: catagesex

    method: ols
	output vars: AGEP
    input vars: WAGP, SCHL
}
