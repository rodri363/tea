database: demo.db
id: SSN

input {
    input file: dc_pums_08.csv 
    overwrite: no
    output table: dc
	types {
		AGEP: integer
		CATAGE: integer
	}
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
    logWAGP: real
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
	SCHL > 16

	WAGP < 0
	WAGP > 600000

	logWAGP > 10
}


catagesex{
    min group size: 3
    draw count: 3
    seed:2332
    categories {
        CATAGE
        SEX
    }
}

impute{
    input table: viewdc
    output table: imputes
    paste in: catagesex

    method: hot deck
	output vars: SCHL
}



impute{
    input table: viewdc
    earlier output table: imputes
    paste in: catagesex

    method: hot deck
	output vars: SEX
}

impute{
    input table: viewdc
    earlier output table: imputes
    paste in: catagesex

    method: hot deck
	output vars: WAGP
}

impue{
    input table: viewdc
    earlier output table: imputes
    paste in: catagesex

    method: ols
	output vars: AGEP
    input vars: WAGP, SCHL
}
