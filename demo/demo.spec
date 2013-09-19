database: demo.db
id: SSN
    
SPEERfields { FEMP 1
              FAPR 2
              FQPR 3
              FFBR 4
              FSLS 5
              FAET 6
              FTOT 7
              FRPT 8
              FADE 9
            }

SPEERparams { BFLD: 9
              TOTSIC: 2
              NEDFF: 12 
            }

ExpRatios {   .0212400  FEMP FAPR     .0711125        .0369900
             1.5369120  FAPR FQPR    6.8853623       3.2590401
              .1670480  FQPR FAPR     .5273000        .3068400
              .0202880  FFBR FAPR     .2717625        .0929800    
              .0019120  FSLS FAPR   17.9646122       3.7883501
              .4114320  FTOT FAET    2.7373001       1.3070800
              .0007280  FRPT FSLS     .0356625        .0071700
              .1512560  FADE FAET     .7307250        .4357100
              .0430320  FEMP FQPR     .3661875        .1237300
            13.6115752  FAPR FEMP   46.3278246      27.0331402
             2.9163521  FQPR FEMP   15.6438375       8.0822201
            37.1182704  FSLS FEMP  559.9335861     105.2866974
          }


input {
    overwrite: yes
    input file: dc_pums_08.csv
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

impute{
    input table: viewdc
    earlier output table: imputes
    paste in: catagesex

    method: ols
	output vars: AGEP
    input vars: WAGP, SCHL
}
