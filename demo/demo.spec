database: demo.db
id: serialno

input {
    input file: dc_pums_08.csv
    overwrite: no
    output table: dc
	types {
		AGEP: integer
		CATAGE: integer
	}
	primary key {
		SERIALNO
		SPORDER
	}
	indices { PUMA
              agep
    }
}

#input table: dc
#recodes (fff) {
recodes  {
	CATAGE {
		1|AGEP between 0 and 15
		2|AGEP between 16 and 64
		3|
	}
	HISPF{
		0|HISP=1 and SEX=1
		1|
	}
	CAT{
		AGEP||SEX 
	}
}

group recodes{
    group id column: sex
    recodes {
        age_by_sex: avg(agep)
    }
}

fields  {
    AGEP real 0-116
    SEX cat 0-2, NA
    state cat AL, AK, AS, AZ, AR, CA, CO, CT, DE, DC, FM, FL,   \
          GA, GU, HI, ID, IL, IN, IA, KS, KY, LA, ME, MH, MD, MA,   \
          MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, MP,   \
          OH, OK, OR, PW, PA, PR, RI, SC, SD, TN, TX, UT, VT, VI,   \
          VA, WA, WV, WI, WY, AE, AA, AE, AE, AP
    wag real
}

checks {
	AGEP+0.0 < 0 
#	SEX=0 and HISPF=0
#    sex is not null

	AGEP+0.0 > 90 => agep = 90
#AGEP  glob '[0-9]'
}

fingerprint{
    input table: viewdc
	key{
		CATAGE
		SEX
		PUMA
		HISPF
		ESR
		PWGTP
	}
	id: serialno
	frequency: 5
	combinations: 2
}
	
impute{
    input table: viewdc
    min group size: 3
    draw count: 5
    seed:2332
    id: serialno

    categories {
        agep <= 18
        agep >= 65
        sex = 0
        sex = 1
    }

	models{
		sex { method: hot deck }

        wagp { method: lognormal}

        agep { method: ols
            vars:  wagp
        }
	
	}
}

raking {
	all vars: puma|catage | rac1p

	contrasts{
#			 puma | rac1p
			 rac1p
#			 puma | catage
	}
}
