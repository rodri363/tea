database: demo.db
id: SERIALNO

input {
    input file: ss10pdc.csv
    overwrite: yes
    output table: pdc
	types {
		AGEP: integer
		SPORDER: integer
	}
	primary key {
		SERIALNO
		SPORDER
	}
}

fields {
	RELP 00,01,02,06
	AGEP int 0-115
	SEX 1,2
	NHH 0-20
	NSP 0-20
	NUP 0-20
	HHAGE 0-115
	SPAGE 0-115
	SPORDER int 0-20
	SPORD 0-20
	HHSEX 1,2
	SPSEX 1,2
}

recodes {
	ID: SERIALNO||SPORDER
}

group recodes {
    group id column: SERIALNO
    recodes {
        NHH: sum(RELP='00')
        NSP: sum(RELP='01')
        NUP: sum(RELP='15')
		HHAGE: min(case RELP when '00' then AGEP end)
		SPAGE: min(case RELP when '01' then AGEP end)
		SPORD: min(case RELP when '01' then SPORDER end)
		HHSEX: cast(round(avg(case RELP when '00' then SEX end)) as integer)
		SPSEX: cast(round(avg(case RELP when '01' then SEX end)) as integer)
    }

}

checks {
	#No relationship
	RELP is null
	#No age
	AGEP is null
	#No sex
	SEX is null

	#person is householder but there is more than 1 householder
	RELP='00' and NHH > 1

	#no householder
	NHH=0

	#person is parent but is fewer than 15 older younger than householder
	RELP='06' and (AGEP - HHAGE) < 15

	#person is child but is fewer than 15 years younger than householder
	RELP='02' and (HHAGE - AGEP) < 15

	#person is a parent, householder is < 18, and person is 15 to 59 older than hh or has null age
	RELP='06' and HHAGE<18 and (((AGEP-HHAGE) between 15 and 59) or (AGEP is null))

	#householder less than 15
	#redundant given set below
	#RELP='00' and AGEP < 15

	#age range failures; will do more later
	RELP='00' and AGEP not between 15 and 115
	RELP='01' and AGEP not between 15 and 115
	RELP='02' and AGEP not between 0 and 89
	RELP='06' and AGEP not between 30 and 115

	#age/HHAGE failures; will do more later
	RELP='01' and (SEX is null or SEX='1') and (HHAGE-AGEP) not between -35 and 50
	RELP='01' and SEX='2' and (HHAGE-AGEP) not between -50 and 35
	RELP='02' and (SEX is null or SEX='1') and (HHAGE-AGEP) not between 12 and 69
	RELP='02' and SEX='2' and (HHAGE-AGEP) not between 12 and 69

	#age/HHAGE null failures; will do these later (need codes)
	#not sure these are necessary, as we'll always fill in an HH when necessary

	#person is spouse but there is more than 1 spouse
	RELP='01' and NSP > 1

   #person is a child, listed sequentially with spouse, both >15, different sex, and <7 years apart
   #the \ at the end of the line means "rule is continued on next line"
	RELP='02' and abs(SPORDER - SPORD) = 1 and AGEP > 15 and SPAGE > 15 and \
	SEX != SPSEX and abs(AGEP - SPAGE) < 7

   #One spouse and one unmarried partner
   #translated as "you are an unmarried partner and there is a spouse"
#	RELP='15' and NSP=1 and NUP=1

   #Two or more unmarried partners
#	RELP='15' and NUP>1

   #same sex householder/spouse
   #translated as "you are a spouse and your sex is same as householder"
   #could also say hhsex = spsex; but this would make entire HH inconsistent
	RELP='01' and SEX=HHSEX
}
