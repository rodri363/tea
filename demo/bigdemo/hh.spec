database: demo.db
id: ANC

input {
    input file: /cenhome/rodri363/tea/data/ss10pdc.csv
#    input file: ../../data/ss10pdc.csv
    overwrite: yes
    output table: pdc
	types {
		AGEP: integer
		SPORDER: integer
		PWGTP: integer
	}
	primary key {
		SERIALNO
		SPORDER
	}
}

fields {
	AGEP int 0-115
	RELP cat 00,01,02,06
	SEX cat 1,2
	NHH int 0-10
	NSP int 0-10
	NUP int 0-10
	HHAGE int 0-115
	SPAGE int 0-115
	SPORDER int 0-10
	SPORD int 0-10
	HHSEX cat 1,2
	SPSEX cat 1,2
}

recodes{
	blah: AGEP=22
	RORD: case SPORDER when 1 then 1 when 2 then 2 else 3 end
	RMIG: case MIG when 1 then 1 else 0 end
}

group recodes {
    group id column: SERIALNO
    recodes {
		NP: max(SPORDER)
        NHH: sum(RELP='00')
        NSP: sum(RELP='01')
        NUP: sum(RELP='15')
		HHAGE: max(case RELP when '00' then AGEP end)
		SPAGE: max(case RELP when '01' then AGEP end)
		SPORD: max(case RELP when '01' then SPORDER end)
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

	#person is parent but is fewer than 15 older than householder
	RELP='06' and (AGEP - HHAGE) < 15

	#person is child but is fewer than 15 years younger than householder
	RELP='02' and (HHAGE - AGEP) < 14

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
	#RELP='15' and NSP=1 and NUP=1

	#Two or more unmarried partners
	#RELP='15' and NUP>1

	#same sex householder/spouse
	#translated as "you are a spouse and your sex is same as householder"
	#could also say hhsex = spsex; but this would make entire HH inconsistent
	RELP='01' and SEX=HHSEX
}
