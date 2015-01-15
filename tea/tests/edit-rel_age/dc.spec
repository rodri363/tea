database: dc.db
timeout: 60
input{
	output table: dc
	overwrite: no
}

fields{
	sex: 1,2
	hhsex: 1,2
	spsex: 1,2

	rel: 00,01,02,06
	mar: 1-5
	fer: 1-2

	agep: int 0-115
	hhage: int 0-115
	spage: int 0-115
	sppresent: 0,1
}

#group recodes {
#    group id column: CMID
#    recodes {
#        ELDEST: max(age *(nrel==3))
#        YOUNGEST: min(age +1000*(nrel!=3))
#        PARENT: min(age + 1000*(nrel!=1 and nrel!=2 ))
#    }
#
#}
#
checks{
	# 1: HH under 15
	hhage < 15
	# 2: SP under 15
	spage < 15
	# 3: SP not married
	rel='01' and not mar='1'
	# 4: SP present and HH not married
	sppresent='1' and rel='00' and not mar='1'
	# 5: SP present and HH and SP of same sex
	hhsex = spsex
	# 6: Person less than 15 years old married
	agep < 15 and mar='1'
	# 7: Male with fertility
	sex='1' and fer='1'
	# 8: Female less than 15 years old with fertility
	sex='2' and agep < 15 and fer='1'
	# 9: Female 15 or over should with blank fertility
	sex='2' and agep >= 15 and fer is null
	#12: Child older than HH
	#rel='02' and agep > hhage
	#replacing this rule with two rules below
	#child must be at least 12 years younger than HH or spouse
	rel='02' and (hhage - agep) < 12
	rel='02' and (spage - agep) < 12
	#11: Parent older than HH
	#rel='06' and agep < hhage
	#replace with rule below
	#parent must be at least 12 years older than HH
	rel='06' and (agep - hhage) < 12
}
