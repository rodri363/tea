database: Reg.db
input{
	input file: Reg.csv
	output table: Reg
    overwrite: yes
	primary key: ID
	types{
		x: real
		y2: real
	}
}
recodes{
	catx{
		1|x < 35
		2|x between 35 and 50
		3|
	}
}

fields{
    x 0-51
    y1 0-4
    y3 0-3
}

checks{
	x = 51
	y1 = 4
	y3 = 3
	y1 = 3 and y3 = 2
	y1 in (2,3) and x between 25 and 35
}

Regression{
#input table: viewReg
	by: geo
	bayes: 1
	ncore: 2
	models{
		x{
			flag: FLAG
			model: gam
			predictors: y1
			check: 51
		}
		y1{
			flag: FLAG
			model: polr
			predictors: x
			check: 4
		}
		y3{
			flag: FLAG
			model: multinom
			predictors: 1
			check: 3
		}
	}
}
