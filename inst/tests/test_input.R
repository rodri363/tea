library(tea)
cat(paste("database: input.db",
		"id: id",
		"",
		"input {",
		"	input file: data",
		"	overwrite: yes",
		"	output table: data",
		"	types {",
		"		i1: int",
		"		2i: int",
		"		r1: real",
		"	}",
		"	primary key {",
		"		id",
		"	}",
		"	indices {",
		"		c2",
		"	}",
		"}\n\n",
	sep="\n"),
	file="spec")


cat(paste("id,i1,2i,c1,r1,c2,c3",
"1,1,1,a,0.0,a,z",
"2,2,2,1,,1,y",
"3,,,3.14,3.14,,x",
"4,1.1,1.1,a b,a,1.1,w",
"5,hello,a,'b',0.4,a,v\n",
sep="\n"),
file="data")

readSpec("spec")
doInput()
DF <- teaTable("data")

context("Input")
test_that("Input works in R", {
	expect_true(is.integer(DF$i1))
	expect_true(is.integer(DF$"2i"))
	expect_true(is.double(DF$r1))
	expect_true(is.character(DF$c1))
	expect_true(is.character(DF$c2))
	expect_true(is.character(DF$c3))
	expect_equal(complete.cases(DF),
		c(TRUE,FALSE,FALSE,TRUE,TRUE))
	expect_equal(rev(letters)[1:nrow(DF)],as.character(DF$c3))
	expect_true("a b" %in% DF$c1)
	expect_true("3.14" %in% DF$c1)
	expect_true("1.1" %in% DF$c2)
})

cat(paste("fields{",
		"	i1 int 0-2",
		"	c3 z,y,x,w,v",
		"}\n",
		"checks{",
		"	i1 < 2 and c3='y'",
		"}\n\n",
	sep="\n"),
	file="spec",
	append=TRUE)

readSpec("spec")
doInput()
DF <- teaTable("data")
