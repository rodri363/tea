database: test.db
id: id
seed:2332

input {input file: testin
    overwrite: no
    output table: dd
types {
age: numeric
}
}

fields  {
#    age real 
    age 0-120
    sex cat 0, 1
    status cat married, single, divorced, complicated
    state cat AL, AK, AS, AZ, AR, CA, CO, CT, DE, DC, FM, FL,   \
          GA, GU, HI, ID, IL, IN, IA, KS, KY, LA, ME, MH, MD, MA,   
          MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, MP,   \
          OH, OK, OR, PW, PA, PR, RI, SC, SD, TN, TX, UT, VT, VI,   \
          VA, WA, WV, WI, WY, AE, AA, AE, AE, AP
}


checks {
    age <15 and status = 'married'
    age > 90 => age = 90
}

impute{
#input table: viewdc
    min group size: 1
    draw count: 5

#    categories {
#        agep <= 18
#        agep > 18 and agep < 65
#        agep >= 65
#        sex = 0
#        sex = 1
#    }

models{
sex { method: hot deck }

        age { method: ols 
            vars: sex}
       status  { method: hot deck }

}
}

