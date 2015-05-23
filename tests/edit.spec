database: test.db
id: id

input {
    input file: dc_pums_08.csv
    output table: dc
}

fields {
    agep: [.9] real
    PINCP: real
    schl: [2.1] int 0-24
    sex: int 1, 2
}

recodes {
    id: serialno*100 + sporder
}

checks {
    agep < 13 and pincp > 0
    schl>12 and agep <=14 and agep>=5 => schl = agep-5

    #13 is unlucky.
    schl=13 => schl=NULL
}

edit {
    input table: viewdc
    fill table: imp
}

impute {
    input table: viewdc
    method: normal
    categories {
        agep
    }
    vars: sex
    input fill table: imp
    fill table: imp
}

impute {
    subset: agep>=5
    input table: viewdc
    method: em
    vars: schl, agep
    input fill table: imp
    fill table: imp
}
