database: test.db
id: id

input {
    input file: dc_pums_08.csv
    output table: dc
}

fields {
    agep: real
    PINCP: real
    income: real
    log_in: real
    schl: int 0-24
    has_income: int 0, 1
    id: real
    age_cat: cat 0-3, X
}

checks {
    pincp < 0
}

recodes {
    id: serialno*100 + sporder
    log_in: log10(PINCP+10)
}

recodes {
    has_income {
        0 | PINCP==0
        1 | PINCP>0
    }

    age_cat {
        0 | AGEP <= 15
        1 | AGEP between 16 and 29
        2 | AGEP between 30 and 64
        3 | AGEP>= 65
        X |
    }
}

checks {
    has_income=0 and PINCP is null => PINCP=0

    PINCP +0.0 < 0
}

input {
    input file: fake_in.csv
    output table: fake_incomes
}

join {
    host: viewdc
    add: fake_incomes
    output table: dc_united
}

impute {
    input table: dc_united
    method: em
    vars: income, PINCP, age_cat
    fill table: via_em
    subset: agep>15
    near misses: ok
}

impute {
    categories {
        age_cat
    }
    input table: dc_united
    method: hot deck
    vars: PINCP
    subset: agep>15
    fill table: via_hot_deck
}
