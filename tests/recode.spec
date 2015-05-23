database: recode.db
id: id

input {
    input file: dc_pums_08.csv
    output table: dc
}

fields {
    agep: real
    PINCP: real
    log_in: real
    schl: int 1-24
    has_income: int 0, 1
}

recodes {
    id: serialno*100 + sporder
    log_in: log10(PINCP+10)
}

group recodes {
   group id: serialno
   hh_in: max(case sporder when 1 then log_in else 0 end)
   house_in: sum(case when PINCP is not null \
                      and PINCP > 0 then PINCP else 0 end)
}

recodes {
    log_house_in: log10(house_in)

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
