include: recode.spec

impute {
    input table: viewdc
    vars: has_income
    categories {
        age_cat
        sex
        puma   #public use microdata area
    }
    method: hot deck
    output table: has_in
}

common {
    input table: viewdc
    earlier output table: has_in
    min group size: 5
    subset: agep+0.0>15

    categories {
        has_income
        age_cat
        sex
        puma
    }
}

impute {
    vars: PINCP
    paste in: common
    method: hot deck
    fill table: hd
}

impute {
    vars: PINCP
    paste in: common
    method: lognormal
    fill table: norm
}

