database: db
id: id

input {
    input file: em_imputation.data
    output table: t
    overwrite: yes
}

fields {
    y1: int 1, 2
    y2: int 1, 2
}

impute {
    draw count: 20000
    weights:weights
    input table: t
    output table: i
    vars: y1, y2
    method: rake
}
