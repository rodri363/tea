database: edit_tmp.db
id: id

input {
    input file: editcheck.data
    output table: ed
}

fields{
    y1: int 1, 2, 3, 4
    y2: int 1, 2, 3, 4
}

checks {
    #sum is even
    (y1+y2) % 2 = 1

    y1=1

    #ordered
    y2 < y1 => y1=y2, y2=y1
}
