database: a.dbid: idinput {    output table: hhs    overwrite: no}include: cheksfields{   include: fields}
database: a.db
id: id

input {
    output table: hhs
    overwrite: no
}

include: cheks

fields{
   include: fields
}

