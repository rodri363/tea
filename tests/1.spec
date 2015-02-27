
database: demo.db

input {
    input file: dc_pums_08.csv
    output table: dc 
     overwrite: y 
 } 
  
fields { 
SCHL: int 0-24 
WAGP: real
}
impute { 
  min group size: 3
  draw count: 3
  seed: 2332
}
