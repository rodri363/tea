
database: demo.db

input{
input file: dc_pums_08.csv
outpt table: dc 
 ovewrite: y} 
  
fields { 
SCHL: int 0-24 
WAGP: real
}
impute {
  sed: 2332:
  min gruop size: 3
  draw count: 3
}
