
databas: demo.db

input {
input file: dc_pums_08.csv
output table: dc 
 ovewrite: y 
 } 
  
fields { 
AGEP: int 0-116} 

checks {
AGEP < 0 

AGEP > 95 => AGEP = 95 }
