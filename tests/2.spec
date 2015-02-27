
database: demo.db

input {
inpt file: dc_pums_08.csv
output table: dc 
 overwrite: y 
 } 
  
fields { 
AGEP: int 0-116} 

recodes{
CATAGE {
1|AGEP between 0 and 15
2|AGEP between 16 and 64
3|} 
}