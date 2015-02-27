           database: test.db                     
           input {
               input file: onetwo.data
               output table: d 
                overwrite: y 
 }     
 
            recodes [first] {               
                abc {                       
                    A | one =1 and two=1    
                    B | one = 1 and two=2   
                    C |                     
                }                           
                                           
                def {                       
                    D | one=2 and two=1     
                    E | one=2 and two=2     
                    F | one=2 and two=3     
                    G |                     
                }                           
            }                               

            recodes [second] {              
                L21 {                       
                    L1 | abc = 'A'          
                    L2 | abc = 'B'          
                    L3 |                    
                }                           
            }                               
