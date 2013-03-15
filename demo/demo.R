library(tea)
library(ggplot2)
readSpec("demo.spec")
doMImpute()



source("rolando_plot")
plotAge("AGEP")
plotWage("WAGP")
