library(tea)
library(mvtnorm)

#create database then make views to do recodes:
#youngest biochild age
#oldest biochild age
#mother's age
#text recodes from numeric values

con <- dbConnect(dbDriver("SQLite"),"db.db")
#create table from data frame
#will assume 2 in a household, a single parent and a child
kNhh <- 10 # # of households
DFrel <- data.frame(CMID=as.vector(sapply(1:kNhh,rep,2)),
	PNUM=rep(1:2,kNhh/2),
	NREL=rep(c(1,3),kNhh/2), #either a 1 or a 3 (HH or child)
	NSEX=floor(runif(kNhh*2,0,2))+1,
	AGE=floor(runif(kNhh*2,0,76)))

Mdbtypes <- cbind(names(DFrel),"numeric")
WriteTable(DFrel,"tab",con,Mdbtypes,c("CMID","PNUM"),overwrite=TRUE)
WriteTable(DFrel,"orig",con,Mdbtypes,c("CMID","PNUM"),overwrite=TRUE)
#make recodes
read_spec("spec")
query <- paste("create view v as select a.*,",
	"case a.NREL when 1 then 'HH' when 2 then 'SP' when 3 then 'CH' else 'OTHER' end as REL,",
	"case a.NSEX when 1 then 'MALE' when 2 then 'FEMALE' else 'OTHER' end as SEX",
	"from viewdc as a") #hard-coded view table name.
dbGetQuery(con,"drop view if exists v")
dbGetQuery(con,query)
#read in spec

DFrel <- dbGetQuery(con,"select * from v")
#editing variables
#Vedvar <- c("PARENT","ELDEST","YOUNGEST")
Vedvar <- dbGetQuery(con,"select * from variables")$name
#show that things fail and the alternatives
#can graph alternative space to be fancy
#kpass <- CheckConsistency(DFrel[1,Vedvar],Vedvar,"passfail",con)
#DFalt <- CheckConsistency(DFrel[1,Vedvar],Vedvar,"find_alternatives",con)

#do imputation
#for each CMID (household)
#step 1: draw relationships from conditional bernoullis (e.g. if first is mom then 2nd can't be)
#step 2: draw ages from multivariate normal
for(rdx in 1:nrow(DFrel)){
	print(paste("Row",rdx))
	Vfail <- as.logical(c(CheckConsistency(DFrel[rdx,Vedvar],Vedvar,"passfail",con),
				CheckConsistency(DFrel[rdx,Vedvar],Vedvar,"passfail",con)))
	kfails <- any(Vfail)
	print(paste("Fails?",kfails))
	while(kfails){
		#is 1st person adult
		krel1 <- ifelse(rbinom(1,1,0.5)==0,3,1)
		krel2 <- ifelse(krel1==1,3,1)
		#round and floor/ceiling age draws
		Vage <- as.numeric(round(rmvnorm(1,mean=c((krel1==2)*30+15,(krel2==2)*30+15),
			sigma=matrix(c(1e2,.7*1e2,.7*1e2,1e2),2,2)),0))
		#Vage[Vage<0] <- 0
		#Vage[Vage>75] <- 75  
		kcmid <- DFrel$CMID[rdx]
		DFupdate <- data.frame(CMID=kcmid,PNUM=1:2,NREL=c(krel1,krel2),AGE=Vage)
		UpdateTablefromDF(DFupdate,"tab",con,c("NREL","AGE"),c("CMID","PNUM"))
		DFhh <- dbGetQuery(con,paste("select * from v where CMID =",kcmid))
		Vfail <- as.logical(c(CheckConsistency(DFhh[1,Vedvar],Vedvar,"passfail",con),
					CheckConsistency(DFhh[2,Vedvar],Vedvar,"passfail",con)))
		kfails <- any(Vfail)
	}
}
DFrel <- dbGetQuery(con,"select * from v") #get new values
DFo <- dbGetQuery(con,"select * from orig")
for(rdx in 1:nrow(DFrel)){
	print(paste("Row",rdx))
	Vfail <- as.logical(c(CheckConsistency(DFrel[rdx,Vedvar],Vedvar,"passfail",con),
				CheckConsistency(DFrel[rdx,Vedvar],Vedvar,"passfail",con)))
	kfails <- any(Vfail)
	print(paste("Fails?",kfails))
}


if(!interactive()) dbDisconnect(con)
