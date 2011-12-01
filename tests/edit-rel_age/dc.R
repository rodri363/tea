library(tea)
library(mvtnorm)
library(ggplot2)
library(LearnBayes)
library(MCMCpack)
#check out library(animation) for animated gif possibilities
con <- dbConnect(dbDriver("SQLite"),"dc.db")

#set.seed(825265)
kiter <- 100
Vid <- c("serialno","sporder")
#SHOULD USE COLCLASSES HERE TO GET TRUE VALUES
DFdc <- read.csv("~/tea/data/ss08pdc.csv",colClasses="character")
DFdc[DFdc=="NaN"] <- NA
names(DFdc) <- tolower(names(DFdc))
DFdc <- subset(DFdc,rel %in% c("00","01","02","06")) #subset to only hh, spouse, and biochildren
DFdc$schl[is.na(DFdc$schl)] <- "0"
DFdc$fer[is.na(DFdc$fer)] <- "3"
Mtypes <- cbind(names(DFdc),"text")
Vnumvar <- c("agep","schl",names(DFdc)[grep("pwgt",names(DFdc))])
Mtypes[Mtypes[,1] %in% Vnumvar,2] <- "integer"
WriteTable(DFdc,"dc",con,Mtypes,c("serialno","sporder"),overwrite=TRUE)

#group by selects don't give groups where the "where" clause is false! fix that
#query <- paste("create view v as select a.*,a.hisp != '01' as ishisp,",
#	"b.eldest,b.youngest,c.hhage,c.hhsex,d.spage,d.spsex,e.sppresent,f.parent1,f.parent2",
#	"from dc as a,",
#	"(select serialno,max(agep) as eldest,min(agep) as youngest from dc where rel='02' group by serialno) as b,",
#	"(select serialno,agep as hhage,sex as hhsex from dc where rel='00' group by serialno ) as c,",
#	"(select serialno,agep as spage,sex as spsex from dc where rel='01' group by serialno ) as d,",
#	"(select serialno,max(rel='01') as sppresent from dc group by serialno) as e,",
#	"(select serialno,max(agep) as parent1,min(agep) as parent2 from dc where rel='06' group by serialno) as f",
#	"where a.serialno=b.serialno and b.serialno=c.serialno and c.serialno=d.serialno",
#	"and d.serialno=e.serialno and e.serialno=f.serialno")

query <- paste("create view v as select a.*,a.hisp != '01' as ishisp,",
	"b.hhage,b.spage,b.eldest,b.youngest,b.parent1,b.parent2,b.hhsex,b.spsex,b.sppresent",
	"from dc as a,",
	"(select serialno,",
	"max(case rel when '00' then agep else NULL end) as hhage,",
	"max(case rel when '01' then agep else NULL end) as spage,",
	"max(case rel when '00' then sex else NULL end) as hhsex,",
	"max(case rel when '01' then sex else NULL end) as spsex,",
	#not sure if I need these; can do all edits relative to hh/sp
#	"max(case rel when '02' then agep else NULL end) as eldest,",
#	"min(case rel when '02' then agep else NULL end) as youngest,",
#	"max(case rel when '06' then agep else NULL end) as parent1,",
#	"min(case rel when '06' then agep else NULL end) as parent2,",
	"max(rel='01') as sppresent",
	"from dc group by serialno) as b",
	"where a.serialno=b.serialno")

	#select max((agep+0)*nullif(rel='06',0)) as parent1,
	#min((agep+0)*nullif(rel='06',0)) as parent2,
	#from dc group by serialno
dbGetQuery(con,"drop view if exists v")
dbGetQuery(con,query)
#Vkeep <- c("serialno","sporder","agep","rel","eldest","youngest",
#	"hhage","spage","sex","hhsex","spsex","ishisp","schl")
Vkeep <- dbListFields(con,"v")
DFdc <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),"from v"))
DFo <- dbGetQuery(con,paste("select * from v"))
#randomly permute ages in each household
#permutes about 10%
fby <- function(DFsub){
	if(as.logical(rbinom(1,1,0.1))) DFsub$agep <- sample(DFsub$agep,nrow(DFsub))
	return(DFsub)
}
Lby <- by(DFdc,DFdc$serialno,fby)
DFdc <- do.call(rbind,Lby)
#update db table with bad values
UpdateTablefromDF(DFdc,"dc",con,"agep",c("serialno","sporder"))
DFdc <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),"from v"))

#Bayes regression on age on original data
#using simple regression function from LearnBayes package
#use lm to get model matrix easily
Mlm <- model.matrix(lm(agep ~ sex + rel + ishisp,data=DFo))
#draw of posterior distribution of beta/sigma
#one for each allowed iteration
Fitage <- blinreg(DFo$agep,Mlm,kiter)

#logistic regression of sex on rel
Fitsex <- glm(as.numeric(sex)-1 ~ rel, data=DFo)

#logistic regression of sex on rel, Bayes
Fitsexb <- MCMClogit(as.numeric(sex)-1 ~ rel, data=DFo)

read_spec("dc.spec")
#edit vars
Vedvar <- dbGetQuery(con,"select * from variables")$name
#function to check / synthesize a given serialno
fby <- function(DFsub){
	browser()
	print(paste("serialno:",unique(DFsub$serialno)))
	Vfail <- rep(TRUE,nrow(DFsub))
	for(rdx in 1:nrow(DFsub))
		Vfail[rdx] <- as.logical(CheckConsistency(DFsub[rdx,Vedvar],Vedvar,"passfail",con))
	itdx <- 0
	while(itdx < kiter & any(Vfail)){
		#for now we'll assume rels are in right place
		#DFsub$rel <- sample(DFsub$rel,nrow(DFsub)) #randomly swap rels
		#NOTE, IN MORE COMPLICATED EXAMPLES MAY NEED TO UPDATE AFTER EACH CHANGE
		#freq. sex imputation
		#Vp <- predict(Fitsex,newdata=DFsub) #
		#DFsub$sex <- as.character(rbinom(nrow(DFsub),1,Vp)+1)

		#to get post. pred.
		#make X matrix for HH
		#Mb <- with(DFsub,cbind(1,as.numeric(rel=="01"),as.numeric(rel=="02")))
		Mb <- with(DFsub,cbind(1,as.numeric(rel=="01"),
				as.numeric(rel=="02"),as.numeric(rel=="06")))
		#each row of MXb is the logit(p) draws for a record in the HH (if Mb is a HH matrix)
		#each col is one draw for the whole HH
		MXb <- Mb %*% t(Fitsexb)
		Mp <- 1/(1+exp(-MXb))
		#?? draw new param each iteration, or keep same one?
		#DFsub$sex <- as.character(rbinom(nrow(DFsub),1,Mp[,itdx+1])+1)
		DFsub$sex <- as.character(rbinom(nrow(DFsub),1,Mp[,1])+1)
		#set mar to correct vals
		DFsub$mar <- "5"
		DFsub$mar[DFsub$rel=="00"] <- "1"
		DFsub$mar[DFsub$rel=="01"] <- "1"
		#set fer to correct vals
		DFsub$fer <- "2"
		if("02" %in% DFsub$rel)	DFsub$fer[DFsub$sex=="2" & DFsub$rel %in% c("00","01")] <- "1"
		DFsub$fer[DFsub$sex=="1"] <- "3"

		#Mage <- with(DFsub,cbind(1,as.numeric(sex=="2"),as.numeric(rel=="01"),as.numeric(rel=="02"),ishisp))
		Mage <- with(DFsub,cbind(1,as.numeric(sex=="2"),as.numeric(rel=="01"),
				as.numeric(rel=="02"),as.numeric(rel=="06"),ishisp))
		DFsub$agep <- as.numeric(round(blinregpred(Mage,
#					Fitage)[itdx+1,],0))
					Fitage)[1,],0))
		DFsub$agep <- pmin(pmax(DFsub$agep,0),115)
		UpdateTablefromDF(DFsub,"dc",con,c("rel","agep","sex","mar","fer"),c("serialno","sporder"))
		DFsub <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),
			"from v where serialno=",pQuote(unique(DFsub$serialno))))
		for(rdx in 1:nrow(DFsub))
			Vfail[rdx] <- as.logical(CheckConsistency(DFsub[rdx,Vedvar],Vedvar,"passfail",con))
		itdx <- itdx + 1
		print(paste("   iter:",itdx))
	}
	return(DFsub)
}
#Lfix <- by(DFdc,DFdc$serialno,fby)
#DFfix <- do.call(rbind,Lfix)
Vbad <- numeric(nrow(DFdc))
for(rdx in 1:nrow(DFdc)) Vbad[rdx] <- CheckConsistency(DFdc[rdx,Vedvar],Vedvar,"passfail",con)

##compare DFdc (bad) to DFfix (good)
#DFfix$data <- "Consistent"
#DFdc$data <- "Inconsistent"
#DFo$data <- "Original"
#DFg <- rbind(DFo[c(Vkeep,"data")],DFdc,DFfix)
#
#p <- ggplot(DFg,aes(x=agep,fill=data,color=data))
#p <- p + geom_density(alpha=1/3,adjust=1.25,trim=TRUE)
#p <- p + scale_fill_brewer(pal="Dark2")
#p <- p + scale_color_brewer(pal="Dark2")
#png(file="plot.png",width=10,height=7,unit="in",res=300)
#print(p)
#dev.off()
#p1 <- p + facet_grid(rel ~ sex)
#png(file="facet.png",width=10,height=7,unit="in",res=300)
#print(p1)
#dev.off()
#p2 <- p + aes(x=hhage-spage)
#png(file="diff.png",width=10,height=7,unit="in",res=300)
#print(p2)
#dev.off()
#p3 <- p + aes(x=hhage-eldest)
#png(file="diff2.png",width=10,height=7,unit="in",res=300)
#print(p3)
#dev.off()
#
if(!interactive()) rm(list=ls(all=TRUE))
