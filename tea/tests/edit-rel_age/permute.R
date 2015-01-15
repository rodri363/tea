library(tea)
library(mvtnorm)
library(nnet)
library(ggplot2)
library(gtools) #for combinations()
#check out library(animation) for animated gif possibilities
con <- dbConnect(dbDriver("SQLite"),"dc.db")

#set.seed(825265)
kiter <- 100
#do graphs for animated gif; requires ./graph/ directory
kgraph <- TRUE
Vid <- c("serialno","sporder")
#USE COLCLASSES HERE TO GET TRUE VALUES
DFdc <- read.csv("/cenhome/rodri363/tea/data/ss08pdc.csv",colClasses="character")
DFdc[DFdc=="NaN"] <- NA
names(DFdc) <- tolower(names(DFdc))
#subset to only hh, spouse, biochildren, and parents
DFdc <- subset(DFdc,rel %in% c("00","01","02","06"))
DFdc$sch[is.na(DFdc$sch)] <- "0"
DFdc$schl[is.na(DFdc$schl)] <- "0"
Mtypes <- cbind(names(DFdc),"text")
Vnumvar <- c("agep",names(DFdc)[grep("pwgt",names(DFdc))],"wagp","schl")
Mtypes[Mtypes[,1] %in% Vnumvar,2] <- "integer"
WriteTable(DFdc,"dc",con,Mtypes,c("serialno","sporder"),overwrite=TRUE)


query <- paste("create view v as select a.*,a.hisp != '01' as ishisp,",
	"b.hhage,b.spage,b.hhsex,b.spsex,b.hhsize,b.sppresent,b.maxage,b.minage,b.youngest,",
	"b.nhh,b.nsp",
	"from dc as a,",
	"(select serialno,",
	"sum(rel='00') as nhh,",
	"sum(rel='01') as nsp,",
	"max(case rel when '00' then agep end) as hhage,",
	"max(case rel when '01' then agep end) as spage,",
	"max(case rel when '00' then sex end) as hhsex,",
	"max(case rel when '01' then sex end) as spsex,",
	"min(case rel when '02' then agep end) as youngest,",
	"max(agep) as maxage,",
	"min(agep) as minage,",
	"count(*) as hhsize,",
	"max(rel='01') as sppresent",
	"from dc group by serialno) as b",
	"where a.serialno=b.serialno")
#NOTE
#column affinity for max() vars is NULL in sqlite
#thus type when brought into R is not guaranteed by RSQLite

dbGetQuery(con,"drop view if exists v")
dbGetQuery(con,query)
Vkeep <- dbListFields(con,"v")
DFdc <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),"from v"))
#convert characters to factors to facilitate model fitting
ffactor <- function(x){
	if(is.character(x)) return(factor(x))
	else return(x)
}
DFdc <- as.data.frame(lapply(DFdc,ffactor))
DFo <- DFdc

#randomly permute ages in each household
#permutes about 10%
fby <- function(DFsub){
	if(nrow(DFsub)>1 & as.logical(rbinom(1,1,0.9))) DFsub$agep <- sample(DFsub$agep,nrow(DFsub))
	return(DFsub)
}
Lby <- by(DFdc,DFdc$serialno,fby)
DFdc <- do.call(rbind,Lby)
#update db table with bad values
UpdateTablefromDF(DFdc,"dc",con,"agep",c("serialno","sporder"))
DFdc <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),"from v"))

read_spec("dc.spec")
#edit vars
Vedvar <- dbGetQuery(con,"select * from variables")$name

#get bad records
#remove for model fitting
Vbad <- numeric(nrow(DFdc))
for(rdx in 1:nrow(DFdc)) Vbad[rdx] <- CheckConsistency(DFdc[rdx,Vedvar],Vedvar,"passfail",con)

#function to check / synthesize a given serialno
fby <- function(DFsub){
	#have to store original values, less permutation order gets missed
	DF1 <- DFsub
	print(paste("serialno:",unique(DFsub$serialno)))
	Vfail <- rep(TRUE,nrow(DFsub))
	for(idx in 1:nrow(DFsub))
		Vfail[idx] <- as.logical(CheckConsistency(DFsub[idx,Vedvar],Vedvar,"passfail",con))
	Mperm <- permutations(nrow(DFsub),nrow(DFsub))
	#randomly swap rows of permutation so you don't get too many runs of ridiculous tries
	if(nrow(Mperm)>1) Mperm <- Mperm[sample(1:nrow(Mperm),nrow(Mperm)),]
	rdx <- 0
	while(rdx < nrow(Mperm) & any(Vfail)){
		#permute the ages until they work
		DFsub$agep <- DF1[Mperm[rdx+1,],"agep"]
		UpdateTablefromDF(DFsub,"dc",con,c("rel","agep","sex","mar","fer"),c("serialno","sporder"))
		DFsub <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),
			"from v where serialno=",pQuote(unique(DFsub$serialno))))
		print(DFsub[Vedvar])
		for(idx in 1:nrow(DFsub))
			Vfail[idx] <- as.logical(CheckConsistency(DFsub[idx,Vedvar],Vedvar,"passfail",con))
		rdx <- rdx + 1
		print(paste("   permutation:",rdx))
	}
	if(!any(Vfail)){
		UpdateTablefromDF(DFsub,"dc",con,c("rel","agep","sex","mar","fer"),c("serialno","sporder"))
		DFsub <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),
			"from v where serialno=",pQuote(unique(DFsub$serialno))))
		return(DFsub)
	}else{
		print("No feasible age permutation")
		return(DF1)
	}
}

if(FALSE){
	Lfix <- by(DFdc,DFdc$serialno,fby)
	DFfix <- do.call(rbind,Lfix)
	Vfix <- numeric(nrow(DFfix))
	for(rdx in 1:nrow(DFfix)) Vfix[rdx] <- CheckConsistency(DFfix[rdx,Vedvar],Vedvar,"passfail",con)

#Stepwise AIC selection of regression for imputation
#requires a lot of variable handling for factors
#vars with no missing
Vcomplete <- names(DFo)[unlist(lapply(DFo[Vbad==0,],function(x) return(sum(is.na(x))==0)))]
#vars we know we don't want or that are problematic (such as "youngest")
Vcomplete <- Vcomplete[-grep("agep|pwgt|serialno|sporder|st|puma|rt|adjinc|hhage|hhsex|youngest|nhh|nsp",Vcomplete)]
#remove flags
Vcomplete <- Vcomplete[-grep("^f[[:alnum:]]+p",Vcomplete)]
#vars with too many levels
Vnlev <- unlist(lapply(DFo[Vbad==0,Vcomplete],function(x) return(if(is.factor(x)) length(levels(x)))))
Vnlev[is.null(Vnlev)] <- 0
Vcomplete <- Vcomplete[Vnlev<6]
#vars that change have levels in bad data only
#needed so predict() doesn't break
Vgoodlev <- unlist(lapply(DFo[Vbad==1,Vcomplete],function(x) return(if(is.factor(x)) length(levels(x)))))
Vlev <- unlist(lapply(DFo[Vcomplete],function(x) return(if(is.factor(x)) length(levels(x)))))
Vcomplete <- Vcomplete[which((Vlev-Vgoodlev)==0)]
#formula
form.complete <- paste("agep",paste(Vcomplete,collapse="+"),sep="~")
Fitstep <- step(lm(agep ~ rel,data=DFo[Vbad==0,]),list(lower=agep~rel,upper=form.complete),direction="both")
Vstep <- all.vars(formula(Fitstep))[-1]
form.age <- as.formula(paste("agep",paste(Vstep[1:3],collapse="+"),sep="~"))
#fit models
#Fitage <- lm(agep ~ rel + sch + minage + maxage, data=DFdc[Vbad==0,])
Fitage <- lm(form.age, data=DFo[Vbad==0,])
Vfitvar <- all.vars(formula(Fitage))
ksigma <- summary(Fitage)$sigma
}

#use model instead
fmodel <- function(DFsub){
	if(nrow(DFsub)==1) return(DFsub)
	#copy factor levels from DFo
	DF1 <- DFsub
	kmaxiter <- 100
	kiter <- 0
	Vmu <- predict.lm(Fitage,newdata=DFsub)
	Vsig <- rep(ksigma,nrow(DFsub))
	print(paste("serialno:",unique(DFsub$serialno)))
	Vfail <- rep(TRUE,nrow(DFsub))
	for(idx in 1:nrow(DFsub))
		Vfail[idx] <- as.logical(CheckConsistency(DFsub[idx,Vedvar],Vedvar,"passfail",con))
	while(kiter < kmaxiter & any(Vfail)){
		#model ages until they work
		#draw a normal from fit and se.fit
		DFsub$agep <- pmax(0,pmin(115,floor(rnorm(nrow(DFsub),Vmu,Vsig))))
		UpdateTablefromDF(DFsub,"dc",con,c("rel","agep","sex","mar","fer"),c("serialno","sporder"))
		if(kgraph){
			#get data to graph
			DFgraph <- dbGetQuery(con,"select serialno,agep,rel,hhage,youngest,hhsize from v")
			DFgraph$ishh <- as.character(as.numeric(DFgraph$serialno==DFsub$serialno[1]))
			png(file=paste("./graph/plot.",paste(DFsub$serialno[1],kiter,sep="."),".png",sep=""),
				width=1024,height=768)
			grid.newpage()
			pushViewport(viewport(layout=grid.layout(2,1)))
			p <- ggplot(DFgraph,aes(x=hhage,y=as.numeric(youngest),color=ishh,size=as.numeric(hhsize)))
			p <- p + scale_color_brewer(pal="Dark2")
			p <- p + geom_point()
			p <- p + coord_cartesian(xlim=c(0,115),ylim=c(0,115))
			print(p,vp=viewport(layout.pos.row=1,layout.pos.col=1))
			p2 <- ggplot(DFgraph,aes(x=agep,fill=rel))
			p2 <- p2 + scale_fill_brewer(pal="Dark2")
			p2 <- p2 + coord_cartesian(xlim=c(0,115),ylim=c(0,0.05))
			p2 <- p2 + geom_density(alpha=1/3,adjust=1.25,trim=TRUE)
			print(p2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
			dev.off()
		}
		DFsub <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),
			"from v where serialno=",pQuote(unique(DFsub$serialno))))
		print(DFsub[Vedvar])
		for(idx in 1:nrow(DFsub))
			Vfail[idx] <- as.logical(CheckConsistency(DFsub[idx,Vedvar],Vedvar,"passfail",con))
		kiter <- kiter + 1
		print(paste("   iteration:",kiter))
	}
	if(!any(Vfail)){
		UpdateTablefromDF(DFsub,"dc",con,c("rel","agep","sex","mar","fer"),c("serialno","sporder"))
		DFsub <- dbGetQuery(con,paste("select",paste(Vkeep,collapse=","),
			"from v where serialno=",pQuote(unique(DFsub$serialno))))
		return(DFsub)
	}else{
		print("No valid values drawn")
		return(DF1)
	}
}
if(FALSE){
Lmod <- by(DFdc,DFdc$serialno,fmodel)
DFmod <- do.call(rbind,Lmod)
Vmod <- numeric(nrow(DFmod))
for(rdx in 1:nrow(DFmod)) Vmod[rdx] <- CheckConsistency(DFmod[rdx,Vedvar],Vedvar,"passfail",con)

#compare DFdc (bad) to DFfix (good)
#DFfix$data <- "Consistent"
DFmod$data <- "Model"
DFdc$data <- "Inconsistent"
DFo$data <- "Original"
DFg <- rbind(DFo,DFdc,DFmod)

p <- ggplot(subset(DFg,rel != "06"),aes(x=agep,fill=data,color=data))
p <- p + geom_density(alpha=1/3,adjust=1.25,trim=TRUE)
p <- p + scale_fill_brewer(pal="Dark2")
p <- p + scale_color_brewer(pal="Dark2")
#png(file="plot.png",width=10,height=7,unit="in",res=300)
png(file="plot.png",width=800,height=600)
print(p)
dev.off()
p1 <- p + facet_grid(rel ~ sex)
#png(file="facet.png",width=10,height=7,unit="in",res=300)
png(file="facet.png",width=800,height=600)
print(p1)
dev.off()

if(!interactive()) save.image(file="out.RData")
if(!interactive()) rm(list=ls(all=TRUE))
}
