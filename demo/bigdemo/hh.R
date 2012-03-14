library(tea)
library(ggplot2)
con <- dbConnect(dbDriver("SQLite"),"demo.db")
options(warn=1)
read_spec("hh.spec")
doInput()
#drop data that has values we don't have in spec yet
dbGetQuery(pepenv$con,"delete from pdc where not RELP in ('00','01','02','06') or SCHL is null")
vedvar <- c("SERIALNO",dbGetQuery(pepenv$con,"select * from variables")$name)
DF <- dbGetQuery(pepenv$con,"select * from viewpdc")
#to check original data for consistency failures
#vfail <- CheckDF(DF,pepenv$con)

DFo <- DF
#randomly blank out RELP,AGEP,SEX
is.na(DF$AGEP) <- as.logical(runif(nrow(DF),0,1)>0.50)
is.na(DF$RELP) <- as.logical(runif(nrow(DF),0,1)>0.50)
is.na(DF$SEX) <- as.logical(runif(nrow(DF),0,1)>0.50)
#insert NAs into database, otherwise no missing values to update in SRMI
UpdateTablefromDF(DF,"pdc",pepenv$con,c("AGEP","RELP","SEX"),c("SERIALNO","SPORDER"),verbose=TRUE)
DF <- dbGetQuery(pepenv$con,"select * from viewpdc")
#to check original data for consistency failures

DFc <- rbind(subset(DF,is.na(AGEP))[1,vedvar],subset(DF,AGEP==22)[1,vedvar])
u <- CheckDF(DFc,con)
v <- .Call("r_check_a_table",DFc)

if(FALSE){
#blank based on flags
#is.na(DF$AGEP) <- as.logical(DF$FAGEP=="1")
#is.na(DF$RELP) <- as.logical(DF$FRELP=="1")
#is.na(DF$SEX) <- as.logical(DF$FSEX=="1")

#ee <- as.environment(list(Formula=RELP ~ AGEP + SEX,Data=DFo))
#u <- TEA.MCMCmnl.est(ee)
#debug(TEA.MCMCmnl.draw)
#v <- TEA.MCMCmnl.draw(ee)
#
#fit SRMI model on all of DF
lsrmi <- list(vmatch=c("SERIALNO","SPORDER"),Data=DF,kloop=1,
#			lform=list(AGEP ~ SCHL,SEX ~ AGEP + SCHL, RELP ~ SEX + AGEP),
			lform=list(RELP ~ SPORDER,SEX ~ RELP + SPORDER,AGEP ~ SEX + RELP + SPORDER),
			kdb="demo.db",kstab="viewpdc",kutab="pdc",vmatch=c("SERIALNO","SPORDER"),
			ksave="srmi_save",
#			lmodel=list(mcmc.reg,mcmc.mnl,mcmc.mnl))
			lmodel=list(mcmc.mnl,mcmc.mnl,mcmc.reg))
modsrmi <- setupRapopModel(teasrmi)
#problem comes due to missing SCHL for infants... need to check in SRMI for missing X (covariates)
#srmi.est(as.environment(lsrmi))
fitsrmi <- estimateRapopModel(lsrmi,modsrmi)

#draw, check each household, flag it if it is still inconsistent
#Data for all households having any missing items
#Write serialnos of interest to new table
vsyn <- c("AGEP","SEX","RELP") #vars we are synthesizing
#Scheme
DFsyn <- dbGetQuery(pepenv$con,paste("select * from viewpdc",
	"where (AGEP is null or SEX is null or RELP is null)"))
#remove na schl
#write IDs to a table
dbWriteTable(pepenv$con,"syntemp",DFsyn[,c("SERIALNO","SPORDER")],row.names=FALSE,overwrite=TRUE)
dbGetQuery(pepenv$con,"create index syndx on syntemp(SERIALNO,SPORDER)")
kleft <- dbGetQuery(con,"select count(*) as ct from syntemp")$ct
kloop <- 0
while(kleft>0 & kloop<50){
	print(kleft)
	print(kloop)
	#get data from syntemp ids
	DFsyn <- dbGetQuery(pepenv$con,paste("select * from viewpdc as a, syntemp as b",
		"where a.SERIALNO=b.SERIALNO and a.SPORDER=b.SPORDER"))
	#synthesize the data
	fitsrmi$env$Newdata <- DFsyn
	print("Synthesizing")
	DFsyn <- RapopModelDraw(fitsrmi)
	print("Updating")
	UpdateTablefromDF(DFsyn,"pdc",pepenv$con,vsyn,c("SERIALNO","SPORDER"),verbose=TRUE)
	#now that we've updated, check records of all households with someone who was synthesized
	query <- paste("select * from viewpdc",
		"where SERIALNO in (select distinct SERIALNO from syntemp)")
	DFcheck <- dbGetQuery(pepenv$con,query)

	print("Checking Bounds")
	#find bounds errors in age, and reupdate
	#annoying that SPAGE and HHAGE are not always numeric!!!!
	Mbnd <- with(DFcheck,cbind(AGEP<0|AGEP>115,as.numeric(HHAGE)<0|as.numeric(HHAGE)>115,
		as.numeric(SPAGE)<0|as.numeric(SPAGE)>115))
	Mbnd[is.na(Mbnd)] <- FALSE
	vbound <- as.logical(rowSums(Mbnd))
	print("Checking Consistency")
	vfail <- CheckDF(DFcheck[!vbound,],pepenv$con)
	vbad <- vbound
	vbad[!vbad] <- vfail
	#any SERIALNO with any bound or consistency failures must be run through again
	#so find good serialnos, remove them from syntemp, then run again
	Vdel <- setdiff(DFcheck[vfail==0,"SERIALNO"],DFcheck[vfail==1,"SERIALNO"])
	dbGetQuery(con,"savepoint syn_del")
	if(length(Vdel)>0)	dbGetPreparedQuery(con,"delete from syntemp where SERIALNO = ?",bind.data=as.data.frame(Vdel))
	dbGetQuery(con,"release savepoint syn_del")
	kleft <- dbGetQuery(con,"select count(*) as ct from syntemp")$ct
	kloop <- kloop+1

	#weighted graphs
	DFsyn <- dbGetQuery(con,"select * from viewpdc")
	DFo$DAT <- "Original"
	DFsyn$DAT <- "Synthetic"
	DFg <- rbind(DFo,DFsyn)
	p <- ggplot(DFg,aes(x=AGEP,weight=PWGTP,fill=DAT))
	p <- p + scale_fill_brewer(pal="Dark2")
	p <- p + facet_grid(SEX ~ RELP)
	p1 <- p + geom_density(alph=1/3,adjust=1.25,trim=TRUE)
	p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
	png(file=paste("syn",kloop,"png",sep="."),width=11*(5/11),height=8.5*(5/11),units="in",res=600)
	print(p1)
	dev.off()
}

#final synthetic data
#need to reset bad records to original values, so need to save that DF
DFsyn <- dbGetQuery(con,"select * from viewpdc")
DFsyn[vfail,] <- DFo[vfail,]


#weighted graphs
DFo$DAT <- "Original"
DFsyn$DAT <- "Synthetic"
DFg <- rbind(DFo,DFsyn)
p <- ggplot(DFg,aes(x=AGEP,weight=PWGTP,fill=DAT))
p <- p + scale_fill_brewer(pal="Dark2")
p <- p + facet_grid(SEX ~ RELP)
p1 <- p + geom_density(alph=1/3,adjust=1.25,trim=TRUE)
p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
png(file="dageXsexXrel.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p1)
dev.off()
png(file="ageXsexXrel.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p2)
dev.off()

}
