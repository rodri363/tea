library(tea)
library(ggplot2)
set.seed(1234567891)
con <- dbConnect(dbDriver("SQLite"),"demo.db")
#options(warn=1)
read_spec("hh.spec")
doInput()
#drop data that has values we don't have in spec yet
dbGetQuery(pepenv$con,"delete from pdc where not RELP in ('00','01','02','06')")
vedvar <- c("SERIALNO",dbGetQuery(pepenv$con,"select * from variables")$name)
DF <- dbGetQuery(pepenv$con,"select * from viewpdc")
#to check original data for consistency failures
#vfail <- CheckDF(DF,pepenv$con)

DFo <- DF
##randomly blank out RELP,AGEP,SEX
is.na(DF$AGEP) <- as.logical(runif(nrow(DF),0,1)>0.05)
is.na(DF$RELP) <- as.logical(runif(nrow(DF),0,1)>0.05)
is.na(DF$SEX) <- as.logical(runif(nrow(DF),0,1)>0.05)
##blank based on flags
#is.na(DF$AGEP) <- as.logical(DF$FAGEP=="1")
#is.na(DF$RELP) <- as.logical(DF$FRELP=="1")
#is.na(DF$SEX) <- as.logical(DF$FSEX=="1")
#insert NAs into database, otherwise no missing values to update in SRMI
UpdateTablefromDF(DF,"pdc",pepenv$con,c("AGEP","RELP","SEX"),c("SERIALNO","SPORDER"),verbose=TRUE)
DF <- dbGetQuery(pepenv$con,"select * from viewpdc")

#print(system.time(u <- CheckDF(DFo,con)))
#print(system.time(v <- .Call("r_check_a_table",DFo)))

#lgam <- list(Formula=AGEP ~ DEG,Data=DF)
#$modgam <- setupRapopModel(teagam)
#$fitgam <- estimateRapopModel(lgam,modgam)
#$debug(TEA.gam.draw)
#$u <- TEA.gam.draw(fitgam$env)


#fit SRMI model on all of DF
lsrmi <- list(vmatch=c("SERIALNO","SPORDER"),Data=DF,kloop=1,
			lform=list(AGEP ~ DEG + MOVE + EARN,
				RELP ~ AGEP + DEG + MOVE + EARN,
				SEX ~ RELP + AGEP + DEG + MOVE + EARN),
#			lform=list(AGEP ~ DEG,
#				RELP ~ AGEP + DEG,
#				SEX ~ RELP + AGEP + DEG),
			kdb="demo.db",kstab="viewpdc",kutab="pdc",vmatch=c("SERIALNO","SPORDER"),
			ksave="srmi_save",
#			lmodel=list(mcmc.reg,mcmc.mnl,mcmc.mnl))
			lmodel=list(teagam,mcmc.mnl,mcmc.mnl))
modsrmi <- setupRapopModel(teasrmi)
#problem comes due to missing SCHL for infants... need to check in SRMI for missing X (covariates)
#srmi.est(as.environment(lsrmi))
fitsrmi <- estimateRapopModel(lsrmi,modsrmi)

vsyn <- c("AGEP","SEX","RELP") #vars we are synthesizing
DFsyn <- dbGetQuery(pepenv$con,paste("select * from viewpdc",
	"where (AGEP is null or SEX is null or RELP is null)"))
Lconsist <- list(kdb="demo.db",ktab="viewpdc",kupdate="pdc",vsyn=vsyn,
	DFsyn=DFsyn,vid=c("SERIALNO","SPORDER"),vgroup="SERIALNO",Lfit=list(fitsrmi),kmaxloop=10)

#TODO
#see why we're still getting some inconsistent ages at the end of this
DFsyn <- consistency_draw(as.environment(Lconsist))

if(FALSE){
#remove na schl
#write IDs to a table
dbWriteTable(pepenv$con,"syntemp",DFsyn[,c("SERIALNO","SPORDER")],row.names=FALSE,overwrite=TRUE)
dbGetQuery(pepenv$con,"create index syndx on syntemp(SERIALNO,SPORDER)")
kleft <- dbGetQuery(con,"select count(*) as ct from syntemp")$ct
kloop <- 0
vfail <- NULL
vbound <- NULL
while(kleft>0 & kloop<2){
	print(kleft)
	print(kloop)
	#get data from syntemp ids
	DFsyn <- dbGetQuery(pepenv$con,paste("select * from viewpdc as a, syntemp as b",
		"where a.SERIALNO=b.SERIALNO and a.SPORDER=b.SPORDER"))
	print(head(DFsyn[,vedvar]))
	#synthesize the data
	fitsrmi$env$Newdata <- DFsyn
	print("Synthesizing")
	DFsyn <- RapopModelDraw(fitsrmi)
	print("Updating")
	UpdateTablefromDF(DFsyn,"pdc",pepenv$con,vsyn,c("SERIALNO","SPORDER"),verbose=TRUE)
	#now that we've updated,
	#check records of all households with someone who was synthesized
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
	#basically: keep everything in BAD!!!
	#delete from table where not in... won't work for prep query
	#delete via select
	Vdel <- setdiff(unique(DFcheck[vbad==0,"SERIALNO"]),unique(DFcheck[vbad==1,"SERIALNO"]))
	dbGetQuery(con,"savepoint syn_del")
	if(length(Vdel)>0)	dbGetPreparedQuery(con,"delete from syntemp where SERIALNO = ?",bind.data=as.data.frame(Vdel))
	dbGetQuery(con,"release savepoint syn_del")
	print(dbGetQuery(con,"select * from syntemp limit 10"))
	kleft <- dbGetQuery(con,"select count(*) as ct from syntemp")$ct
	kloop <- kloop+1

	#weighted graphs
	DFsyn <- dbGetQuery(con,"select * from viewpdc")
	DFo$DAT <- "Original"
	DFsyn$DAT <- "Synthetic"
	DFg <- rbind(DFo,DFsyn)
	p <- ggplot(DFg,aes(x=AGEP,weight=PWGTP,fill=DAT))
	p <- p + scale_fill_brewer(pal="Dark2")
	p <- p + coord_cartesian(xlim=c(0,115),ylim=c(0,3000))
	p <- p + facet_grid(MF ~ REL)
	p1 <- p + geom_density(alph=1/3,adjust=1.25,trim=TRUE)
	p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
	png(file=paste("syn",formatC(kloop,width=2,flag="0"),"png",sep="."),
		width=1280,height=1024,units="px")
	print(p1)
	dev.off()
}

print(paste("Unable to make consistent synthetic data for",kleft,"records"))
}

#final synthetic data
#need to reset bad records to original values
#so insert from ORIGpdc back into pdc for anything
#stil in syntemp
#for now am selecting everything that wasn't still bad
vv <- c("SERIALNO","SPORDER","RELP","SEX","AGEP")
DFup <- dbGetQuery(con,paste("select",paste("a",vv,sep=".",collapse=","),
	"from ORIGpdc as a, syntemp as b",
	"where a.SERIALNO=b.SERIALNO and a.SPORDER=b.SPORDER"))
UpdateTablefromDF(DFup,"pdc",con,c("RELP","SEX","AGEP"),c("SERIALNO","SPORDER"))
DFsyn <- dbGetQuery(con,"select * from viewpdc")

save(DFo,DFsyn,file="save.RData")

#weighted graphs
DFo$DAT <- "Original"
DFsyn$DAT <- "Synthetic"
DFg <- rbind(DFo,DFsyn)
p <- ggplot(DFg,aes(x=AGEP,weight=PWGTP,fill=DAT))
p <- p + scale_fill_brewer(pal="Dark2")
p <- p + facet_grid(MF ~ REL)
p1 <- p + geom_density(alph=1/2,adjust=1,trim=FALSE)
p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
png(file="dageXsexXrel.png",width=11*(10/11),height=8.5*(10/11),units="in",res=300)
print(p1)
dev.off()
png(file="ageXsexXrel.png",width=11*(10/11),height=8.5*(10/11),units="in",res=300)
print(p2)
dev.off()
p3 <- ggplot(DFg,aes(x=DAT,y=AGEP,fill=DAT)) + geom_boxplot()
p3 <- p3 + scale_fill_brewer(pal="Dark2") + facet_grid(MF ~ REL)
png(file="box.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p3)
dev.off()

