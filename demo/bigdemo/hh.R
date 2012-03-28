library(tea)
library(ggplot2)
set.seed(1234567891)
con <- dbConnect(dbDriver("SQLite"),"demo.db")
#options(warn=1)
read_spec("hh.spec")
doInput()
#drop data that has values we don't have in spec yet
dbGetQuery(con,"delete from pdc where not RELP in ('00','01','02','06')")

vedvar <- c("SERIALNO",dbGetQuery(con,"select * from variables")$name)
DF <- dbGetQuery(con,"select * from viewpdc where WAGP is not null")
#to check original data for consistency failures

DFo <- DF
##randomly blank out RELP,AGEP,SEX
is.na(DF$AGEP) <- as.logical(runif(nrow(DF),0,1)<0.10)
is.na(DF$RELP) <- as.logical(runif(nrow(DF),0,1)<0.10)
is.na(DF$SEX) <- as.logical(runif(nrow(DF),0,1)<0.10)
##blank based on flags
#is.na(DF$AGEP) <- as.logical(DF$FAGEP=="1")
#is.na(DF$RELP) <- as.logical(DF$FRELP=="1")
#is.na(DF$SEX) <- as.logical(DF$FSEX=="1")

dbCommit(con)
dbGetQuery(con,"savepoint tea_save")
UpdateTablefromDF(DF,"pdc",con,c("AGEP","RELP","SEX"),c("SERIALNO","SPORDER"),verbose=TRUE)
DF <- dbGetQuery(con,"select * from viewpdc where WAGP is not null")

#first fill in data where WAG is available as a predictor
#fit SRMI model on all of DF
#TODO need to check in SRMI for missing X (covariates)
lsrmi <- list(vmatch=c("SERIALNO","SPORDER"),Data=DF,kloop=1,
			lform=list(AGEP ~ DEG + MOVE + WAGP,
#			lform=list(AGEP ~ DEG + MOVE + s(WAGP),
				RELP ~ AGEP + DEG + MOVE + EARN,
				SEX ~ RELP + AGEP + DEG + MOVE + EARN),
			con=con,kstab="viewpdc",kutab="pdc",vmatch=c("SERIALNO","SPORDER"),
			ksave="srmi_save",
			lmodel=list(mcmc.reg,mcmc.mnl,mcmc.mnl))
#			lmodel=list(teagam,mcmc.mnl,mcmc.mnl))
modsrmi <- setupRapopModel(teasrmi)
fitsrmi <- estimateRapopModel(lsrmi,modsrmi)

vsyn <- c("AGEP","SEX","RELP") #vars we are synthesizing
#fill in only records with missing
DFsyn <- dbGetQuery(con,paste("select * from viewpdc",
	"where (AGEP is null or SEX is null or RELP is null)"))

#TODO
Lconsist <- list(con=con,ktab="viewpdc",kupdate="pdc",vsyn=vsyn,
	DFsyn=DFsyn,vid=c("SERIALNO","SPORDER"),vgroup="SERIALNO",Lfit=list(fitsrmi),kmaxloop=10)
DFsyn <- consistency_draw(as.environment(Lconsist))
#Final update to get all vars for DFsyn
UpdateTablefromDF(DFsyn,"pdc",con,c("AGEP","RELP","SEX"),c("SERIALNO","SPORDER"),verbose=TRUE)
#DFsyn <- dbGetPreparedQuery(con,
#	"select * from viewpdc where SERIALNO=$SERIALNO and SPORDER=$SPORDER",
#	bind.data=DFsyn)
DFsyn <- dbGetQuery(con,"select * from viewpdc")
#rollback to very beginning
dbGetQuery(con,"rollback to tea_save")
dbGetQuery(con,"release tea_save")

#yank original data before blanking for these
#DFo <- dbGetPreparedQuery(con,
#	"select * from viewpdc where SERIALNO=$SERIALNO and SPORDER=$SPORDER",
#	bind.data=DFsyn)
DFo <- dbGetQuery(con,"select * from viewpdc")
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

if(FALSE){
}
