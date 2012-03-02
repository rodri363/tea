library(tea)
library(ggplot2)
read_spec("hh.spec")
doInput()
dbGetQuery(pepenv$con,"drop index if exists IDX")
dbGetQuery(pepenv$con,"create index IDX on pdc(SERIALNO,SPORDER)")
DF <- dbGetQuery(pepenv$con,"select * from viewpdc where RELP in ('00','01','02','06')")
#remove na schl
DF$SCHL[is.na(DF$SCHL)] <- "00"
#remove leading zeros from RELP, due to naming issues in predicted values from MCMCmnl
#HATE THIS
#DF$RELP <- substr(DF$RELP,2,2)

DFo <- DF
#randomly blank out RELP,AGEP,SEX
is.na(DF$AGEP) <- as.logical(runif(nrow(DF),0,1)>0.90)
is.na(DF$RELP) <- as.logical(runif(nrow(DF),0,1)>0.90)
is.na(DF$SEX) <- as.logical(runif(nrow(DF),0,1)>0.90)

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
			lform=list(AGEP ~ SCHL,SEX ~ AGEP + SCHL, RELP ~ SEX + AGEP + SCHL),
			lmodel=list(mcmc.reg,mcmc.mnl,mcmc.mnl))
#			lform=list(AGEP ~ SCHL + RELP,SEX ~ AGEP + SCHL + RELP),
#			lmodel=list(mcmc.reg,mcmc.mnl))
modsrmi <- setupRapopModel(teasrmi)
fitsrmi <- estimateRapopModel(lsrmi,modsrmi)

#subset to synthesize is anyone missing age or sex
#DF[vsub,] is original data to replace
vsub <- is.na(DF$AGEP)|is.na(DF$SEX)
#everyone starts off "bad"
vbad <- 1:nrow(DF[vsub,])
while(length(vbad)>0){
	print(length(vbad))	
	#set Newdata to all "bad" records
	fitsrmi$env$Newdata <- DF[vsub,][vbad,]
	DFsyn <- RapopModelDraw(fitsrmi)
	#nrow(DFsyn) == length(vbad) here
	#keep previous bad indices
	#find bound failures
	vbound <- as.logical(DFsyn$AGEP < 0 | DFsyn$AGEP > 115)
	#find edit fails on in-bounds records
	vfail <- rep(FALSE,length(vbound))
	vfail[!vbound] <- as.logical(CheckDF(DFsyn[!vbound,],pepenv$con))
	DF[vsub,][vbad[!(vbound|vfail)],c("AGEP","SEX","RELP")] <- DFsyn[!(vbound|vfail),c("AGEP","SEX","RELP")]
	vbad <- vbad[vbound|vfail]
}

#weighted graphs
DFo$DAT <- "Original"
DF$DAT <- "Synthetic"
DFg <- rbind(DFo,DF)
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
p <- p + facet_grid(SEX ~ .)
p1 <- p + geom_density(alph=1/3,adjust=1.25,trim=TRUE)
p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
png(file="dageXsex.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p1)
dev.off()
png(file="ageXsex.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p2)
dev.off()
if(FALSE){
}
