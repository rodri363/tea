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
#is.na(DF$RELP) <- as.logical(runif(nrow(DF),0,1)>0.90)
is.na(DF$SEX) <- as.logical(runif(nrow(DF),0,1)>0.90)

#blank based on flags
#is.na(DF$AGEP) <- as.logical(DF$FAGEP=="1")
#is.na(DF$RELP) <- as.logical(DF$FRELP=="1")
#is.na(DF$SEX) <- as.logical(DF$FSEX=="1")

lsrmi <- list(vmatch=c("SERIALNO","SPORDER"),Data=DF,kloop=2,
			lform=list(AGEP ~ SCHL + RELP,SEX ~ AGEP + SCHL + RELP),
			lmodel=list(mcmc.reg,mcmc.mnl))
esrmi <- srmi.fit(lsrmi)

esrmi$Newdata <- DF
DFsyn <- srmi.draw(esrmi)
ved <- dbGetQuery(pepenv$con,"select * from variables")$name
#test consistency
Vbound <- as.logical(DFsyn$AGEP < 0 | DFsyn$AGEP > 115)
Vfail <- as.logical(CheckDF(DFsyn[!Vbound,],pepenv$con))

#CheckConsistency(DFsyn[!Vbound,ved][which(Vfail)[1],],ved,"passfail",pepenv$con)
#print(DFsyn[!Vbound,ved][which(Vfail)[1],])
#DFalt <- CheckConsistency(DFsyn[!Vbound,ved][which(Vfail)[1],],ved,"find_alternatives",pepenv$con)

while(sum(Vbound)>0 | sum(Vfail) > 0){
	print(sum(Vbound))
	print(sum(Vfail))
	DFnew <- srmi.draw(esrmi)
	DFsyn[Vbound,] <- DFnew[Vbound,]
	DFsyn[!Vbound,][which(Vfail),] <- DFnew[!Vbound,][which(Vfail),]
	Vbound <- as.logical(DFsyn$AGEP < 0 | DFsyn$AGEP > 115)
	Vfail <- as.logical(CheckDF(DFsyn[!Vbound,],pepenv$con))
}

DFo$DAT <- "Original"
DFsyn$DAT <- "Synthetic"
DFg <- rbind(DFo,DFsyn)

p <- ggplot(DFg,aes(x=AGEP,fill=DAT))
p <- p + scale_fill_brewer(pal="Dark2")
p <- p + facet_grid(SEX ~ RELP)
#p <- p + facet_grid(SEX ~ .)
p1 <- p + geom_density(alph=1/3,adjust=1.25,trim=TRUE)
p2<- p + stat_bin(binwidth=1,position="identity",alpha=1/2)
png(file="dagexsex.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p1)
dev.off()
png(file="agexsex.png",width=11*(10/11),height=8.5*(10/11),units="in",res=600)
print(p2)
dev.off()


