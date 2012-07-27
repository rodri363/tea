library(tea)
library(ggplot2)

kN <- 1000
kdebug <- FALSE
DF <- data.frame(va=rnorm(kN,0,1),
	vb=sample(letters[1:5],kN,replace=TRUE),
	vc=floor(runif(kN,0,10)),
	vd=sample(LETTERS[21:23],
	kN,replace=TRUE))

#test multinomial regression
modmnl <- setupRapopModel(mcmc.mnl)
modlist <- list(Formula=vb ~ va + vc + vd,Data=DF)
fit <- estimateRapopModel(modlist,modmnl)
print(table(DF$vb))

#new data, leaving out two of the factor levels of the response
#this should work
DFnew <- data.frame(va=rnorm(kN,0,1),
	vb=sample(letters[c(2,3,5)],kN,replace=TRUE),
	vc=floor(runif(kN,0,10)),
	vd=sample(LETTERS[21:23],kN,replace=TRUE))
fit$env$Newdata <- DFnew
fit$env$debug <- kdebug
DFnew <- RapopModelDraw(fit)
print(table(DFnew$vb))

#new data, adding in a factor level of the response
#this should also work
DFnew <- data.frame(va=rnorm(kN,0,1),
	vb=sample(letters[1:6],kN,replace=TRUE),
	vc=floor(runif(kN,0,10)),
	vd=sample(LETTERS[21:23],kN,replace=TRUE))
fit$env$Newdata <- DFnew
fit$env$debug <- kdebug
DFnew <- RapopModelDraw(fit)
print(table(DFnew$vb))

#new data, adding in a factor level of the predictor vd
#this should break
DFnew <- data.frame(va=rnorm(kN,0,1),
	vb=sample(letters[1:6],kN,replace=TRUE),
	vc=floor(runif(kN,0,10)),
	vd=sample(LETTERS[21:24],kN,replace=TRUE))
fit$env$Newdata <- DFnew
fit$env$debug <- kdebug
DFnew <- try(RapopModelDraw(fit))
if(!inherits(DFnew,"try-error")) print(table(DFnew$vb))

#lots of implicates
#new data, leaving out two of the factor levels of the response
#this should work
fit$env$Newdata <- DF
fit$env$debug <- FALSE
Mtab <- NULL
Mtab <- t(sapply(1:100,function(x) return(table(RapopModelDraw(fit)$vb))))
print(apply(Mtab,2,summary))

#test normal regression
#make an actual relationship here
Vb=sample(letters[1:5],kN,replace=TRUE)
Vc=floor(runif(kN,0,10))
Vd=sample(LETTERS[21:23],kN,replace=TRUE)
#Mmod <- model.matrix(~Vb + Vc + Vd)
Mmod <- model.matrix(~Vc)
Vbeta <- matrix(rnorm(ncol(Mmod),5,2),nrow=ncol(Mmod))
Vmu <- Mmod%*%Vbeta
Vs <- rep(Vmu[1]/2,length(Vmu))
Va <- rnorm(length(Vmu),Vmu,Vs)
DF <- data.frame(va=Va,vb=Vb,vc=Vc,vd=Vd)
modreg <- setupRapopModel(mcmc.reg)
#modlist <- list(Formula=va ~ vb + vc + vd,Data=DF)
modlist <- list(Formula=va ~ vc,Data=DF)
fit <- estimateRapopModel(modlist,modreg)

#lots of implicates
fit$env$Newdata <- DF
fit$env$debug <- FALSE
Mtab <- NULL
Ctab <- NULL
Vsig <- NULL
for(idx in 1:1000){
	#synthetic data
	DFnew <- RapopModelDraw(fit)
	#basic summaries of va
	Mtab <- rbind(Mtab,summary(RapopModelDraw(fit)$va))
	#estimates of model parameters from a standard lm() call on new data
	lmfit <- lm(fit$env$Formula,data=DFnew)
	Ctab <- rbind(Ctab,coefficients(lmfit))
	Vsig <- c(Vsig,summary(lmfit)$sigma)
}
Ctab <- cbind(Ctab,Vsig)
#summary of summaries
print(summary(DF$va))
print(apply(Mtab,2,summary))

#summary of coefficient estimates
print(c(t(Vbeta),Vs[1]))
print(apply(Ctab,2,summary))

#plot all the lines
DFline <- as.data.frame(Ctab[,1:2])
names(DFline) <- c("m","b")
p <- ggplot()
p <- p + geom_point(data=DF,aes(x=vc,y=va),position="jitter")
p <- p + geom_abline(data=DFline,aes(intercept=m,slope=b),alpha=1/5)
p <- p + geom_abline(intercept=Vbeta[1],slope=Vbeta[2],color="red",size=1.25)
#png("lines.png",width=1240,height=1080)
png("lines.png",width=10,height=7,units="in",res=300)
print(p)
dev.off()

