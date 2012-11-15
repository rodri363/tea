#not exactly the model i want
#need to have:
#for a given value of lwag, age missing randomly
#for a given value of age, lwag missing randomly
#e.g.
#for age = 30, dist of lwag same for missing and non-missing
#for lwag = 1.5, dist of age same for missing and non-missing
library(tea)
library(ggplot2)
library(Cairo)

kn <- 1e4L

#loginc from age
vage <- runif(kn,15,65)
kslope <- log10(50000/15000)/50
Mb <- matrix(c(log10(50000) - 65*kslope,kslope))
Mx <- cbind(1,vage)
ksig <- log10(1.2)
vm <- as.vector(Mx %*% Mb)
vwag <- vm + rnorm(kn,0,ksig)

#sex from age
#50% male at age 0
#40% male at 65
Mb <- matrix(c(0,log(0.4/0.6)/65))
vm <- as.vector(Mx %*% Mb)
vp <- 1/(exp(-vm)+1)
vsex <- rbinom(kn,1,vp)

#missingness from age
#50% missing at age 0
#5% missing at 65
Mb <- matrix(c(log(0.5/0.5),(log(0.05/0.95)-log(0.5/0.5))/65))
vm <- as.vector(Mx %*% Mb)
vp <- 1/(exp(-vm)+1)
vmiss <- rbinom(kn,1,vp)

dfo <- data.frame(age=vage,sex=factor(vsex),lwag=vwag)
dfo$msex <- rbinom(kn,1,vp)==1
dfo$mlwag <- rbinom(kn,1,vp)==1
df <- dfo
is.na(df$sex) <- df$msex
is.na(df$lwag) <- df$mlwag
df$d <- "miss"
dfo$d <- "full"

p <- ggplot(dfo,aes(x=age,y=lwag,color=factor(mlwag)))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_point()
p <- p + stat_smooth(method="lm")
CairoPNG(file="df1.png",width=1024,height=768)
print(p)
dev.off()

p <- ggplot(dfo,aes(x=lwag,fill=factor(mlwag)))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_density()
CairoPNG(file="df2.png",width=1024,height=768)
print(p)
dev.off()

if(FALSE){
dfacs <- with(subset(dcp,WAGP>0),data.frame(age=AGEP,lwag=log(WAGP)))
dfx <- dfacs
is.na(dfacs$lwag) <- rbinom(nrow(dfacs),1,runif(nrow(dfacs),0,1)/2.5)==1
ereg <- as.environment(list(Data=dfacs, Formula=lwag~age, debug=FALSE))
tea.srmi.reg.fit(ereg)
ldft <- lapply(1:5,function(x) return(tea.srmi.reg.draw(ereg)))
lfit <- lapply(ldft,function(x) return(lm(lwag ~ age,data=x)))
vQ <- unlist(lapply(lapply(lfit,coef),"[[",2))
vU <- unlist(lapply(lapply(lapply(lfit,vcov),diag),"[[",2))
km <- length(vQ)
Qbar <- (1/km)*sum(vQ)
Ubar <- (1/km)*sum(vU)
B <- (1/km)*sum((vQ-Qbar)^2)
T <- (1+1/km)*B + Ubar
stat <- Qbar/sqrt(T) #assumes Q=coef=0
nu <- (km-1)*(1+(Ubar/((1+1/km)*B)))^2
}

for(outdx in 1:1){
	msrmi <- setupRapopModel(tea.srmi)
	esrmi <- as.environment(list(Data=df, LHS=~sex+lwag, RHS=~age,
		debug=FALSE,maxit=10,verbose=1))
	efit <- RapopModelEstimate(esrmi,msrmi)

	dfa <- dfo
	for(idx in 1:9){
		dfs <- RapopModelDraw(efit)
		dfs$d <-as.factor(formatC(idx,width=2,flag="0"))
		dfa <- rbind(dfa,dfs)
	}

#	dfa <- subset(dfa,mb==1 | ma==1)

	p <- ggplot(dfa,aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + geom_bar(aes(x=sex),position=position_dodge())
	CairoPNG(file=paste("sex",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(dfa,aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + geom_density(aes(x=lwag),alpha=1/10)
	CairoPNG(file=paste("lwagdist",outdx,".png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(dfa,aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + stat_smooth(aes(x=age,y=lwag),method="lm")
	CairoPNG(file=paste("lwagXage",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(subset(dfa,mlwag==1),aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + geom_boxplot(aes(x=d,y=lwag),alpha=1/5)
	CairoPNG(file=paste("bdens",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	CairoPDF(paste("codalwag",outdx,"pdf",sep="."))
	plot(esrmi$Lfit$lwag$env$Fit)
	dev.off()

	CairoPDF(paste("codasex",outdx,"pdf",sep="."))
	plot(esrmi$Lfit$sex$env$Fit)
	dev.off()

}
