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

kn <- 3e4L
vage <- runif(kn,15,65)
#sex from age
#50% male at age 0
#40% male at 65
Mb <- matrix(c(0,log(0.4/0.6)/65))
Mx <- cbind(1,vage)
vm <- as.vector(Mx %*% Mb)
vp <- 1/(exp(-vm)+1)
vsex <- rbinom(kn,1,vp)

#loginc from age and sex
kaslope <- log10(50000/15000)/50
ksslope <- log10(2)
Mb <- matrix(c(log10(50000) - 65*kaslope - ksslope,kaslope,ksslope),ncol=1)
Mx <- cbind(1,vage,vsex)
ksig <- log10(1.2)
vm <- as.vector(Mx %*% Mb)
vlwag <- vm + rnorm(kn,0,ksig)

#missing patterns
#SEX	WAG	AGE
#x		o	o	can depend on wag/age
#o		x	o	can depend on sex/age
#x		x	o	can depend on age

#missing both from age
#50% missing at age 0
#5% missing at 65
Mb <- matrix(c(log(0.5/0.5),(log(0.05/0.95)-log(0.5/0.5))/65))
Mx <- cbind(1,vage)
vm <- as.vector(Mx %*% Mb)
vp <- 1/(exp(-vm)+1)
vma <- rbinom(kn,1,vp)==1
kma <- sum(!vma) #total non-missing

#missing wag from sex
vp <- (0.25*(vsex[!vma]==1)) + (0.025*(vsex[!vma]==0))
vms <- rbinom(kma,1,vp)==1
kms <- sum(!vms)

#missing sex from wag
vp <- 0.25*ecdf(vlwag[!vma][!vms])(vlwag[!vma][!vms]) #richer = more missing
vmw <- rbinom(kms,1,vp)==1

dfo <- data.frame(age=vage,sex=factor(vsex),lwag=vlwag)
dfo$mlwag <- vma
dfo$msex <- vma
dfo$mlwag[!vma] <- vms
dfo$msex[!vma][!vms] <- vmw
dfo$qage <- ceiling(10*ecdf(dfo$age)(dfo$age)/2)
dfo$qlwag <- ceiling(10*ecdf(dfo$lwag)(dfo$lwag)/2)
df <- dfo
df$d <- "miss"
dfo$d <- "full"
is.na(df$sex) <- df$msex
is.na(df$lwag) <- df$mlwag

#show that dists when both missing are the same given age
#first, show diff in univariate sex
p <- ggplot(dfo,
	aes(x=sex,fill=factor(mlwag & msex)))
p <- p + geom_bar(position=position_fill())
CairoPNG(file="sex11.png",width=1024,height=768)
print(p)
dev.off()
p <- p + facet_grid(.~qage)
CairoPNG(file="sexXage11.png",width=1024,height=768)
print(p)
dev.off()

#now show diff in conditional wag|sex
p <- ggplot(dfo,
	aes(x=lwag,fill=factor(mlwag & msex)))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_density(alpha=1/4)
p <- p + facet_grid(sex~.)
CairoPNG(file="lwag11.png",width=1024,height=768)
print(p)
dev.off()
#now show how age fixes it
p <- p + facet_grid(sex~qage)
CairoPNG(file="lwagXage11.png",width=1024,height=768)
print(p)
dev.off()

#show diff in univariate sex when only wag is missing
p <- ggplot(dfo,
	aes(x=sex,fill=factor(msex & !mlwag)))
p <- p + geom_bar(position=position_fill())
CairoPNG(file="sex01.png",width=1024,height=768)
print(p)
dev.off()
p <- p + facet_grid(.~qlwag)
CairoPNG(file="sexXwag01.png",width=1024,height=768)
print(p)
dev.off()

#now show diff in wag when only it is missing
p <- ggplot(dfo,
	aes(x=lwag,fill=factor(mlwag & !msex)))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_density(alpha=1/2)
CairoPNG(file="lwag01.png",width=1024,height=768)
print(p)
dev.off()
#now show how sex fixes it
p <- p + facet_grid(sex~.)
CairoPNG(file="lwagXsex01.png",width=1024,height=768)
print(p)
dev.off()

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

dfa <- rbind(dfa,df[complete.cases(df),])
#	dfa <- subset(dfa,mb==1 | ma==1)

p <- ggplot(dfa,aes(fill=d,color=d))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_bar(aes(x=sex),position=position_dodge())
CairoPNG(file=paste("sex","png",sep="."),width=1024,height=768)
print(p)
dev.off()

p <- ggplot(dfa,aes(fill=d,color=d))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_density(aes(x=lwag),alpha=1/10)
p <- p + facet_grid(sex ~ .)
CairoPNG(file=paste("lwagdist",".png",sep="."),width=1024,height=768)
print(p)
dev.off()

p <- ggplot(dfa,aes(fill=d,color=d))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + stat_smooth(aes(x=age,y=lwag),method="lm")
p <- p + facet_grid(sex ~ .)
CairoPNG(file=paste("lwagXage","png",sep="."),width=1024,height=768)
print(p)
dev.off()

p <- ggplot(dfa,aes(fill=d,color=d))
p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
p <- p + geom_boxplot(aes(x=d,y=lwag),alpha=1/5)
CairoPNG(file=paste("bdens","png",sep="."),width=1024,height=768)
print(p)
dev.off()

CairoPDF(paste("codalwag","pdf",sep="."))
plot(esrmi$Lfit$lwag$env$Fit)
dev.off()

CairoPDF(paste("codasex","pdf",sep="."))
plot(esrmi$Lfit$sex$env$Fit)
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
