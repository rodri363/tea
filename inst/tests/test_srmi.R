library(tea)
library(ggplot2)
library(Cairo)

kn <- 1e4L
vc <- runif(kn,0,1)
vb <- rnorm(kn,30+100*vc,10)
va <- sample(LETTERS[1:3],kn,replace=TRUE)
df <- data.frame(a=va,b=vb,c=vc)
dfo <- df
is.na(df$a) <- rbinom(kn,1,vc/2)==1
is.na(df$b) <- rbinom(kn,1,vc/2)==1

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
	df <- with(subset(dcp,WAGP>0 & AGEP>34),
		data.frame(a=factor(SEX),b=log(WAGP),c=AGEP))
	df$mp <- ecdf(df$c)(df$c)
	df$ma <- rbinom(nrow(df),1,df$mp)
	df$mb <- rbinom(nrow(df),1,df$mp)
	dfo <- df
	dfo$d <- "orig"
	dfo$d <- as.factor(dfo$d)
	df$d <- "miss"
	df$d <- as.factor(df$d)
	is.na(df$a) <- df$ma==1
	is.na(df$b) <- df$mb==1

	msrmi <- setupRapopModel(tea.srmi)
	esrmi <- as.environment(list(Data=df, LHS=~a+b, RHS=~c,debug=FALSE,maxit=10))
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
	p <- p + geom_bar(aes(x=a),position=position_dodge())
	CairoPNG(file=paste("a",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(dfa,aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + geom_density(aes(x=b),alpha=1/10)
	CairoPNG(file=paste("bdist",outdx,".png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(dfa,aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + stat_smooth(aes(x=c,y=b),method="lm")
	CairoPNG(file=paste("b",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	p <- ggplot(subset(dfa,mb==1),aes(fill=d,color=d))
	p <- p + scale_color_brewer(pal="Paired") + scale_fill_brewer(pal="Paired")
	p <- p + geom_boxplot(aes(x=d,y=b),alpha=1/5)
	CairoPNG(file=paste("bdens",outdx,"png",sep="."),width=1024,height=768)
	print(p)
	dev.off()

	CairoPDF(paste("densb",outdx,"pdf",sep="."))
	plot(esrmi$Lfit$b$env$Fit)
	dev.off()

	CairoPDF(paste("densa",outdx,"pdf",sep="."))
	plot(esrmi$Lfit$a$env$Fit)
	dev.off()
}
