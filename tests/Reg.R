library(tea);

kN <- 1000;
data <- data.frame(ID=1:kN,geo=rep(1:2,length.out=kN),x=floor(runif(kN,0,50)),
	y1=factor(floor(runif(kN,0,4))),
	y2=rchisq(kN,5),
	y3=factor(floor(runif(kN,0,3))));
write.csv(data,"Reg.csv",quote=FALSE,row.names=FALSE)

read_spec("Reg.spec")
doInput()
doFingerprint()
#debug(doRegression)
#debug(RegEditSyn)
doRegression()
print(warnings())

load("updates.RData")
data <- dbGetQuery(pepenv$con,"select * from ORIGReg")
syn <- dbGetQuery(pepenv$con,"select * from viewReg")
#flags <- list(data.frame(ID=1:kN, flag=1),list("flag","flag","flag"))
#model.spec <- vector("list",3)
#model.spec[[1]] <- list("x~y1","gam")
#model.spec[[2]] <- list("y1~x","polr")
#model.spec[[3]] <- list("y3~1","multinom")
#
#syn <- RegEditSyn(model.spec,data,flags)
print(summary(data$x))
print(summary(syn$x))
print(table(data$y1))
print(table(syn$y1))
print(table(data$y3))
print(table(syn$y3))
print(table(data[,c("y1","y3")]))
print(table(syn[,c("y1","y3")]))
stopifnot(abs(table(data[,c("y1","y3")]) - table(syn[,c("y1","y3")]))  < 1)
print(tapply(data$x,data$y1,table))
print(tapply(syn$x,syn$y1,table))
