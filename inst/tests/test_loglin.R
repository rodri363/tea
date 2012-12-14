set.seed(1029384756)
kn <- 1e5
#multinomial on 8 categories
Mxyz <- expand.grid(0:1,0:1,0:1)
vp <- c(5,5,20,10,5,35,5,15)/100
vmult <- t(rmultinom(kn,1,vp)) %*% 1:8
Mdat <- Mxyz[vmult,]

#4 missing patterns
#1=vpz  -> 1 1 (both missing)
#2=vpxz -> 0 1 (y missing)
#3=vpyz -> 1 0 (x missing)
#4=vxyz -> 0 0 (neither missing)

#MCAR
vp11 <- rep(0.1,nrow(Mdat))
vp01 <- rep(0.4,nrow(Mdat))
vp10 <- rep(0.2,nrow(Mdat))
vp00 <- rep(0.3,nrow(Mdat))

##MAR
vp11 <- 0.25*(Mdat[,3]==0) + 0.125*(Mdat[,3]==1)
vp01 <- 0.25*(Mdat[,1]==Mdat[,3])
vp10 <- 0.25*(Mdat[,2]==Mdat[,3])
vp00 <- 1 - (vp11+vp01+vp10) #none missing

###MNAR
#vp11 <- 0.25*(Mdat[,1]==Mdat[,2])
#vp01 <- 0.125*(Mdat[,2]==0)+0.25*(Mdat[,2]==1)
#vp10 <- 0.25*(Mdat[,1]==0)+0.125*(Mdat[,1]==1)
#vp00 <- 1 - (vp11+vp01+vp10) #none missing
#
Mp <- cbind(vp11,vp01,vp10,vp00)
vpattern <- rowSums(Mp%*%upper.tri(diag(ncol(Mp)),diag=TRUE) < runif(nrow(Mp))) + 1
Mpattern <- matrix(c(1,0,1,0,1,1,0,0),ncol=2)
Mmiss <- Mpattern[vpattern,]

df <- as.data.frame(Mdat)
df <- as.data.frame(lapply(df,as.factor))
names(df) <- c("x","y","z")
dfo <- df
is.na(df$x) <- as.logical(Mmiss[,1])
is.na(df$y) <- as.logical(Mmiss[,2])

#print(as.data.frame(table(df)))
#print(as.data.frame(table(df,exclude=NULL)))
#print(as.data.frame(table(dfo)))
#
Mcat <- Mdat
is.na(Mcat[,1]) <- as.logical(Mmiss[,1])
is.na(Mcat[,2]) <- as.logical(Mmiss[,2])
colnames(Mcat) <- c("x","y","z")
Mcat <- as.matrix(Mcat+1)

library(cat)
lpre <- prelim.cat(Mcat)
fit <- em.cat(lpre)
dimnames(fit) <- dimnames(table(dfo))
fit1 <- ecm.cat(lpre,c(1,2,0,1,3,0,2,3,0))
dimnames(fit1) <- dimnames(table(dfo))

print(table(df,exclude=NULL))
print(round(table(df)/sum(table(df)),2))
print(round(fit,2))

print(round(apply(table(dfo)/sum(table(dfo)),c(1,2),sum),2)) #x/y margin
print(round(apply(fit1,c(1,2),sum),2)) #x/y margin

