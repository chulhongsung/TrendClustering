# Simulation 1
rm(list=ls()) ; gc()
# setwd('C:\\Users\\uos_stat\\Dropbox\\A Grouping trends\\grouping pursuit (med)\\prog\\result')
setwd("/Users/chulhongsung/Desktop/lab/working_paper/result")

# plot
load("gev_group1_1.rdata")
for ( i in 1:nrow(betaMat)) 
{
  if ( i == 1) plot(x = lambda.vec, y = betaMat[1,], 
                    ylim = c(min(betaMat)-1, max(betaMat)+1), type = 'l')
  lines(x = lambda.vec, y = betaMat[i,])
} 
library(quantreg)
rbeta.mat = NULL
beta.mat = betaMat
beta.mat = round(beta.mat,5)

##recovering?
for ( i in 1:ncol(beta.mat))
{
  
  rbeta.vec = beta.vec = beta.mat[,i]
  u.beta = unique(beta.vec)
  if (  setequal(u.beta,0) ) 
  {
    rbeta.vec = rep(0, length(beta.vec))
    rbeta.mat = rbind(rbeta.mat,rbeta.vec)
    next
  }
  
  g.idx = 1:length(u.beta)
  # j = 1
  
  for ( j in 1:length(g.idx))
  {
    sel = which(beta.vec == u.beta[g.idx[j]])
    if (u.beta[g.idx[j]] ==0 )
    {
      rbeta.vec[sel] =  0
    }
    y = unlist( conv.data$diffy[sel] )
    x = unlist(conv.data$diffx[sel])
    fit = rq.fit(x,y)
    rbeta.vec[sel] =  fit$coefficients    
  }
  rbeta.mat = rbind(rbeta.mat,rbeta.vec)
}

numActive = c()

for (i in 1:nrow(rbeta.mat))  
{
  tmp = unique(rbeta.mat[i,])
  if ( setequal(tmp,0) ) 
  {
    numActive[i] <- 0 
    next()
  }
  tmp = rbeta.mat[i,]
  ttmp<- tmp[tmp!=0]
  numActive[i] = length(unique(tmp))
}

BIC.vec = c()
y = unlist(conv.data$diffy)
x = unlist(conv.data$diffx)
z = unlist(lapply(conv.data$diffx,length))
p = 60
nj.vec = c()
for (i in 1:p)
{
  tmp <- preci[i,-1]
  nj.vec[i]<- sum(!is.na(tmp))
}

eff.n <- sum(nj.vec)

for (i in 1:ncol(beta.mat))
{
  tmp = rbeta.mat[i,]
  BIC.vec[i] = log(sum(abs(y-rep(tmp,z)*x))) +  numActive[i]/eff.n
  # BIC.vec[i] = log(sum(abs(y-rep(tmp,z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
}
par(mfrow = c(1,1))
tmp = log(sum(abs(y-rep(rep(0,60),z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
BIC.vec1 = c(tmp, BIC.vec)
plot(BIC.vec1)
sum(rbeta.mat[which.min(BIC.vec1),] == rbeta.mat[which.min(BIC.vec1),1])
length(unique(rbeta.mat[which.min(BIC.vec1),]))
table(rbeta.mat[which.min(BIC.vec1),])
min(BIC.vec1)
max(BIC.vec1)
which.min(BIC.vec1)

numActive[2]


unique(rbeta.mat[which.min(BIC.vec1), ])

numActive[which.min(BIC.vec1)-1]
numActive[10]

numActive[12]

unique(rbeta.mat[10, ])
table(rbeta.mat[10, ])
plot(c(NA,lambda.vec), BIC.vec1, type= 's', lwd=2,
     ylim = c(15.326, 15.34),
     ylab = 'BIC', main = 'BIC plot', xlab = 'tuning parameter',
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
abline(v = lambda.vec[4], col = "blue", lw = 3, lty = 2)
abline(v = lambda.vec[10], col = "red", lw = 3, lty = 1)
abline(v = lambda.vec[12], col = "black", lw = 3, lty = 3)
legend("topright", col = c("blue", "red", "black"),cex=1.5,lw = 3,  lty = c(2,1,3),
       legend = c("7 clusters", "12 clusters", "19 clusters"))


library(quantreg)
lambda = 0
i = 1
tmp <-c()
for (i in 1:60)
{ 
  fit <- rq.fit(x = conv.data$diffx[[i]]  , y = conv.data$diffy[[i]])
  tmp[i] <- fit$coefficients
}

#rq.fit(x = unlist(conv.data$diffx)  , y = unlist(conv.data$diffy))$coef

rbeta.mat = rbind(rep(0.7352841,60),rbeta.mat, tmp)

#betaVec = fused.QR.fun (conv.data, adj.mat,lambda1=lambda*l.scale,lambda2=lambda)

lambda1.vec = c(700,lambda.vec, 0 )
for ( i in 1:ncol(rbeta.mat)) 
{
  if ( i == 1) plot(x = lambda1.vec, y = rbeta.mat[,1], 
                    ylim = c(min(rbeta.mat)-0.1, max(rbeta.mat)+0.1), type = 's',
                    lty = 2,
                    main = "Path of coefficients",
                    xlab = 'tuning parameter',
                    ylab = "coefficient", 
                    cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
  lines(x = lambda1.vec, y = rbeta.mat[,i], type = "s", lty = 1)
}

abline(v = lambda1.vec[5], col = "blue",lw = 3, lty = 2)
abline(v = lambda1.vec[11], col = "red", lw = 3, lty = 1)
abline(v = lambda1.vec[13], col = "black", lw = 3, lty = 3)
legend("topright", col = c("blue", "red", "black"), lw = 3, lty = c(2,1,3),
  legend = c("7 clusters", "12 clusters", "19 clusters"), cex=1.5)



#plot(c(51,lambda.vec), BIC.vec1, type= 's', ylab = 'BIC', main = 'BIC plot', xlab = 'tuning parameter')
#write.csv(rbeta.mat, file = '..\\result\\rbetaMat2021.csv',row.names = F)
#rbeta.mat[9,]
