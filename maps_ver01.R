rm(list = ls()) ; gc()
#setwd('C:\\Users\\uos_stat\\Dropbox\\A Grouping trends\\grouping pursuit (med)\\prog\\data')
setwd("/Users/chulhongsung/Desktop/lab/working_paper/water")
library(mapdata)
library(ggmap)
library(lubridate)
library(kriging)
library(lattice)
Sys.setlocale("LC_ALL", 'korean')
# coefMat = rbeta.mat
coefMat = read.csv(file = 'rbetaMat1.csv')
site    = read.csv(file = 'site.csv', fileEncoding = "CP949", encoding = "UTF-8")
#cbind(preci[,1], site)
tmp1 = unlist(strsplit(site$L, c("˚")))
tmp2= tmp1[seq(1,length(tmp1), by = 2)]
tmp3= tmp1[seq(2,length(tmp1), by = 2)]
tmp4 = unlist(strsplit(tmp3,'´'))
longitude = round(as.numeric(tmp2)+ as.numeric(tmp4)/60,2)

tmp1 = unlist(strsplit(site$A, c("˚")))
tmp2= tmp1[seq(1,length(tmp1), by = 2)]
tmp3= tmp1[seq(2,length(tmp1), by = 2)]
tmp4 = unlist(strsplit(tmp3,'´'))
latitude = round(as.numeric(tmp2)+ as.numeric(tmp4)/60,2)
##

coef = unlist(coefMat[5,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef, pixels=300)
map.1 = kriged$map

layout(matrix(c(1,2),1,2,byrow = TRUE), c(6,1.5), c(6), T)
jpar<- colorRampPalette(c("blue", "white","red"))
j.col<- jpar(64)[20:64]

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)


##
coef = unlist(coefMat[5,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)

coef = unlist(coefMat[9,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)



coef = unlist(coefMat[12,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)


################################################################################
################################################################################


setwd('./result')

# plot
load("gev_group1_1.rdata")

library(quantreg)
lambda = 0
i = 1
tmp <-c()
for (i in 1:60)
{ 
  y <- preci[i,-1]
  x <- 1:length(y)
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  fit <- rq.fit(x = cbind(1,x), y= y)
  tmp[i] <- fit$coefficients[2]
}

coef = tmp
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)




library(quantreg)
lambda = 0
i = 1
tmp <-c()
for (i in 1:60)
{ 
  fit <- rq.fit(x = conv.data$diffx[[i]]  , y = conv.data$diffy[[i]])
  tmp[i] <- fit$coefficients
}
tmp

dim(betaMat)

coef = betaMat[,14]
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      #sub = paste(19, "clusters"), cex.sub = 1.5,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude',
      cex.lab=1.5,
      cex.axis=1.5)
#col2rgb("#FFF6F6", alpha = FALSE)
#image(kriged, xlim = c(125,132), ylim = c(33,38.5),col = gray((25:5)/25),main = "3 clusters",
#      ylab = 'latitude', xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}
text(c(130.53, 126.31, 126.26), c(37.28, 33.30, 37.42), labels=c("Ulleung", "Jeju", "Ganghwa"), cex=1.4, font=4)

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef", cex.lab=1.5, cex.axis=1.5)
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T, 
      cex.lab=1.5)

cbind(preci[,1], site[,2], c(coefMat[9,]))



###########
#######################################################################
# plot (point map)
rdata = rdata1[1:60,]
ggmap(get_googlemap(center = 'korea', zoom=7, maptype='terrain',
                    language = "en-EN"),
      extent='device', fullpage = TRUE)+
  geom_point(aes(y=longitude, x=latitude, size=coef), colour = 'blue', data = rdata, alpha = 0.8)
