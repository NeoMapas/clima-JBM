require(circular)
x <- circular(subset(dts.clm,tmax>36)$doy*2*pi)
plot(x,stack=T)
rose.diag(x,bins=23)

require(ade4)
subset(dts.clm,tmax>36)
tmp <- with(subset(dts.clm,year %in% 2000),aggregate(tmean,list(anual),mean,na.rm=T))
dotcircle(tmp$x,labels = 1:23)

par(mfrow = c(4,4),xpd=T)
for (k in 1980:1995) {
  tmp <- with(subset(dts.clm,year %in% k),aggregate(tmean,list(month),mean,na.rm=T))
  dotcircle(tmp$x,labels = c("E","F","M","A","M","J","J","A","S","O","N","D"),xlim=c(25,30))
  text(1,1,k)

}

tmp <- subset(dts.clm,year==1984)
x <- tmp$tmax##-min(tmp$tmean,na.rm=T)
grd <- (tmp$doy*2*pi)

tmp <- subset(dts.sen, var=="LST_Day_1km")
x <- tmp$val##-min(tmp$val,na.rm=T)
grd <- deg(tmp$doy*2*pi)
grd <- tmp$doy*2*pi


par(pty="s")

clr <- colorRampPalette(c("cyan","red"))
mx <- max(abs(c(range(sin(grd)*x,na.rm=T),range(cos(grd)*x,na.rm=T))))
plot(sin(grd)*x,cos(grd)*x,axes=F,xlab=NA,ylab=NA,pch=19,
     xlim=c(-1*mx,mx),ylim=c(-1*mx,mx),
     col=clr(25)[as.numeric(cut(x,breaks=25))])
y <- (seq(0,1,length=365)*2*pi)
lines(sin(y)*20,cos(y)*20,lty=3)
##lines(cos(y)*25,sin(y)*25)
lines(sin(y)*30,cos(y)*30,lty=3)
lines(sin(y)*40,cos(y)*40,lty=3)
ms <- cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))*2*pi/365
text(sin(ms)*15,cos(ms)*15,c("E","F","M","A","M","J","J","A","S","O","N","D"))
ms <- c(20,30,40)
text(sin(0)*(ms+1),cos(0)*(ms+1),paste(ms,"°C"),cex=.5)

