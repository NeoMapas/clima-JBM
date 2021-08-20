### R code from vignette source '~/Dropbox/CEBA/doc/400_InventarioJardinBotanicoMaracaibo/Documento1_climaJBM.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Documento1_climaJBM.Rnw:57-71
###################################################
require(chron)
require(raster)
require(RColorBrewer)

paquetes <- (.packages())
paquetes <- paquetes[!(paquetes %in% c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base", "deldir", "DBI", "RMySQL"))]

luq <- function(x,contar.NA=FALSE) {
	if (contar.NA==F) {
	x <- x[!is.na(x)]
	}
 length(unique(x))
 }



###################################################
### code chunk number 2: citas paquetes
###################################################
cat(paste("\\emph{",paquetes,"} \\citep{pqt::",paquetes,"}",sep="",collapse="; "))


###################################################
### code chunk number 3: mapoteca
###################################################
mptc <- "~/CEBA/lib/mapoteca/"
mapaDB <- "Maracaibo"
mapaDB <- "ZPM"

if (!exists("bosque")) {
  LC <- raster(sprintf("%s/%2$s/MCD12Q1.A2005001.%2$s_LC1.tif",mptc,mapaDB))
  bosque <- raster(sprintf("%s/%s/GFC2013/GFC2013.%2$s.treecover2000.tif",mptc,mapaDB))
  perdida <- raster(sprintf("%s/%s/GFC2013/GFC2013.%2$s.loss.tif",mptc,mapaDB))
  ganancia <- raster(sprintf("%s/%s/GFC2013/GFC2013.%2$s.gain.tif",mptc,mapaDB))
  frags <- clump(bosque>40)
  sfrags <- clump(ganancia)

  tt <- rev(sort(table(values(frags))))
  values(frags)[values(frags) %in% as.numeric(names(tt)[tt<200])] <- 9999
  ##table(values(sfrags))
  ##plot(sfrags %in% 446)
  tt <- rev(sort(table(values(frags))))
  
  slc <- c(426, 441, 442) ## para Maracaibo
  slc <- c(602, 617, 1504,449,98,277,1288) ## para ZPM
  ##plot(frags %in% slc[1:2])
  ##plot(frags %in% slc[c(4,6)])

##  r0 <- (frags %in% slc[1:2]) + sfrags %in% 446 ## para Maracaibo
    r0 <- (frags %in% slc[1:2]) + sfrags %in% 669 ## para ZPM
  JBM <- rasterToPolygons(r0, fun=function(x) {x %in% 1},dissolve=T)
}


###################################################
### code chunk number 4: municipios
###################################################
vptc <- "~/NeoMapas/lib/gisdata/"                              
if (!exists("rADM2")) {
  
##  municipio <- shapefile(sprintf("%s/%s",mptc,"VEN_adm2.shp"))
##  subset(municipio@data,NAME_2 %in% c("Maracaibo","San Francisco") & NAME_1 %in% "Zulia")
##  rADM2 <- rasterize(municipio,bosque,field="ID_2")
  
  
  municipio <- shapefile(sprintf("%s/%s",vptc,
                                 "Municipios Vzla_CGeog_WGS84_region.shp"))
  vialidad <- shapefile(sprintf("%s/%s",mptc,"VEN_roads.shp"))
  
  subset(municipio@data,iconv(MUNICIPIO,"latin1","utf8") %in% toupper(c("Maracaibo","San Francisco","La Cañada de Urdaneta","Mara")) & ESTADO %in% "ZULIA")

  rADM2 <- rasterize(municipio,bosque,field="CODIGO")

}
  xarea <- median(values(area(rADM2)))



###################################################
### code chunk number 5: Documento1_climaJBM.Rnw:160-186
###################################################
if (!exists("rZPM")) {
  ZPM <- read.csv(file="~/CEBA/data/JardinBotanicoMaracaibo/ZonaProtectoraCoords.csv",row.names=1)
  ZPM <- rbind(ZPM,head(ZPM,1))
  coordinates(ZPM) <- 1:2
  proj4string(ZPM) <- "+proj=utm +zone=19n +datum=WGS84"
  
  ZPM.ll <- spTransform(ZPM,bosque@crs)
  ZPM.ll <- SpatialPolygons(list(Polygons(list(Polygon(ZPM.ll)),ID=1)),proj4string=ZPM.ll@proj4string)
  
  rZPM <- rasterize(ZPM.ll,bosque)
}

plot(ZPM.ll,col="grey88",lty=3)
##plot(vialidad,add=T,col="grey",lwd=3)
##plot(vialidad,add=T,col=1,lty=2,lwd=1)
plot(municipio,lty=1,lwd=2,add=T)
plot(JBM,col="darkgreen",add=T)

g <- floor(abs(JBM@bbox))
g <- g*sign(JBM@bbox)
m <-  (abs(JBM@bbox) - floor(abs(JBM@bbox))) * 60
##floor(m)
s <-  (abs(m) - floor(abs(m))) * 60

xys <- sprintf("%s°%s'%0.2f''",g,floor(m),s)



###################################################
### code chunk number 6: Area
###################################################
calc.area <- table(values(bosque)>40,values(rADM2),values(rZPM))
area.2000 <- sum(values(frags %in% slc[1:2]))*xarea
area.2012 <- sum(values(r0))*xarea



###################################################
### code chunk number 7: Area2 (eval = FALSE)
###################################################
## 
## table(values(bosque)>0,values(rADM2))
## table(values(rZPM),values(rADM2))
## 
## area.2000 <- sum(values(rADM2) %in% c(275,311) & values(frags %in% slc[1:2]))*xarea
## area.2012 <- sum(values(r0))*xarea
## 
## table(values(rADM2) %in% c(275,311),values(frags %in% slc[1:2]))*xarea
## table(values(rADM2) %in% c(275,311),values(frags>0))*xarea
## 
## table(values(rADM2),values(frags))*xarea
## round(table(values(rADM2),values(frags))*xarea,3)
## ##plot(bosque)
## ##plot(municipio,add=T)
## 
## table(values(frags),values(perdida))
## table(values(sfrags %in% 446))*xarea


###################################################
### code chunk number 8: bajarDatosClimaticos (eval = FALSE)
###################################################
## setwd(" ~/CEBA/data/JardinBotanicoMaracaibo/climaviejo")
## for (yy in 1959:2013) {
##   for (mm in 1:12) {
##     system(sprintf("wget --continue 'http://www.tutiempo.net/clima/Maracaibo-La_Chinita/%02d-%04d/804070.htm' --output-document=/home/jferrer/CEBA/data/JardinBotanicoMaracaibo/climaviejo/E804070_%04d_%02d.htlm",mm,yy,yy,mm))
##   }
## }
## 


###################################################
### code chunk number 9: DatosClimaticos
###################################################
if (file.exists("~/CEBA/Rdata/JBMts.rda")) {
  load(file="~/CEBA/Rdata/JBMts.rda")
}
if (!exists("dts.clm")) {
  dts.clm <- data.frame()
  for (year in 1959:2013) {
    for (month in 1:12) {
      if (file.exists(sprintf("~/CEBA/data/JardinBotanicoMaracaibo/climaviejo/E804070_%i_%02i.htlm",year,month))) {
        system(sprintf("html2text ~/CEBA/data/JardinBotanicoMaracaibo/climaviejo/E804070_%i_%02i.htlm > prueba.txt",year,month))
      
        ##j <- as.numeric(system("grep -n 'Valores medios climáticos' prueba.txt | cut -d: -f1",intern=T))+2
        j <- as.numeric(system("grep -n 'Valores históricos' prueba.txt | cut -d: -f1",intern=T))+1
        k <- as.numeric(system("grep -n 'Medias y totales mensuales' prueba.txt | cut -d: -f1",intern=T))-1
        if (length(j)>0) {
          system(sprintf("sed -n %s,%sp prueba.txt > tabla",j,k))
          ##k=`grep -n "Medias y totales mensuales" prueba.txt| cut -d: -f1`
          ##let "j += 2"
          ##let "k -= 1"
          ##sed -n $j,$kp prueba.txt > tabla
          ##sed -n $j,+31p prueba.txt > tabla
          
          
          tt <- read.table("tabla",sep="",as.is=T)
          dts.clm <- rbind(dts.clm,data.frame(year,month,day=1:nrow(tt),
                                        tmean=as.numeric(tt$DíaT),
                                        tmin=as.numeric(tt$Tm),
                                        tmax=as.numeric(tt$TM),
                                        H=as.numeric(tt$H),
                                        PP=as.numeric(tt$PP),
                                        VV=as.numeric(tt$VV)))
        }
      }
    }
  }
  tt <- chron(dates.=sprintf("%04d/%02d/%02d",dts.clm$year,dts.clm$month,dts.clm$day),
              format = c(dates = "y/m/d"))
  dts.clm$doy <- as.numeric(format(as.Date(tt),format="%j"))/365
  dts.clm$anual <- cut(dts.clm$doy,breaks=seq(0,1.01,length=24))

  save(file="~/CEBA/Rdata/JBMts.rda",dts.clm)
}



###################################################
### code chunk number 10: DatosModis
###################################################
if (!exists("dts.sen")) {

  dts.sen <- data.frame()
  for (vv in c("250m_16_days_NDVI","250m_16_days_EVI","LST_Day_1km","LST_Night_1km","PET_1km","ET_1km","Lai_1km","Fpar_1km")) {
    for (k in dir(sprintf("%s/%s/%s",mptc,mapaDB,vv))) {
      fch <- sub("A","",strsplit(k,"\\.")[[1]][2])
      yr <- as.numeric(substr( fch,1,4))
      dd <- as.numeric(substr( fch,5,8))
      fch <- yr + (dd/365)
      rq <- raster(sprintf("%s/%s/%s/%s",mptc,mapaDB,vv,k))
      qry <- unlist(extract(rq,JBM))
      dts.sen <- rbind(dts.sen,data.frame(fch=fch,
                                          year=yr,
                                          doy=dd/365,
                                          j=1:length(qry),var=vv,
                                          val=qry))
    }
  }
 
  dts.sen$anual <- cut(dts.sen$doy,breaks=seq(0,1.01,length=24))
  save(file="~/CEBA/Rdata/JBMts.rda",dts.sen,dts.clm)
}
                                                                      
ss <- dts.sen$var %in% c("LST_Day_1km","LST_Night_1km")
dts.sen$val[ss & dts.sen$val < 7500] <- NA
dts.sen$val[ss] <- (dts.sen$val[ss] * 0.02)-273.15
ss <- dts.sen$var %in% c("ET_1km","PET_1km")
dts.sen$val[ss & dts.sen$val > 32760] <- NA
dts.sen$val[ss] <- dts.sen$val[ss] * 0.1
ss <- dts.sen$var %in% c("Fpar_1km","Lai_1km")
dts.sen$val[ss & dts.sen$val > 100] <- NA
dts.sen$val[ss & dts.sen$val < 0] <- NA
dts.sen$val[ss] <- dts.sen$val[ss] * 0.1
ss <- dts.sen$var %in% c("250m_16_days_EVI","250m_16_days_NDVI")
dts.sen$val[ss & dts.sen$val > 10000] <- NA
dts.sen$val[ss & dts.sen$val < -2000] <- NA
dts.sen$val[ss] <- dts.sen$val[ss] * 0.0001
dts.sen$val[ss & dts.sen$val == 0] <- NA



###################################################
### code chunk number 11: SerieTemperatura
###################################################
ss <- subset(dts.clm,year>1972)
ss$fch <- ss$year+ss$doy


###################################################
### code chunk number 12: AumentoTemperatura
###################################################

##plot(tmean~fch,ss,cex=.25)
##lines(supsmu(ss$fch,ss$tmax,span=.20),col=2,lwd=3)
##lines(supsmu(ss$fch,ss$tmean,span=.20),col=1,lwd=3)
##lines(supsmu(ss$fch,ss$tmin,span=.20),col=4,lwd=3)
##decompose(ts(ss$tmean,start=150,frequency=365))

plot(tmean~fch,ss,cex=.5,pch=1,col="grey55",xlab="Año",ylab="Temperatura [°C]")
ys <- ts(ss$tmean,start=152,frequency=365)
ys <- zoo::na.approx(ys)
lines(ss$fch,decompose(ys)$trend[1:nrow(ss)],col=1,lwd=2)

ys <- ts(ss$tmin,start=152,frequency=365)
ys <- zoo::na.approx(ys)
lines(ss$fch,decompose(ys)$trend[1:nrow(ss)],col=4,lwd=2)

ys <- ts(ss$tmax,start=152,frequency=365)
ys <- zoo::na.approx(ys)
lines(ss$fch,decompose(ys)$trend[1:nrow(ss)],col=2,lwd=2)



###################################################
### code chunk number 13: Documento1_climaJBM.Rnw:385-388
###################################################
pp <- with(ss,
           aggregate(data.frame(PP=PP),list(yr=year),
                     function(x) {sum(x,na.rm=T)*365*1/length(x)}))


###################################################
### code chunk number 14: Documento1_climaJBM.Rnw:393-398
###################################################

plot(PP~yr,pp,type="h",xlab="Año",ylab="Precipitación total anual [mm]")
lines(supsmu(pp$yr,pp$PP),col=3)




###################################################
### code chunk number 15: NDVIts
###################################################
ss <- subset(dts.sen,var %in% "250m_16_days_NDVI")
plot(val~fch,ss,cex=.5,pch=1,xlab="Año",ylab="NDVI")
for (k in 1:46) {
  x <- subset(dts.sen,var %in% "250m_16_days_NDVI" & j ==k)$fch
  y <- subset(dts.sen,var %in% "250m_16_days_NDVI" & j ==k)$val
  ys <- ts(y,start=4,frequency=23)
  ys <- zoo::na.approx(ys)

  lines(x,decompose(ys)$trend,col=2,lwd=2)
}


###################################################
### code chunk number 16: LSTts
###################################################
ss <- subset(dts.sen,var %in% c("LST_Day_1km","LST_Night_1km"))
plot(val~fch,ss,cex=.5,pch=1,xlab="Año",ylab="LST")
for (k in 1:46) {
  x <- subset(dts.sen,var %in% "LST_Day_1km" & j ==k)$fch
  if (length(x)>0) {
    y <- subset(dts.sen,var %in% "LST_Day_1km" & j ==k)$val
    ##  x <- x[!is.na(y)]
    ##  y <- y[!is.na(y)]
    ys <- ts(y,start=c(2000,9),frequency=46)
    ys <- zoo::na.approx(ys)
    
    lines(as.numeric(time(ys)),decompose(ys)$trend,col=2,lwd=2)
  }
  x <- subset(dts.sen,var %in% "LST_Night_1km" & j ==k)$fch
  if (length(x)>0) {
    
    y <- subset(dts.sen,var %in% "LST_Night_1km" & j ==k)$val
    ##  x <- x[!is.na(y)]
    ##  y <- y[!is.na(y)]
    ys <- ts(y,start=c(2000,9),frequency=46)
    ys <- zoo::na.approx(ys)
    
    lines(as.numeric(time(ys)),decompose(ys)$trend,col=4,lwd=2)
  }
}


###################################################
### code chunk number 17: temperatura
###################################################
##tt <- with(subset(dts.clm,year>1972),aggregate(data.frame(tmean=tmean,tmin=tmin,tmax=tmax),list(yr=year,anual=anual),function(x) {mean(x,na.rm=T)}))

tt <- with(subset(dts.clm,year>1999 & year<2012),
           aggregate(data.frame(tmean=tmean,tmin=tmin,tmax=tmax),
                     list(yr=year,anual=anual),function(x) {mean(x,na.rm=T)}))
tx <- with(subset(dts.clm,year>1999 & year<2012),
           aggregate(data.frame(tmax=tmax),list(yr=year,anual=anual),
                     function(x) {max(x,na.rm=T)}))
tn <- with(subset(dts.clm,year>1999 & year<2012),
           aggregate(data.frame(tmin=tmin),list(yr=year,anual=anual),
                     function(x) {min(x,na.rm=T)}))

par(xpd=T)
boxplot(tmax~anual,tx,ylim=c(10,45),axes=F,ylab="Temperatura [°C]")
boxplot(tmin~anual,tn,axes=F,add=T)
boxplot(tmean~anual,tt,ylim=c(20,35),axes=F,add=T)
axis(2)
xxs <- 0.5+(23*c(0,31,59,90,120,151,181,212,243,273,304,334,365)/365)
for (j in 1:12)
  polygon(c(xxs[j],xxs[j],xxs[j+1],xxs[j+1],xxs[j]),
          c(-1.65,-.5,-.5,-1.65,-1.65)+10)
text(xxs[1:12]+(23*16/365),9,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"))



###################################################
### code chunk number 18: Documento1_climaJBM.Rnw:483-485
###################################################
summary(with(subset(dts.clm,year>1972),aggregate(data.frame(tmean=tmean,tmin=tmin,tmax=tmax),list(yr=year),function(x) {max(x,na.rm=T)})))
summary(with(subset(dts.clm,year>1972),aggregate(data.frame(tmean=tmean,tmin=tmin,tmax=tmax),list(yr=year),function(x) {min(x,na.rm=T)})))


###################################################
### code chunk number 19: Documento1_climaJBM.Rnw:496-509
###################################################
ss <- subset(dts.clm,year>1999 & year<2012) ##subset(dts.clm,year>1972),
pttl <- with(ss,aggregate(data.frame(PP=PP),list(yr=year),function(x) {sum(x,na.rm=T)*365/length(x)}))
pver <- with(subset(ss, anual %in% levels(anual)[7:13]),
             aggregate(data.frame(PP=PP),list(yr=year),function(x) {sum(x,na.rm=T)*365/23*7/length(x)}))
phum <- with(subset(ss, anual %in% levels(anual)[14:21]),
             aggregate(data.frame(PP=PP),list(yr=year),function(x) {sum(x,na.rm=T)*365/23*8/length(x)}))

pttl <- merge(pttl,merge(phum,pver,by="yr"),by="yr")
summary(pttl$PP.x)
summary(pttl$PP.x/pttl$PP)
summary(pttl$PP.y/pttl$PP)
summary((pttl$PP.x+pttl$PP.y)/pttl$PP)



###################################################
### code chunk number 20: PrecipitacionTotalAnual
###################################################

pp <- with(ss,
           aggregate(data.frame(PP=PP),list(yr=year,anual=anual),
                     function(x) {sum(x,na.rm=T)*365/23*1/length(x)}))

par(xpd=T)
boxplot(PP~anual,pp,ylim=c(0,200),axes=F,ylab="Precipitación [mm]")
axis(2)
xxs <- 0.5+(23*c(0,31,59,90,120,151,181,212,243,273,304,334,365)/365)
for (j in 1:12)
  polygon(c(xxs[j],xxs[j],xxs[j+1],xxs[j+1],xxs[j]),
          c(-2.15,-.5,-.5,-2.15,-2.15)*5)
text(xxs[1:12]+(23*16/365),-7,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"))
box(,bty="n")



###################################################
### code chunk number 21: Documento1_climaJBM.Rnw:534-537 (eval = FALSE)
###################################################
## ##dias con precipitacion mayor a 1mm
## aggregate(dts.clm$PP>1,list(dts.clm$year),sum,na.rm=T)
## 


###################################################
### code chunk number 22: Tmodis
###################################################
tx <- with(subset(dts.sen,var %in% "LST_Day_1km"),aggregate(data.frame(tmax=val),list(yr=year,anual=anual),function(x) {max(x,na.rm=T)}))
tn <- with(subset(dts.sen,var %in% "LST_Night_1km"),aggregate(data.frame(tmin=val),list(yr=year,anual=anual),function(x) {min(x,na.rm=T)}))

tx <- subset(tx,is.finite(tx$tmax))
tn <- subset(tn,is.finite(tn$tmin))
par(xpd=T)
boxplot(tmax~anual,tx,ylim=c(10,45),axes=F,ylab="Temperatura [°C]",
        col="grey77",border="grey35")
boxplot(tmin~anual,tn,axes=F,add=T,
        col="grey12",border="grey35")
##boxplot(tmax~anual,tx,ylim=c(20,35),axes=F,add=T)
axis(2)
xxs <- 0.5+(23*c(0,31,59,90,120,151,181,212,243,273,304,334,365)/365)
for (j in 1:12)
  polygon(c(xxs[j],xxs[j],xxs[j+1],xxs[j+1],xxs[j]),
          c(-1.65,-.5,-.5,-1.65,-1.65)+10)
text(xxs[1:12]+(23*16/365),9,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"))



###################################################
### code chunk number 23: Documento1_climaJBM.Rnw:565-569
###################################################
summary(aggregate(tt$tmean,list(tt$yr),mean,na.rm=T)$x)
summary(aggregate(tx$tmax,list(tx$yr),max,na.rm=T)$x)
summary(aggregate(tn$tmin,list(tn$yr),min,na.rm=T)$x)



###################################################
### code chunk number 24: ET
###################################################
ee <- with(subset(dts.sen,var %in% "ET_1km"),aggregate(data.frame(ET=val),list(yr=year,anual=anual),function(x) {max(x,na.rm=T)}))
ep <- with(subset(dts.sen,var %in% "PET_1km"),aggregate(data.frame(PET=val),list(yr=year,anual=anual),function(x) {max(x,na.rm=T)}))


par(xpd=T)
boxplot(ET~anual,ee,ylim=c(0,100),axes=F,ylab="Evapotranspiración []",
        col="grey77",border="grey35")
##boxplot(PET~anual,ep,axes=F,add=T,
##        col="grey12",border="grey35")
##boxplot(tmax~anual,tx,ylim=c(20,35),axes=F,add=T)
axis(2)
xxs <- 0.5+(23*c(0,31,59,90,120,151,181,212,243,273,304,334,365)/365)
for (j in 1:12)
  polygon(c(xxs[j],xxs[j],xxs[j+1],xxs[j+1],xxs[j]),
          c(-3.65,-.5,-.5,-3.65,-3.65)+0)
text(xxs[1:12]+(23*16/365),-2,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"))



###################################################
### code chunk number 25: NDVI
###################################################
nn <- with(subset(dts.sen,var %in% "250m_16_days_NDVI"),aggregate(data.frame(NDVI=val),list(yr=year,anual=anual),function(x) {mean(x,na.rm=T)}))

par(xpd=T)
boxplot(NDVI~anual,nn,ylim=c(0,1),axes=F,ylab="NDVI",
        col="grey77",border="grey35")
##boxplot(PET~anual,ep,axes=F,add=T,
##        col="grey12",border="grey35")
##boxplot(tmax~anual,tx,ylim=c(20,35),axes=F,add=T)
axis(2)
xxs <- 0.5+(23*c(0,31,59,90,120,151,181,212,243,273,304,334,365)/365)
for (j in 1:12)
  polygon(c(xxs[j],xxs[j],xxs[j+1],xxs[j+1],xxs[j]),
          c(-.1,-.05,-.05,-.1,-.1)+0)
text(xxs[1:12]+(23*16/365),-.075,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"))



###################################################
### code chunk number 26: Documento1_climaJBM.Rnw:619-622 (eval = FALSE)
###################################################
##  with(merge(ep,pp,by=c("yr","anual")),cor.test(PP,PET))
## with(merge(ee,pp,by=c("yr","anual")),cor.test(PP,ET))
## 


