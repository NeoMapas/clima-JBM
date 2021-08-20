##R --vanilla
require(chron)
require(raster)
setwd("~/CEBA/tmp")
hoy <- format(Sys.time(), "%Y%m%d")
##Datos estacion climatica
##*** Clima en Maracaibo-La Chinita Diciembre de 2011 *****
##Datos reportados por la estación meteorológica: 804070 (SVMC)
##Latitud: 10.56 | Longitud: -71.73 | Altitud: 66
##El_tiempo_en_Maracaibo-La_Chinita

##http://www.tutiempo.net/clima/Maracaibo-La_Chinita/804070.htm
## cd ~/CEBA/data/JardinBotanicoMaracaibo/climaviejo
for (yy in 1959:2014) {
  for (mm in 1:12) {
 ##    system(sprintf("wget --continue 'http://www.tutiempo.net/clima/Maracaibo-La_Chinita/%02d-%04d/804070.htm' --output-document=/home/jferrer/CEBA/data/JardinBotanicoMaracaibo/climaviejo/E804070_%04d_%02d.htlm",mm,yy,yy,mm))
  }
}

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

################
## mapoteca
###############
mptc <- "~/CEBA/lib/mapoteca/"
mapaDB <- "Maracaibo"
LC <- raster(sprintf("%s/%s/%s",mptc,mapaDB,"MCD12Q1.A2005001.Maracaibo_LC1.tif"))
bosque <- raster(sprintf("%s/%s/GFC2013/%s",mptc,mapaDB,"GFC2013.Maracaibo.treecover2000.tif"))
perdida <- raster(sprintf("%s/%s/GFC2013/%s",mptc,mapaDB,"GFC2013.Maracaibo.loss.tif"))
ganancia <- raster(sprintf("%s/%s/GFC2013/%s",mptc,mapaDB,"GFC2013.Maracaibo.gain.tif"))
frags <- clump(bosque>40)
sfrags <- clump(ganancia)

tt <- rev(sort(table(values(frags))))
values(frags)[values(frags) %in% as.numeric(names(tt)[tt<200])] <- 999
table(values(sfrags))
plot(sfrags %in% 446)
tt <- rev(sort(table(values(frags))))

slc <- c(426, 441, 442) 
plot(frags %in% slc[1:2])

r0 <- (frags %in% slc[1:2]) + sfrags %in% 446
JBM <- rasterToPolygons(r0, fun=function(x) {x %in% 1},dissolve=T)

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





plot(PP~fch,dts.clm)
plot(tt,dts.clm$tmin)
aggregate(dts.clm$T,list(yy=years(tt)),mean,na.rm=T)
hstr <- aggregate(data.frame(PP=dts.clm$PP,DP=!is.na(dts.clm$PP)),
                  list(yy=as.numeric(as.character(years(tt)))),
                  ##years(tt),Q=quarters(tt)),
                  sum,na.rm=T)
hstr$PT <- hstr$PP*365/hstr$DP
table(is.na(dts.clm$T))
plot(PT~yy,hstr,type="h",col=2,lty=2)
points(PP~yy,hstr,type="h",col=2,lty=1)
points(PP~yy,hstr,type="p",col=2,pch=18)

plot(dts.clm$T~tt)
plot(dts.clm$T~tt,cex=.5)


plot(T~tt,dts.clm,subset=tt>"71/12/31")

rsm <- aggregate(data.frame(T=dts.clm$T),
                 list(yy=as.numeric(as.character(years(tt))),
                      mm=as.numeric(months(tt))/12),mean,na.rm=T)
rsm$fch <- rsm$yy+rsm$mm
rsm <- rsm[order(rsm$fch),]
plot(T~fch,rsm,cex=.5)

plot(T~fch,rsm,cex=.5)
plot(T~fch,rsm,cex=.5,type="l")

plot(H~tt,dts.clm)


dir(sprintf("%s/%s/",mptc,mapaDB))


plot(JBM,border=NA)
plot(bosque>0,add=T)
plot(ganancia,add=T,col=c(NA,"blue"))
plot(JBM,add=T)









plot(val~fch,dts,subset=var %in% "LST_Day_1km")
points(val~fch,dts,subset=var %in% "LST_Night_1km",col=2)

plot(val~fch,dts,subset=var %in% "PET_1km")
points(val~fch,dts,subset=var %in% "LST_Night_1km",col=2)

plot(val~fch,dts,subset=var %in% "Fpar_1km")
points(val~fch,dts,subset=var %in% "LST_Night_1km",col=2)
plot(val~fch,dts,subset=var %in% "250m_16_days_NDVI" & j==44)



lines(dts.clm$fch,dts.clm$T)
lines(dts.clm$fch,dts.clm$tmax)

## generalmente coinciden
LST.ts$fch %in% fff
## se puede usar interpolate




plot(LST~fch,LST.ts,col=(dn %in% "dia")+1)
lines(fff,dts.clm$tmax)
lines(fff,dts.clm$tmin)




###########
##
############

aggregate(dts.clm$PP,list(cut(dts.clm$anual,breaks=24)),sum,na.rm=T)

sre <- subset(dts,var %in% "250m_16_days_EVI")
sre <- subset(dts,var %in% "LST_Night_1km")
sre <- subset(dts,var %in% "LST_Day_1km")
sre <- subset(dts,var %in% "Lai_1km")
sre <- subset(dts,var %in% "ET_1km")
sre$tiempo <- sre$fch-2000
sre$anual <- sre$fch-floor(sre$fch)

boxplot(val~anual,data=sre)

boxplot(T~anual,data=dts.clm)

sre <- subset(dts,var %in% "250m_16_days_NDVI")
sre$tiempo <- sre$fch-2000
sre$anual <- sre$fch-floor(sre$fch)
boxplot(val~anual,sre,tiempo<=1)

mdl0 <- gls(val~tiempo +
            I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
            I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)),
            data=sre,subset=j==20)
mdl1 <- lme(val~tiempo +
            I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
            I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)),
            data=sre,random=~1|j)
## muy lento
mdl2 <- lme(val~tiempo +
            I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
            I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)),
            data=sre,random=~1|j,
            correlation=corCAR1(0.2,~tiempo|j))

plot(val~anual,sre)
lines(c(0:24)/24,
      predict(mdl1,newdata=data.frame(anual=c(0:24)/24,
                     tiempo=0,j=1),level=0),col=2)
lines(c(0:24)/24,
      predict(mdl1,newdata=data.frame(anual=c(0:24)/24,
                     tiempo=10,j=1),level=0),col=2)

## mal ajuste...
boxplot(val~anual,sre,tiempo<=1)
lines(c(1:23),
      predict(mdl1,newdata=data.frame(anual=c(1:23)/23,
                     tiempo=c(1:23)/23,j=1),level=0),col=2)

##dos outliers... 1 y 6
hist(ranef(mdl1)[,1])



plot(mdl1)



mdl0 <- nlsList(val~tiempo +
                I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
                I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)) | j,
                sre)

correlation=corCAR1(0.5,~tiempo),
             random=~1|j)
nts1 <- ts(sre$val,frequency=23,start=c(2000,4))

tiempo <- time(nts1)-start(nts1)[1]
anual <- time(nts1)-floor(time(nts1))
require(nlme)

gfit1 <- try(gls(nts1~tiempo +
                 I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
                 I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)) ,
                 correlation=corCAR1(0.5,~tiempo)))
gfit2 <- try(gls(nts1~tiempo +
                 I(sin(2*pi*anual)) + I(cos(2*pi*anual)),
                 correlation=corCAR1(0.5,~tiempo)))
gfit3 <- try(gls(nts1~
                 I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
                 I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2)) ,
                 correlation=corCAR1(0.5,~tiempo)))
gfit4 <- try(gls(nts1~tiempo +
                 I(sin(2*pi*anual)) + I(cos(2*pi*anual)) +
                 I(sin(2*pi*anual*2)) + I(cos(2*pi*anual*2))))

      x <- as.numeric(time(nts1))


################
## viejo
###############
##google earth # no nos hace falta
##Coordenadas de ubicación del JBM
##lat = c(10.5242,10.56,10.5711614);
##lon = c(-71.72012318,-71.73,-71.7498284);
##center = c(mean(lat), mean(lon));
##zoom <- min(MaxZoom(range(lat), range(lon)));
##this overhead is taken care of implicitly by GetMap.bbox();              
##MyMap <- GetMap(center=center, zoom=zoom,markers = "&markers=color:blue|label:S|10.56,-71.73", maptype="satellite",destfile = "~/Escritorio/MyTile1.png");
     



r0 <- raster("~/CEBA/data/JardinBotanicoMaracaibo/JR-JBM/STACK_DIC2009_MAR2011_JBM_NDVI_MEAN1.tif")
s0 <- shapefile("~/CEBA/data/JardinBotanicoMaracaibo/JR-JBM/Catastro_JBM")
r0@crs

LC <- raster("~/modisSS/08May2013_12:57:01_567279737L10.58815L-71.71004S57L57_MOD13Q1/MCD12Q1.A2005001.h10v07.005.2011084235212_Land_Cover_Type_1.tif")
EVI <- raster("~/modisSS/08May2013_12:57:01_567279737L10.58815L-71.71004S57L57_MOD13Q1/MOD13Q1.A2000049.h10v07.005.2006269232146_250m_16_days_EVI.tif")

##LCxy <- projectRaster(LC,r0)
LCxy <- projectRaster(LC,r0,method="ngb")
EVIxy <- projectRaster(EVI,r0,method="ngb")

##plot(r0)
plot(EVIxy)
plot(s0,add=T)



(load("~/modisSS/08May2013_12:57:01_567279737L10.58815L-71.71004S57L57_MOD13Q1/NDVItrend.Rda"))

symbols(xys[,1],xys[,2],circles=abs(rsms$trend),inches=.05,bg=3+sign(rsms$trend),fg=3+sign(rsms$trend))



##gpsbabel -i gpx -f 20140328_visitaJardin.gpx -o csv,prefer_shortnames=1 -F prueba.csv
wpts <- read.csv("~/CEBA/data/JardinBotanicoMaracaibo/prueba.csv",header=F)
coordinates(wpts) <- 2:1
proj4string(wpts) <-  EVI@crs
wpxy <- spTransform(wpts,r0@crs)


plot(r0,ylim=c(1171000,1172300),xlim=c(202000,204500))
plot(s0,add=T)
points(wpxy,col=2)

##digit <- locator()
crds <- data.frame(digit$x,digit$y)
bsq <- Polygons(list(Polygon(rbind(crds,head(crds,1)))),ID=1)

bsq.xy <- SpatialPolygons(list(bsq), proj4string=wpxy@proj4string)

save(file="~/CEBA/Rdata/JBM.rda",bsq.xy)
plot(r0,ylim=c(1171000,1172300),xlim=c(202000,204500))
plot(s0,add=T)
points(wpxy,col=2)
plot(bsq.xy,border="maroon",lwd=4,add=T)
text(wpxy,wpxy$V3,cex=0.8,font=2)

## parcelas <- locator()
parcelas <- data.frame(x=parcelas$x,y=parcelas$y)

coordinates(parcelas) <- 1:2
proj4string(parcelas) <-  wpxy@proj4string
points(parcelas)
par.ll <- spTransform(parcelas,EVI@crs)
##write.csv(par.ll,file="Parcelas.csv")
##gpsbabel -i csv -f  ~/CEBA/tmp/Parcelas.csv -o gpx -F Parcelas.gpx
##sudo gpsbabel -i gpx -f Parcelas.gpx -o garmin -F usb:


plot(EVI,xlim=c(-71.7253,-71.6956),ylim=c(10.58,10.5967))
with(wpts,text(V2,V1,V3,cex=.5))


plot(EVI)

with(wpts,text(V2,V1,V3))

points(V1~V2,wpts,V3 %in% 35)
