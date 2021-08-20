##R --vanilla
setwd("~/tmp/CEBA")
hoy <- format(Sys.time(), "%Y%m%d")
mi.dir <- "400_InventarioJardinBotanicoMaracaibo"

mi.arch <- "Documento1_climaJBM"
titulo <- "EstacionalidadClimaJBM"
cdg.doc <- ""
mi.arch <- "Documento2_MonitoreoAcustico"
titulo <- "Ensayo_Monitoreo_Acustico_JBM"
cdg.doc <- ""
if (file.exists("~/Dropbox/CEBA/doc/")) {
	mi.path <- "~/Dropbox/CEBA/doc/"
} else {
	mi.path <- "~/CEBA/doc/"
}
##system(sprintf("rm %s.*",mi.arch))
Sweave(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""),eps=F)

mi.arch <- "Documento3_ManuscritoMonitoreoAcustico"
Sweave(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""),eps=F)
Stangle(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""))
tools::texi2dvi(paste(mi.arch,".tex",sep=""), pdf=TRUE)

##system(sprintf("evince %s.pdf &",mi.arch))

system(paste("mv ",mi.arch,".pdf ",mi.path,"/",mi.dir,"/",hoy,"_",titulo,".pdf",sep=""))
system(paste("mv ",mi.arch,".R ",mi.path,"/",mi.dir,"/",hoy,"_",titulo,".R",sep=""))

