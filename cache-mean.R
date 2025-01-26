

mean_cache <-function(x,y){
  expnumber<-NULL
  execution<-1800
  outlines=9
  dfMQ<-NULL
  cachestate<-NULL
  mqfile<-paste("/LOG",1,".csv",sep="")
  MQLogFile <- paste(VALNAME,expnumber,mqfile,sep = "")
  dfMQ <- read.csv(MQLogFile, header = T,sep = ",")[-c(1:outlines),]
  cachestate<- dfMQ$state[1:execution]
  print(VALNAME)
  print(mean(cachestate,na.rm = TRUE))
  
}

folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/"
setwd(folder)
mqs=8
mqs<-NULL
VALNAME<-NULL

VALNAME="BP_888_dynamic_eno_8-20_SUMServer_8_GB-rep1"
mean_cache(folder,VALNAME)
#11.7275

VALNAME="BP_889_min-8-5-eno-_GlobalSUMServer_20_GB-rep1"
mean_cache(folder,VALNAME)
#13.30417

VALNAME="BP_889_min-8-5-eno-_GlobalHistogramServer_20_GB-rep1"
mean_cache(folder,VALNAME)
#10.37992


############################# LONG 
folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/long/"
setwd(folder)

VALNAME="long-ok-BP_888_adap-8-20_SUMServer_8_GB-rep1"
mean_cache(folder,VALNAME)
execution<-7200

############################# 

