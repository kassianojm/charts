
stat<-function(x,y,z){
  name<-NULL
  m<-NULL
  a<-10
  b<-890
  # for (i in 1:REP){
  #ti verify case there are more one repetition, conditional is wired.
  #  if(REP != 99){
  name <- paste(VALNAME,"/batch_infos.csv",sep = "")
  #   REP=1
  #name <- paste(VALNAME,i,"/batch_infos.csv",sep = "") - use it case we have more than one exp
  #}else{
  
  # name <- paste(VALNAME,"/batch_infos.csv",sep = "")
  #  REP=1
  #}
  df <- read.csv(name, header = T,sep = ";")
  
  rmin    = round(min(df$numRecords[a:b]))
  rmean   = round(mean(df$numRecords[a:b]))
  rmedian = round(median(df$numRecords[a:b]))
  rmax    = round(max(df$numRecords[a:b]))
  throughput = round((mean(df$numRecords[a:b])*9600/1024/1024)/2)
  ptmin    = round(min(df$processingDelay[a:b]))
  ptmean   = round(mean(df$processingDelay[a:b]))
  ptmedian = round(median(df$processingDelay[a:b]))
  ptmax    = round(max(df$processingDelay[a:b]))
  
  sdtmin   = round(min(df$schedulingDelay[a:b]))
  sdmean   = round(mean(df$schedulingDelay[a:b]))
  sdmedian = round(median(df$schedulingDelay[a:b]))
  sdmax    = round(max(df$schedulingDelay[a:b]))
  m <- rbind(m,c(rmin,rmean,rmedian,rmax,ptmin,ptmean,ptmedian,ptmax,sdtmin,sdmean,sdmedian,sdmax,throughput))
  #}
  colnames(m)<- c("Min #Rec", 
                  "AVG #Rec", 
                  "Median #Rec", 
                  "Max. #Rec", 
                  "Min. PT (ms)", 
                  "AVG PT (ms)", 
                  "Median PT (ms)", 
                  "Max (ms)", 
                  "Min. SD (ms)", 
                  "AVG SD (ms)", 
                  "Median SD (ms)", 
                  "Max. SD (ms)",
                  "Throughput (MBps)"
  )
  
  file<-paste(folder,VALNAME,"/overall_statistics.csv",sep="")
  
  write.table(m,file,sep=",",row.names=TRUE,col.names=NA,append = TRUE,quote=F)
  
  Mfinal<-NULL
  AVGPT <-NULL
  AVGPT     <- round(mean(m[,6]))
  StDEVPT  <- round(sd(m[,6]),2)
  PT     <- round((100*StDEVPT)/AVGPT,2)
  
  AVGrec <-NULL
  AVGrec    <- round(mean(m[,2]))
  StDEVrec  <- round(sd(m[,2]),2)
  Rec    <- round((100*StDEVrec)/AVGrec,2)
  
  
  AVGsd <-NULL
  AVGsd    <- round(mean(m[,10]))
  StDEVsd  <- round(sd(m[,10]),2)
  SD    <- round((100*StDEVsd)/AVGsd,2)
  
  
  TH<-NULL
  TH<-round((AVGrec/2)*9600/1024/1024,2)
  
  Mfinal <- cbind(Mfinal,c(Rec,
                           PT,
                           SD,
                           TH,
                           AVGPT,
                           AVGsd,
                           AVGrec/2
  ))
  
  colnames(Mfinal)<- c("Values")
  rownames(Mfinal)<- c("STD. Dev #Rec (%)   ",
                       "STD. Dev. PT  (%)   ",
                       "STD. Dev SD   (%)   ", 
                       "AVG Th.       (MBps)",
                       "AVG PT        (ms)  ",
                       "AVG SD        (ms)  ",
                       "AVG Rec       (s)   " 
  )
  
  
  
  print(VALNAME)
  print(Mfinal)
  
  file<-paste(folder,VALNAME,"/avg_statistics.csv",sep="")
  
  write.table(Mfinal, file,sep=",",row.names=TRUE,col.names=NA,append = TRUE,quote=F)
  
}

folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/"
setwd(folder)
REP<-99

VALNAME="BP_888_dynamic_eno_8-20_SUMServer_8_GB-rep1"
stat(folder,VALNAME,REP)

VALNAME="BP_889_min-8-5-eno-_GlobalSUMServer_20_GB-rep1"
stat(folder,VALNAME,REP)

VALNAME="BP_889_min-8-5-eno-_GlobalHistogramServer_20_GB-rep1"
stat(folder,VALNAME,REP)

############################# LONG
folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/long/"
setwd(folder)
REP<-99

VALNAME="long-ok-BP_888_adap-8-20_SUMServer_8_GB-rep1"
stat(folder,VALNAME,REP)





