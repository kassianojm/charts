library(ggplot2)
theme_set(theme_bw())
library(zoo)
library(reshape)
library(dplyr)
library(ggforce)

install.packages("dplyr")
install.packages("reshape")
install.packages("ggplot2")
install.packages("zoo")




charts <-function(x,y,z,p,r,s){
  
  
  #limitmax<-10
  #limitmin<-8
  lmitadd<-2
  expnumber<-NULL
  outlines=9
  
  
  ########################################33
  
  df_batch_infos<-NULL
  batch_infos<-NULL
  #read data from spark
  batch_infos <- paste(VALNAME,expnumber,"/batch_infos.csv",sep = "")
  df_batch_infos<- read.csv(batch_infos, header = T,sep = ";")
  #df_batch_infos is old var df
  #batch_infos is old vat name
  #this loop read the TMSGSEC variable for every MQ log file
  DFMQmsg<-NULL
  dfMQ<-NULL
  mqrec<-NULL
  mqfile<-NULL
  MQLogFile<-NULL
  timestamp <-seq(from=1,to=execution,by=1)
  mqrec<-cbind(mqrec,timestamp[1:execution])
  colnames(mqrec)<-c("timestamp")
  for (i in 1:mqs)
  {
    #print(i)
    dfMQ<-NULL
    mqfile<-paste("/LOG",i,".csv",sep="")
    MQLogFile <- paste(VALNAME,expnumber,mqfile,sep = "")
    dfMQ <- read.csv(MQLogFile, header = T,sep = ",")[-c(1:outlines),]
    mqrec<- cbind(mqrec,dfMQ$TMSGSEC[1:execution])
    DFMQmsg<-data.frame(mqrec)
  }
  
  #rename col names from DFMQavg dataframe
  DFMQavg<-NULL
  for (i in 1:mqs){
    valueName<-NULL
    colmq<-paste("V",i+1,sep="")
    valueName<-paste('mq',i,sep="")
    names(DFMQmsg)[names(DFMQmsg) == colmq] <- valueName
  }
  
  
  
  
  #measuring avg and total throughput per MQ and general one
  DFMQavg<-NULL
  DFMQth<-NULL
  DFMQavg<-data.frame(timestamp)
  DFMQth<-data.frame(timestamp)
  mqcont<-NULL
  for (mqcont in 1:mqs){
    valueName<-NULL
    valueName<-paste('mq',mqcont,sep="")
    auxDFMQavg<-NULL
    auxDFMQth<-NULL
    for (i in 1:nrow(DFMQmsg)) 
    {
      auxDFMQavg[i]<-round((sum(DFMQmsg[,mqcont+1][1:i])/i)*9600/1024/1024,2)
      auxDFMQth[i]<-round((DFMQmsg[,mqcont+1][i])*9600/1024/1024,2)
    }
    DFMQavg[,mqcont+1]<-data.frame(auxDFMQavg)
    DFMQth[,mqcont+1]<-data.frame(auxDFMQth)
    colnames(DFMQavg)[mqcont+1]<-valueName
    colnames(DFMQth)[mqcont+1]<-valueName
  }
  
  DFmqavgTotal<-NULL
  DFmqThTotal<-NULL
  auxDFmqavgTotal<-NULL
  auxDFmqThTotal<-NULL
  auxmqs<-mqs+1
  if (mqs > 1){
    auxDFmqavgTotal<-rowSums(DFMQavg[,2:auxmqs][1:mqs])
    auxDFmqThTotal<-rowSums(DFMQth[,2:auxmqs][1:mqs])
    DFmqavgTotal<-data.frame(auxDFmqavgTotal)
    DFmqThTotal<-data.frame(auxDFmqThTotal)
    names(DFmqavgTotal)[1]<-"DFmqavgTotal"
    names(DFmqThTotal)[1]<-"DFmqThTotal"
  }else{
    auxDFmqavgTotal<-DFMQavg[,2]
    auxDFmqThTotal<-DFMQth[,2]
    DFmqavgTotal<-data.frame(auxDFmqavgTotal)
    DFmqThTotal<-data.frame(auxDFmqThTotal)
    names(DFmqavgTotal)[1]<-"DFmqavgTotal"
    names(DFmqThTotal)[1]<-"DFmqThTotal"
  }
  
  
  df_batch_infos_recs<-NULL
  #adjusting data from spark to show everything by second 
  df_batch_infos_recs<-round(rep(df_batch_infos$numRecords/2,each=2))
  
  df_batch_infos_throughput_per_sec<-NULL
  #rec per second in MB
  df_batch_infos_throughput_per_sec<-round(rep(df_batch_infos$numRecords/2,each=2)*9600/1024/1024,2)
  
  DFSparkavgTotal<-NULL
  auxDFSparkavgTotal<-NULL
  for (i in 1:nrow(DFMQmsg)) 
  {
    auxDFSparkavgTotal[i]<-round((sum(df_batch_infos_throughput_per_sec[1:i])/i),2)
    
  }
  DFSparkavgTotal<-data.frame(auxDFSparkavgTotal)
  

  timestamp1 <-seq(from=1,to=execution,by=1)
  
  thchart<-NULL
  thchart<-data.frame(timestamp1,DFmqThTotal$DFmqThTotal,DFSparkavgTotal$auxDFSparkavgTotal,DFmqavgTotal$DFmqavgTotal)
  names(thchart)[1]<-"timestamp1"
  names(thchart)[2]<-"auxDFmqthTotal"
  names(thchart)[3]<-"auxDFSparkavgTotal"
  names(thchart)[4]<-"auxDFmqavgTotal"
  
  MTH<-2000
  ThroughPut<-NULL
  ThroughPut <- ggplot(thchart, aes(x=timestamp1)) +
    geom_area(aes(y=auxDFmqavgTotal,fill="MQ AVG Throughput"))+
    geom_area(aes(y=auxDFSparkavgTotal,fill="Spark AVG Throughput"), alpha=0.5)+
    
    coord_cartesian(xlim=c(0,execution), ylim=c(0, MTH)) +
    xlab("Timestamp (s)") + 
    ylab("Throughput (MBps)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/20))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0,MTH+10, 100))  +
    scale_fill_manual(values=c("black","grey"))+
    
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      legend.position=c(0.5,0.95),
      plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
      panel.border=element_blank(), 
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(0.5,"line"),
    )
  plot(ThroughPut)
  
  savefile<-paste(folder,VALNAME,"Throughput.pdf",sep="/")
  ggsave(ThroughPut, file=savefile)
  
  ########################################33
  
  
  dfMQ<-NULL
  mqfile<-NULL
  MQLogFile<-NULL
  dfMQ<-NULL
  mqfile<-paste("/LOG",1,".csv",sep="")
  MQLogFile <- paste(VALNAME,expnumber,mqfile,sep = "")
  dfMQ <- read.csv(MQLogFile, header = T,sep = ",")
  
  mqcache<-NULL
  
  DFMQcache<-NULL
  mqcache<-cbind(mqcache,timestamp[1:execution])
  colnames(mqcache)<-c("timestamp")
  mqcache<- cbind(mqcache,dfMQ$state[1:execution])
  DFMQcache<-data.frame(mqcache)
  mqcache<- cbind(mqcache,dfMQ$globalState[1:execution])
  DFMQcache<-data.frame(mqcache)
  mqcache<- cbind(mqcache,dfMQ$globalState[1:execution])
  DFMQcache<-data.frame(mqcache)
  mqcache<- cbind(mqcache,dfMQ$state[1:execution])
  DFMQcache<-data.frame(mqcache)
  
  names(DFMQcache)[names(DFMQcache) == "V2"] <- "state"
  names(DFMQcache)[names(DFMQcache) == "V3"] <- "global"
  names(DFMQcache)[names(DFMQcache) == "V4"] <- "global1"
  names(DFMQcache)[names(DFMQcache) == "V5"] <- "state1"
  
  print("---")
  print(VALNAME)
  print(mean(DFMQcache$state,na.rm = TRUE))
  print("---")
  
  #method = lm
  Cacheglobal <- ggplot(DFMQcache, aes(x = timestamp)) +
    geom_line(aes(y=limitmax,color="max"), size=1) +
    geom_line(aes(y=limitmin,color="min"), size=1) +
    stat_smooth(aes(y=global,color="globalavg"),size=0.7) +
    stat_smooth(aes(y=state,color="stateavg"),size=0.5) +
    geom_point(aes(y=global1,color="global"),size=1.2,alpha = 0.3,shape=6) +
    geom_point(aes(y=state1,color="state"),size=1.2,alpha = 0.3,shape=2) +
    coord_cartesian(xlim=c(0,execution), ylim=c(0, limitmax+10)) +
    xlab("Timestamp (s)") + 
    ylab("Total of Data Cached at Executors' Memory (GB)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/20)) +
    scale_y_continuous(expand = c(0,0),breaks=seq(0,limitmax+10,lmitadd)) +
    scale_color_manual(values = c(
      globalavg = "#FF0000",
      stateavg = "blue",
      global = "#FF7F50",
      state = "#00BFFF",
      max = "#666666",
      min = "#666666"
    ),
    labels = c(
      globalavg = "Global State Avg.",
      stateavg = "State AVG", 
      global = "Global State", 
      state = "State",
      max = "Maximum",
      min = "Minimum"
    )) +
    theme(
      legend.position=c(0.5,0.95),
      plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      panel.border=element_blank(), 
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.width = unit(0.2, "cm"),
      legend.key.height= unit(0.2, 'cm'),
      legend.text=element_text(size=10)
    ) + 
    guides(color=guide_legend(override.aes=list(fill=NA,shape = c(NA,NA,NA,NA,NA,NA))))
  
  plot(Cacheglobal)
  
  savefile<-paste(folder,VALNAME,"Cacheglobal.pdf",sep="/")
  ggsave(Cacheglobal, file=savefile)
  
  valSD<-NULL
  valPT<-NULL
  valSD<- round(rep(df_batch_infos$schedulingDelay,each=2))
  valPT<- round(rep(df_batch_infos$processingDelay,each=2))
  PT<-round(colMeans(matrix(valPT[0:execution], nrow=2)))
  SD<-round(colMeans(matrix(valSD[0:execution], nrow=2)))
  LIMY<-NULL
  LIMYadj<-NULL
  LIMYadj<-1000
  LIMY<-10000
  DTPS<-NULL
  PT<-valPT[0:execution]
  SD<-valSD[0:execution]
  ptavg<-valPT[0:execution]
  sdavg<-valSD[0:execution]
  
  DTPS<-data.frame(timestamp,PT,SD,ptavg,sdavg)
  
  
  DTPS$window<-2000
  Delay <- NULL
  Delay <- ggplot(DTPS, aes(x = timestamp))+
    geom_point(aes(y=PT,color="pt"),size=1.2,alpha = 0.3,shape=2)+
    geom_point(aes(y=SD,color="sd"),size=1.2,alpha = 0.3,shape=6)+
    stat_smooth(aes(y=ptavg,color="avgpt"),size=0.5)+
    stat_smooth(aes(y=sdavg,color="avgsd"),size=0.5)+
    geom_line(aes(y=window,color="window"),size=1,alpha=0.8)+
    coord_cartesian(xlim=c(0,execution), ylim=c(0, LIMY)) +
    xlab("Timestamp (s)") + 
    ylab("Total Delay (ms)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/10))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0,LIMY, LIMYadj)) +
    scale_colour_manual( labels = c(pt="Proc. Time",
                                    sd="Sched. Delay",
                                    avgpt="Avg. Proc. Time",
                                    avgsd="Avg. Sched. Delay",
                                    window="Window Time"),
                         values=c(pt="#00BFFF",
                                  sd="#FF7F50",
                                  avgpt="blue",
                                  avgsd="#FF0000",
                                  window="black"))+
    theme(
      legend.position=c(0.5,0.95),
      plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      panel.border=element_blank(), 
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.width = unit(0.2, "cm"),
      legend.key.height= unit(0.2, 'cm'),
      legend.text=element_text(size=9)
    ) + guides(color=guide_legend(override.aes=list(fill=NA,shape = c(NA, NA,NA, NA,NA))))
  
  
  plot(Delay) 
  savefile<-paste(folder,VALNAME,"Delay.pdf",sep="/")
  ggsave(Delay, file=savefile)
  
  
  if (mqs == 1) {
    socketnumerb<-8
  }
  
  if (mqs == 2) {
    socketnumerb<-4
  }
  
  if (mqs == 4) {
    socketnumerb<-2
  }
  
  if (mqs == 8) {
    socketnumerb<-1
  }
  
  
  
  mqreceiver<-NULL
  DFMQreceiver<-NULL
  mqfile<-NULL
  MQLogFile<-NULL
  dfMQ<-NULL
  mqreceiver<-cbind(mqreceiver,timestamp[1:execution])
  colnames(mqreceiver)<-c("timestamp")
  for (i in 1:mqs) 
  {
    mqfile<-paste("/LOG",i,".csv",sep="")
    MQLogFile <- paste(VALNAME,expnumber,mqfile,sep = "")
    dfMQ <- read.csv(MQLogFile, header = T,sep = ",")
    sumREC<-5
    for (s in 1:socketnumerb) 
    {
      mqreceiver<-cbind(mqreceiver,dfMQ[,sumREC][1:execution])
      
      sumREC<-5+6
      DFMQreceiver<-data.frame(mqreceiver)
    }
  }
  
  
  
  
  value<-mqs*socketnumerb
  valreceiver<-NULL
  for (i in 1:value){
    teste<-NULL
    valueName<-NULL
    colmq<-paste("V",i+1,sep="")
    valueName<-paste('socket_',i,sep="")
    names(DFMQreceiver)[names(DFMQreceiver) == colmq] <- valueName
    if( i == 1 ){
      valreceiver<-paste(valreceiver,"socket_",i,sep="")
    }else{
      valreceiver<-paste(valreceiver,",socket_",i,sep="")
      
    }
  }
  
  
  receivers<-melt(DFMQreceiver, id.vars = c("timestamp"),measure.vars = c(unlist(strsplit(valreceiver,","))))
  
  
  listreveiver<-NULL
  for (i in 1:value){
    listreveiver<-paste(listreveiver,"Exe ",i,",",sep="")
  }
  
  variable_names <-  as.list(unlist(strsplit(listreveiver, ',')))
  
  
  
  variable_labeller <- function(variable,value){
    return(variable_names[value])
  }
  
  
  receiver <-  ggplot(receivers, aes(x = timestamp,y=value))+
    geom_area(size=0.7)+
    
    xlab("Timestamp (s)") + 
    ylab("Throughput Received Per Executor (MBps)") +
    coord_cartesian(xlim=c(0,execution), ylim=c(0, 600) )+
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/20))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0,600,200)) +
    theme(
      # legend.position=c(0.5,1.06),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      #panel.border=element_blank(), 
      panel.spacing = unit(0.6, "lines"),
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(0.5,"line"),
      strip.background = element_rect(fill="gray"),
      strip.text = element_text(colour = 'black',face = "bold")
    )+
    facet_wrap(~variable,ncol=1,strip.position="right",labeller=variable_labeller)
  plot(receiver)
  
  savefile<-paste(folder,VALNAME,"receivers.pdf",sep="/")
  ggsave(receiver, file=savefile)
  
  
  
  nodes=9
  
  time<-seq(from=1,to=execution,by=1)
  memused<-NULL
  membuff<-NULL
  memucache<-NULL
  memfree<-NULL
  DFused<-NULL
  DFbuf<-NULL
  DFcache<-NULL
  DFfree<-NULL
  memused<-cbind(time)
  DFused<-data.frame(memused)
  membuff<-cbind(time)
  DFbuf<-data.frame(membuff)
  memucache<-cbind(time)
  DFcachec<-data.frame(memucache)
  memfree<-cbind(time)
  DFfree<-data.frame(memfree)
  for (i in 1:nodes)
  {
    print(i)
    dfrec<-NULL
    recfile<-NULL
    recLogFile<-NULL
    recfile<-paste("/RESOURCES",i,".csv",sep="")
    recLogFile<-paste(VALNAME,expnumber,recfile,sep = "")
    dfrec<-read.csv(recLogFile, header = F,  sep = ";", row.names=1, skip = 1)
    memused<-cbind(memused,dfrec$V2[1:execution]/1024/1024/1024)
    DFused<-data.frame(round(memused,0))
    print(recLogFile)
    #membuff<-cbind(membuff,dfrec$V3[1:execution]/1024/1024/1024)
    #DFbuf<-data.frame(round(membuff,0))
    #memucache<-cbind(memucache,dfrec$V4[1:execution]/1024/1024/1024)
    #DFcachec<-data.frame(round(memucache,0))
    #memfree<-cbind(memfree,dfrec$V5[1:execution]/1024/1024/1024)
    #DFfree<-data.frame(round(memfree,0))
  }
  
  
  
  #adjust collum names
  valreceiver<-NULL
  for (i in 1:nodes+1)
  {
    valueName<-NULL
    colmq<-paste("V",i,sep="")
    # if( i == 1 ){
    #   valreceiver<-paste(valreceiver,"time",sep="")
    #  valueName<-paste('time',sep="")
    # names( DFused)[names( DFused) == colmq] <- valueName
    #}
    if( i == 2 )
    {
      valreceiver<-paste(valreceiver,"MDriver",sep="")
      valueName<-paste('MDriver',sep="")
      names( DFused)[names( DFused) == colmq] <- valueName
    }
    
    if( i > 1 )
    {
      if( i <= nodes ) valreceiver<-paste(valreceiver,",MU",i-1,sep="")
      valueName<-paste('MU',i-2,sep="")
      names(DFused)[names(DFused) == colmq] <-valueName
    }
  }
  
  
  variable<-NULL
  MUSED<-NULL
  MUSED = melt(DFused, id.vars = c("time"),measure.vars=c(unlist(strsplit(valreceiver,","))))
  limit<-250
  limitpercen<-20
  
  listreveiver<-NULL
  for (i in 1:nodes+1){
    if( i > 2 )
    {
      listreveiver<-paste(listreveiver,"Exe ",i-2,",",sep="")
    }else{
      listreveiver<-paste(listreveiver,"Driver",",",sep="")
      
    }
  }
  
  variable_names <-  as.list(unlist(strsplit(listreveiver, ',')))
  
  
  
  variable_labeller <- function(variable,value){
    return(variable_names[value])
  }
  
  
  for (i in 1:execution){ 
    DFused$Mtotal[i]<-sum(DFused[i,][3:10])
  }  
  
  for (i in 3:10){ 
    print("Global Memory AVG - Executor ")
    print(i-2)
    print(mean(DFused[,i],na.rm = TRUE))
    
  }  
  
  
  print("Global Memory AVG - Executors only")
  print(VALNAME)
  print(mean(DFused$Mtotal,na.rm = TRUE))
  print("---")
  
  
  MemoryUtilization<-NULL
  MemoryUtilization<-ggplot(MUSED, aes(x = time, y = value))+
    geom_area(position="stack", stat="identity",aes(fill = variable))+
    geom_hline(yintercept=174, color = "red", size=0.5)+
    
    geom_hline(yintercept=98, color = "black", size=0.5, linetype = "dotdash")+
    annotate("text", x =850, y = 130, label = "Storage and Execution Region Limits", size = 2.5)+
    annotate("text", x =850, y = 200, label = "JVM Heap Memory Limit", size = 2.5)+
    coord_cartesian(xlim=c(0,execution), ylim=c(0, limit)) +
    xlab("Timestamp (s)") + 
    ylab("Memory Utilization (GB)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/10))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0, limit, 50))+ 
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      legend.text = element_text(size = 10),
      legend.position="none",
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(0.5,"line"),
      strip.background = element_rect(fill="gray"),
      strip.text = element_text(colour = 'black',face = "bold")
    )+
    facet_wrap(~variable,ncol=1,strip.position="right",labeller=variable_labeller)
  plot(MemoryUtilization)
  savefile<-paste(folder,VALNAME,"MemUtilization.pdf",sep="/")
  ggsave(MemoryUtilization, file=savefile)
  
  
  
  GlobalMemoryUtilization<-NULL
  GlobalMemoryUtilization<-ggplot(MUSED, aes(x = time, y = value))+
    geom_area(position="stack", stat="identity",aes(fill = variable)) +
    geom_hline(yintercept=1392, color = "red", size=0.5)+
    annotate("text", x = 900, y = 1430, label = "Available JVM Heap Memory")+
    scale_fill_discrete(name = "Dose", labels = c("Driver", "Executor 1", 
                                                  "Executor 2","Executor 3", 
                                                  "Executor 4","Executor 5",
                                                  "Executor 6","Executor 7", "Executor 8")) +
    
    coord_cartesian(xlim=c(0,execution), ylim=c(0, 1700)) +
    xlab("Timestamp (s)") + 
    ylab("Global Memory Utilization (GB)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/20))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0, 1700, 150))+
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.key.size = unit(0.5,"line"),
      legend.position=c(0.90,0.66),
      plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
      strip.background = element_rect(fill="gray"),
      strip.text = element_text(colour = 'black',face = "bold")
    )  
  
  
  plot(GlobalMemoryUtilization)
  
  savefile<-paste(folder,VALNAME,"GlobalMemUtilization.pdf",sep="/")
  ggsave(GlobalMemoryUtilization, file=savefile)
  
  
  
  
  dfMQ<-NULL
  mqfile<-NULL
  MQLogFile<-NULL
  dfMQ<-NULL
  mqfile<-paste("/LOG",1,".csv",sep="")
  MQLogFile <- paste(VALNAME,expnumber,mqfile,sep = "")
  dfMQ <- read.csv(MQLogFile, header = T,sep = ",")
  
  mqloss<-NULL
  DFMQloss<-NULL
  mqloss<-cbind(mqloss,timestamp[1:execution])
  colnames(mqloss)<-c("timestamp")
  mqloss<- cbind(mqloss,dfMQ$THloss[1:execution])
  DFMQloss<-data.frame(mqloss)
  names(DFMQloss)[names(DFMQloss) == "V2"] <- "loss"
  
  adjust<-DFMQloss[which.max(DFMQloss$loss),2]
  
  
  
  
  losschart<-NULL
  losschart <- ggplot(DFMQloss, aes(x = timestamp))+
    geom_area(aes(y=loss,fill="Throughput Loss over time"))+
    geom_line(aes(y=5,fill="Maximum Acceptable Loss"),size=1, color="red")+
    coord_cartesian(xlim=c(0,execution), ylim=c(0, adjust+2)) +
    xlab("Timestamp (s)") + 
    ylab("Throughput Loss (%)") +
    scale_x_continuous(expand = c(0,0),breaks=seq(0,execution, execution/20))+
    scale_y_continuous(expand = c(0,0),breaks=seq(0,adjust+2,1)) +
    scale_fill_manual(values=c("red","yellow"))+
    
    theme(
      legend.position=c(0.5,0.95),
      plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3),
      
      panel.border=element_blank(), 
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(0.5,"line")
    )
  plot(losschart)
  

  
  savefile<-paste(folder,VALNAME,"loss.pdf",sep="/")
  ggsave(losschart, file=savefile)
  
  
  print(VALNAME)
  
}

#### TESTING LIMITS FROM 8 TO 20)

folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/"
execution<-1800
mqs=8
setwd(folder)


VALNAME="BP_888_dynamic_eno_8-20_SUMServer_8_GB-rep1"
limitmax=20
limitmin=8
charts(folder,VALNAME,mqs,limitmax,limitmin)

VALNAME="BP_889_min-8-5-eno-_GlobalSUMServer_20_GB-rep1"
limitmax=20
limitmin=8
charts(folder,VALNAME,mqs,limitmax,limitmin)

VALNAME="BP_889_min-8-5-eno-_GlobalHistogramServer_20_GB-rep1"
limitmax=20
limitmin=8
charts(folder,VALNAME,mqs,limitmax,limitmin)


############################# LONG - selected
folder="/DATA/Dropbox/Erods/LOGS/Adapt-BP-bppini/Grenoble-Dahu/Sample/charts/E3/long/"
mqs=8
setwd(folder)

VALNAME="long-ok-BP_888_adap-8-20_SUMServer_8_GB-rep1"

execution<-7200
limitmax=20
limitmin=8
charts(folder,VALNAME,mqs,limitmax,limitmin)
############################# 


