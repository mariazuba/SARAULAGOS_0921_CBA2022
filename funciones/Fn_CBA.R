CBA<-function(admb,dir.0,dir.1,Carpeta,system,l_escRec){
  
  dir<-paste(dir.0,Carpeta,sep="")
  
  dat_admb<-paste(admb,".dat",sep="")
  tpl_admb<-paste(admb,".tpl",sep="")
  rep_admb<-paste(admb,".rep",sep="")
  std_admb<-paste(admb,".std",sep="")
  
  unlink(dir,recursive=T) #borra la carpeta "
  dir.create(file.path(dir.0,Carpeta))#crea la carpeta nuevamente
  setwd(dir.1);file.copy(c(dat_admb,tpl_admb),dir) #copia los archivos de la carpeta 
  setwd(dir) 
  
  #if(system=="mac"){
   # system(paste("~/admb-12.2/admb",admb,sep=" "))
   # system(paste("./",admb,sep=""))
  #}
  
  #if(system=="windows"){
   # system(paste("admb",admb,sep=" "))
   # system(admb)}
  
  data.0 <- readLines(dat_admb,encoding="UTF-8")
  data.1 <- data.0
  escRec      <- c(0,1,2,3)
  
  for(i in 1:4){
    data.1[l_escRec]  <- escRec[i]
    
    cat(data.1,file=(can<-file(paste(admb,"s",escRec[i],".dat", sep=''),"wb",encoding="UTF-8")),sep="\n")
    close(can)
    
    setwd(dir.1)
    file.copy(c(paste(admb,".tpl",sep="")),dir)
    
    setwd(dir)
    file.rename(paste(admb,".tpl",sep=""),paste(admb,"s",escRec[i],".tpl",sep="")) 
    
    if(system=="mac"){
      system(paste("~/admb-12.2/admb ",admb,"s",escRec[i],sep=""))
      system(paste("./",admb,"s",escRec[i],sep="")) }
    
    if(system=="windows"){
      system(paste("admb ",admb,"s",escRec[i],sep=""))
      system(paste(admb,"s",escRec[i],sep="")) }
    
    file.remove(paste(admb,"s",escRec[i],".htp", sep=""),
                paste(admb,"s",escRec[i],".cpp", sep=""),
                paste(admb,"s",escRec[i],".obj", sep=""),
                paste(admb,"s",escRec[i],".p01", sep=""),
                paste(admb,"s",escRec[i],".b01", sep=""),
                paste(admb,"s",escRec[i],".r01", sep=""),
                paste(admb,"s",escRec[i],".p02", sep=""),
                paste(admb,"s",escRec[i],".b02", sep=""),
                paste(admb,"s",escRec[i],".r02", sep=""),
                paste(admb,"s",escRec[i],".p03", sep=""),
                paste(admb,"s",escRec[i],".b03", sep=""),
                paste(admb,"s",escRec[i],".r03", sep=""),
                paste(admb,"s",escRec[i],".p04", sep=""),
                paste(admb,"s",escRec[i],".b04", sep=""),
                paste(admb,"s",escRec[i],".r04", sep=""),
                paste(admb,"s",escRec[i],".p05", sep=""),
                paste(admb,"s",escRec[i],".b05", sep=""),
                paste(admb,"s",escRec[i],".r05", sep=""),
                paste(admb,"s",escRec[i],".p06", sep=""),
                paste(admb,"s",escRec[i],".b06", sep=""),
                paste(admb,"s",escRec[i],".r06", sep=""),
                paste(admb,"s",escRec[i],".p07", sep=""),
                paste(admb,"s",escRec[i],".b07", sep=""),
                paste(admb,"s",escRec[i],".r07", sep=""),
                paste(admb,"s",escRec[i],".par", sep=""),
                paste(admb,"s",escRec[i],".bar", sep=""),
                paste(admb,"s",escRec[i],".eva", sep=""),
                paste(admb,"s",escRec[i],".cor", sep=""),
                paste(admb,"s",escRec[i],".log", sep=""),
                paste(admb,"s",escRec[i],".tpl", sep=""))
    
    
  }
}
