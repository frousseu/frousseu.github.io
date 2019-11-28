library(RCurl)

#g<-getURL("https://docs.google.com/spreadsheets/d/1E6yTZJlZqvwxFESTn0xheB1KS3a7L9PXAFfHc8QXP7w/edit#gid=0") 
#g<-read.csv(text=g,header=TRUE,stringsAsFactors=FALSE)



library(gsheet)
library(xtable)
library(lubridate)
library(htmlTable)


clink<-function(x,col){
  if(col=="Présentateur / Responsable"){
    return(ifelse(!is.na(x[,"Lien_Pres"]),paste0("<a target='_blank' href='",x[,"Lien_Pres"],"'>",x[,col],"</a>"),x[,col]))
  }
  if(col=="Détails"){
    return(ifelse(!is.na(x[,"Lien_Details"]),paste0("<a target='_blank' href='",x[,"Lien_Details"],"'>",x[,col],"</a>"),x[,col]))
  }
}

int2time<-function(x){
  x<-sapply(strsplit(format(x),":"),function(i){
    if(length(i)==1){
      NA
    }else{
      j<-as.numeric(i)
      j[1]*3600+j[2]*60
    }
  })
  h<-x%/%3600
  m<-(x-h*3600)/60
  res<-paste(formatC(h,width=2,flag=0),formatC(m,width=2,flag=0),sep=":")
  res<-substr(res,1,5)
  ifelse(res=="NA:NA","",res)
}

x<-as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1Udz3YHed2MMq7X5eeO8IByIJuyePUa51VKCctZr47IM/edit#gid=0'))
end_time<-as.POSIXct(ifelse(is.na(x$Fin),NA,paste(x$Date,x$Fin)),format="%Y-%m-%d %H:%M:%OS")
keep<-ifelse(is.na(end_time),as.character(x$Date)>=substr(Sys.time(),1,10),end_time>=Sys.time())
x<-x[!is.na(x$Événement) & keep,]
names(x)<-gsub("\\..."," / ",names(x))

day<-weekdays(as.Date(x$Date))
x$Jour<-paste0(toupper(substr(day,1,1)),substr(day,2,nchar(day)))

x$Détails<-clink(x,col="Détails")
x$"Présentateur / Responsable"<-clink(x,col="Présentateur / Responsable")
x$Début<-int2time(x$Début)
x$Fin<-int2time(x$Fin)
x$Lien<-NA
x<-x[order(x$Date,x$Début),]
x<-x[,setdiff(names(x),c("Lien_Pres","Lien_Details","Commentaires"))]
x<-x[,c("Date","Jour","Début","Fin","Événement","Présentateur / Responsable","Local","Détails")]

css.cell1<-matrix("padding-left: .9em; padding-right: .9em;",ncol=ncol(x),nrow=nrow(x))
css.cell2<-matrix("",ncol=ncol(x),nrow=nrow(x))
cw <- strftime(as.POSIXlt(as.character(x$Date)),format="%W")%in%strftime(as.POSIXlt(substr(Sys.time(),1,10)),format="%W") 
css.cell2[cw,]<-""
css.cell<-matrix(paste0(css.cell1,css.cell2),ncol=ncol(x),nrow=nrow(x))


x$Date<-format(as.Date(x$Date),"%d %b")

align<-paste(paste(rep('c',ncol(x)-1),collapse=''),paste(rep("l",1),collapse=""),sep="")  

bab<-function(){
  cat("\014")
  cat("<!DOCTYPE html>\n<html>\n<head>\n<link href='styles.css' rel='stylesheet' type='text/css'>\n</head>\n<body>\n")
  h<-htmlTable(x,rnames=FALSE,css.cell=css.cell,align=align)
  cat(h)
  cat("</body>\n</html>")
}
bab()
