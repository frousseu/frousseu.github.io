library(RCurl)

#g<-getURL("https://docs.google.com/spreadsheets/d/1E6yTZJlZqvwxFESTn0xheB1KS3a7L9PXAFfHc8QXP7w/edit#gid=0") 
#g<-read.csv(text=g,header=TRUE,stringsAsFactors=FALSE)



library(gsheet)
library(xtable)
library(lubridate)
library(htmlTable)


clink<-function(x,col){
  ifelse(!is.na(x[,"Lien"]),paste0("<a target='_blank' href='",x[,"Lien"],"'>",x[,col],"</a>"),x[,col])
}

int2time<-function(x){
  #x<-as.integer(x)
  #x<-ifelse(is.na(x),"",x)
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
x<-x[!is.na(x$Événement) & as.character(x$Date)>=substr(Sys.time()-(3600*24*5),1,10),]


day<-weekdays(as.Date(x$Date))
x$Jour<-paste0(toupper(substr(day,1,1)),substr(day,2,nchar(day)))

x$Détails<-ifelse(x$Événement=="Séminaire invité",x$Détails,clink(x,col="Détails"))
x$Responsable<-ifelse(x$Événement=="Séminaire invité",clink(x,col="Responsable"),x$Responsable)
x$Début<-int2time(x$Début)
x$Fin<-int2time(x$Fin)
x$Lien<-NA
x<-x[order(x$Date,x$Début),]
x<-x[,setdiff(names(x),c("Lien","Commentaires"))]
x<-x[,c("Date","Jour","Début","Fin","Événement","Responsable","Local","Détails")]

css.cell1<-matrix("padding-left: .9em; padding-right: .9em;",ncol=ncol(x),nrow=nrow(x))
css.cell2<-matrix("",ncol=ncol(x),nrow=nrow(x))
cw <- strftime(as.POSIXlt(as.character(x$Date)),format="%W")%in%strftime(as.POSIXlt(substr(Sys.time(),1,10)),format="%W") 
css.cell2[cw,]<-" font-weight: bolder;"
css.cell<-matrix(paste0(css.cell1,css.cell2),ncol=ncol(x),nrow=nrow(x))


x$Date<-format(as.Date(x$Date),"%d %b")

align<-paste(paste(rep('c',ncol(x)-2),collapse=''),paste(rep("l",2),collapse=""),sep="")  

bab<-function(){
  cat("\014")
  cat("<!DOCTYPE html>\n<html>\n<head>\n<link href='styles.css' rel='stylesheet' type='text/css'>\n</head>\n<body>\n")
  h<-htmlTable(x,rnames=FALSE,css.cell=css.cell,align=align)
  cat(h)
  cat("</body>\n</html>")
}
bab()
