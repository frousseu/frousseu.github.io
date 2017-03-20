library(RCurl)

#g<-getURL("https://docs.google.com/spreadsheets/d/1E6yTZJlZqvwxFESTn0xheB1KS3a7L9PXAFfHc8QXP7w/edit#gid=0") 
#g<-read.csv(text=g,header=TRUE,stringsAsFactors=FALSE)



library(gsheet)
library(xtable)
library(lubridate)
library(htmlTable)


clink<-function(x){
  ifelse(!is.na(x[,"Lien"]),paste0("<a target='_blank' href='",x[,"Lien"],"'>",x[,"Sujet"],"</a>"),x[,"Sujet"])
}

int2time<-function(x){
  x<-as.integer(x)
  h<-x%/%3600
  m<-(x-h*3600)/60
  res<-paste(formatC(h,width=2,flag=0),formatC(m,width=2,flag=0),sep=":")
  ifelse(is.na(x),NA,res)
}

x<-as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1Udz3YHed2MMq7X5eeO8IByIJuyePUa51VKCctZr47IM/edit#gid=0'))
x<-x[!is.na(x$Événement) & as.character(x$Date)>=substr(Sys.time()-(3600*24*5),1,10),]

day<-weekdays(x$Date)
x$Jour<-paste0(toupper(substr(day,1,1)),substr(day,2,nchar(day)))

x$Sujet<-clink(x)
x$Début<-int2time(x$Début)
x$Fin<-int2time(x$Fin)
x$Lien<-NA
x<-x[order(x$Date,x$Début),]
x<-x[,setdiff(names(x),c("Lien","Commentaires"))]
x<-x[,c("Date","Jour","Début","Fin","Événement","Responsable","Local","Sujet")]

css.cell1<-matrix("padding-left: .9em; padding-right: .9em;",ncol=ncol(x),nrow=nrow(x))
css.cell2<-matrix("",ncol=ncol(x),nrow=nrow(x))
cw <- strftime(as.POSIXlt(as.character(x$Date)),format="%W")%in%strftime(as.POSIXlt(substr(Sys.time(),1,10)),format="%W") 
css.cell2[cw,]<-" font-weight: bolder;"
css.cell<-matrix(paste0(css.cell1,css.cell2),ncol=ncol(x),nrow=nrow(x))


x$Date<-format(x$Date,"%d %b")

align<-paste(paste(rep('c',ncol(x)-2),collapse=''),paste(rep("l",2),collapse=""),sep="")  

htmlTable(x,rnames=FALSE,css.cell=css.cell,align=align)


