library(rms)
#Auto Binary Data
Data.binary<-function(data,form){
  
  #division form 
  division.form<-function(form){
    team<-terms(form)
    chrlist<-attr(team,"term.labels")
    chrlist
  }
  #calculate the AUC of data
  cal.auc<-function(mydata){
    data<-mydata[order(mydata$P,decreasing=F),]
    len.1<-length(which(data$Y==1))
    
    len.0<-length(data$Y)-len.1
    
    xstep<-1.0/len.0
    ystep<-1.0/len.1
    max<--10.0
    max.index<-0
    x<-1.0
    y<-1.0
    label<-data$Y
    xvalue<-data$X
    temp<-0
    for(i in 1:length(data$Y)){
      if(label[i]!=1){
        x<-x-xstep
        y<-y
        if((y-x)>0)
          temp<-y-x
        else
          temp<-x-y
        len<-temp
        if(len>max){
          max.index<-i
          max<-len
          
        }
      }
      else{
        x<-x
        y<-y-ystep
        if((y-x)>0)
          temp<-y-x
        else
          temp<-x-y
        len<-temp
        if(len>max){
          max.index=i
          max=len
        }
      }
    }
    return (xvalue[max.index])
  }
  
  #Binary feature data according roc
  cal.binary<-function(mydata,thre,xname){
    len<-nrow(mydata)
    for(i in 1:len){
      if(mydata[i,xname]>thre){
        if(xname=='X10'||xname=='X13'){
          mydata[i,xname]<-0
        }
        else{
          mydata[i,xname]<-1
        }
        
      }
      
      else{
        if(xname=='X10'||xname=='X13'){
          mydata[i,xname]<-1
        }
        else{
          mydata[i,xname]<-0
        }
      }
    }
    mydata
  }
  #Binary  all feature data according roc
  cal.binaryAll<-function(mydata,form){
    list.x<-division.form(form)
    len<-length(list.x)
    for(i in 1:len){
      x<-list.x[i]
      
      if(length(unique(mydata[,x]))!=2){
        data1<-mydata[,c(x,'Y')]
        names(data1)[1]<-"X"
        fit<-lrm(Y~X,x=TRUE, y=TRUE, data = data1)
        pred<-predict(fit,data1)
        data1$P=pred
        thre<-cal.auc(data1)
        mydata<-cal.binary(mydata,thre,x)
      }
    }
    mydata
  }
  cal.binaryAll(data,form)
}

#a formula object
form=Y~X1+X2+X3+X4+X7+X10+X11+X12+X13+X14+X15
#read the data
mydata<-read.table(file='originaldata.csv',header=TRUE,sep=",")
binary.data<-Data.binary(mydata,form)
