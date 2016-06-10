## sub function, needs to be called to answer part 3

ranker<-function(data,outcome,num="best") {
        data<-as.data.frame(data)
        if (outcome=="heart attack"){
                data[,11]<-as.numeric(data[,11])
                data<-data[!is.na(data[,11]),]
                sort<-data[order(data[,11],data[,2]),]         
        }
        if (outcome=="heart failure"){
                data[,17]<-as.numeric(data[,17])
                data<-data[!is.na(data[,17]),]
                sort<-data[order(data[,17],data[,2]),]         
        }       
        if (outcome=="pneumonia"){
                data[,23]<-as.numeric(data[,23])
                data<-data[!is.na(data[,23]),]
                sort<-data[order(data[,23],data[,2]),]         
        }
        
        if (num=="best"){
                store<-sort[1,2]
        } else
        if (num=="worst"){
                store<-sort[length(sort[,1]),2]
        } else
        if (as.numeric(num)>=1 & as.numeric(num)<=length(sort[,1])) {
                store<-sort[as.numeric(num),2]
        } else
        if (class(num)=="numeric" & num>length(sort[,1])) {
                store<-"NA"
        } 
        store
}
