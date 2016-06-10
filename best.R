best <- function(state, outcome) {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        checkst<-as.data.frame(table(data$State))
        allstates<-checkst[,1]
        if (state %in% allstates == "FALSE"){
                stop("invalid state")
        }
        checkout<-c("heart attack", "heart failure", "pneumonia")
        if (outcome %in% checkout=="FALSE"){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ST<-data[data$State==state,]
        if (outcome=="heart attack"){
                ST[,11]<-as.numeric(ST[,11])
                ST<-ST[!is.na(ST[,11]),]
                sort<-ST[order(ST[,11],ST[,2]),]         
        }
        if (outcome=="heart failure"){
                ST[,17]<-as.numeric(ST[,17])
                ST<-ST[!is.na(ST[,17]),]
                sort<-ST[order(ST[,17],ST[,2]),]         
        }       
        if (outcome=="pneumonia"){
                ST[,23]<-as.numeric(ST[,23])
                ST<-ST[!is.na(ST[,23]),]
                sort<-ST[order(ST[,23],ST[,2]),]         
        }
        sort[1,2]
}
