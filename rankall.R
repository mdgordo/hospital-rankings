rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        checkout<-c("heart attack", "heart failure", "pneumonia")
        if (outcome %in% checkout=="FALSE"){
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        stateslist<-split(data,data$State)
        states<-names(stateslist)
        hospital<-lapply(stateslist,ranker,outcome,num)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        result<-data.frame(hospital)
        result<-as.data.frame(t(result))
        colnames(result)<-"hospital"
        result$state<-states
        result
}