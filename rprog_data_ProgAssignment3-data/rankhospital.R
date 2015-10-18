rankhospital <- function(state, outcome, num = "best") {
       ## Read outcome data
       ocm=read.csv("outcome-of-care-measures.csv")
       hosdat=read.csv("hospital-data.csv")
       colnames(ocm)=c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County Name","Phone Number","Hospital 30-Day Death (Mortality) Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Hospital 30-Day Death (Mortality) Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Hospital 30-Day Death (Mortality) Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Hospital 30-Day Readmission Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack","Footnote - Hospital 30-Day Readmission Rates from Heart Attack","Hospital 30-Day Readmission Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure","Footnote - Hospital 30-Day Readmission Rates from Heart Failure","Hospital 30-Day Readmission Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia","Footnote - Hospital 30-Day Readmission Rates from Pneumonia")
       colnames(hosdat)=c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County","Phone Number","Hospital Type","Hospital Ownership","Emergency Services")
       ## Check that state and outcome are valid
       outcomes = c("heart attack", "heart failure", "pneumonia")
       if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
       states=ocm[,7]
       states=unique((states))
       if( state %in% states == FALSE ) stop("invalid state")
       ## Return hospital name in that state with the given rank
       ## 30-day death rate
       deathrate=grep("^Hospital 30-Day",colnames(ocm))
       deathrate=ocm[,c(1:10,deathrate)]
       deathrate=deathrate[,1:13]
       colnames(deathrate)=c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County","Phone Number","heart attack","heart failure","pneumonia")
       for (i in 11:13){
              deathrate[,i]=as.numeric(as.character(deathrate[,i]))
       }
       x=subset(deathrate,deathrate$State==state)
       x=x[order(x[outcome]),]
       x=cbind(x[,c(1,2,7)],x[outcome])
       x=na.omit(x)
       x=cbind(x,1:nrow(x))       
       if (is.numeric(num)==TRUE){
              as.character(x$'Hospital Name'[num])
       }
       else if (num=="best"){
              as.character(x$'Hospital Name'[1])
       }
       else if (num=="worst"){
              as.character(x$'Hospital Name'[nrow(x)])
       }
}