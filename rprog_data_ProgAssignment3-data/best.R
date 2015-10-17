best=function(state, outcome) {
       ## Read outcome data
       ocm=read.csv("outcome-of-care-measures.csv")
       hosdat=read.csv("hospital-data.csv")
       colnames(ocm)=c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County Name","Phone Number","Hospital 30-Day Death (Mortality) Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Hospital 30-Day Death (Mortality) Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Hospital 30-Day Death (Mortality) Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Hospital 30-Day Readmission Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack","Footnote - Hospital 30-Day Readmission Rates from Heart Attack","Hospital 30-Day Readmission Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure","Footnote - Hospital 30-Day Readmission Rates from Heart Failure","Hospital 30-Day Readmission Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia","Footnote - Hospital 30-Day Readmission Rates from Pneumonia")
       colnames(hosdat)=c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County","Phone Number","Hospital Type","Hospital Ownership","Emergency Services")
       ## Check that state and outcome are valid
       validnum=hosdat$Provider.Number%in%ocm$Provider.Number
       validname=hosdat$Hospital.Name%in%ocm$Hospital.Name
       
       ## Return hospital name in that state with lowest 30-day death
       ## rate
       outcome=grep("^Hospital 30-Day",colnames(ocm))
       outcome=ocm[,c(1:10,outcome)]
       outcome=outcome[,1:13]
       for (i in 11:13){
       outcome[,i]=as.numeric(as.character(outcome[,i]))
       }

}