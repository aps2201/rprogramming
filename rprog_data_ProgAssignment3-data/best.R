best=function(state, outcome) {
       ## Read outcome data
       ocm=read.csv("outcome-of-care-measures.csv")
       hosdat=read.csv("hospital-data.csv")
       
       ## Check that state and outcome are valid
       validnum=hosdat$Provider.Number%in%ocm$Provider.Number
       validname=hosdat$Hospital.Name%in%ocm$Hospital.Name
       
       ## Return hospital name in that state with lowest 30-day death
       ## rate

}