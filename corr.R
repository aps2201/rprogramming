corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id=1:332
  base=lapply(list.files(directory,full.name=T),read.csv) #read the csv data
  framed=data.frame()
  nobs=numeric()
  x=numeric()
  for (i in id) {
    framed = rbind(framed, base[[i]])#convert base to dataframe
    comp=complete.cases(framed)
    framed=framed[comp,] #reintroduce
  }
  for (i in id) {
      nobs= c(nobs, nrow(subset(framed,framed$ID==i)))
  }
  nobs=data.frame(id=id,nobs=nobs)
  nobs=nobs[nobs$nobs > threshold,]
  id=nobs$id
  x=c()
for (i in id){
x=c(x,cor(subset(framed[,"sulfate"],framed$ID==i),subset(framed[,"nitrate"],framed$ID==i),use="pairwise.complete.obs"))
      
       }
  x
}