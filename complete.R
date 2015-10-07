complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length-1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  base=lapply(list.files(directory,full.name=T),read.csv) #read the csv data
  framed=data.frame()
  nobs=numeric()
  for (i in id) {
    framed = rbind(framed, base[[i]])#convert base to dataframe
  } 
  comp=complete.cases(framed)
  framed=framed[comp,] #reintroduce 
  for (i in id) {
  nobs= c(nobs, nrow(subset(framed,framed$ID==i)))
  }
  nobs=data.frame(id=id,nobs=nobs)
  x
}