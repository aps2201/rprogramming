pollutantmean=function(directory, pollutant, id = 1:332) {
  base=lapply(list.files(directory,full.name=T),read.csv) #read the csv data
  framed=data.frame()
  for (i in id) {
    framed = rbind(framed, base[[i]]) #convert base to dataframe
  } 
  framedsub=subset(framed,framed$ID%in%id) #filter by ID
  mean(framedsub[,pollutant],na.rm=T) #the point of all of this
  #View(framedsub[,pollutant]) #this is a test line

}