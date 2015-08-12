##Write a function that reads a directory full of files 
##and reports the number of completely observed cases in each data file. 
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
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

  ## output exemple
  ##source("complete.R")
  ##complete("specdata", 1)
  ##   id nobs
  ## 1  1  117

        ##location files vector
        vsLocationFiles<-paste0(directory,"/",substring(id+1000,2,4),".csv")
        dfObsPerId<-data.frame()
        for(sLocFile in vsLocationFiles) {
                dfID<-read.csv(sLocFile)
                vlComplets<-complete.cases(dfID)
                dfObsPerId<-rbind(dfObsPerId,
                                  data.frame(id=dfID[1,"ID"],
                                             nobs=length(vlComplets[vlComplets]))
                                  )        
        }
        dfObsPerId
}