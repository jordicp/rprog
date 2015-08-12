##Write a function named 'pollutantmean' 
##that calculates the mean of a pollutant (sulfate or nitrate) 
##across a specified list of monitors. 
pollutantmean <- function(directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ## example output
  ## > pollutantmean("specdata", "sulfate", 1:10)
  ## [1] 4.064
  
        ##location files vector
        vsLocationFiles<-paste0(directory,"/",substring(id+1000,2,4),".csv")
        ##pollutant values vector
        vnPollutantValues<-c(); ##initialize pollutant values vector
        for(sLocFile in vsLocationFiles) {
                vnPollutantValues<-append(vnPollutantValues,
                                          read.csv(sLocFile)[[pollutant]])
        }
        vnPollutantValues<-vnPollutantValues[complete.cases(vnPollutantValues)]
        mean(vnPollutantValues)
}