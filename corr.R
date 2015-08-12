##Write a function that takes a directory of data files 
##and a threshold for complete cases 
##and calculates the correlation between sulfate and nitrate 
##for monitor locations where the number of completely observed cases 
##(on all variables) is greater than the threshold. 
##For this function you will need to use the 'cor' function in R 
##which calculates the correlation between two vectors. 
##Please read the help page for this function via '?cor' 
##and make sure that you know how to use it.
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        ##output exemple
        ##source("corr.R")
        ##source("complete.R")
        ##cr <- corr("specdata", 150)
        ##head(cr)
        ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589

        #locations vector acomplishing requirements of threshold
        vnLocations<-subset(complete(directory),
                            select=c(id,nobs),
                            nobs>=threshold
                            )[[1]]
        #correlation between sulfate and nitrate for each location
        vCorrs<-vector()
        for(loc in vnLocations) {
                dfMeasures<-read.csv(paste0(directory,"/",
                                            substring(loc+1000,2,4),".csv"))
                vCorrs<-append(vCorrs,
                               cor(dfMeasures[["sulfate"]],
                                   dfMeasures[["nitrate"]],
                                   use="pairwise.complete.obs")
                )
        }
        vCorrs
}
