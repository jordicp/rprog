# 2 Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).
# The function should use the following template.
best <- function(state, outcome) {
        ## Read outcome data
        dfOutcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        ## Check that state and outcome are valid
        #dfOutcome[, 7] <- as.factor(dfOutcome[, 7]) #7: State
        if (length(dfOutcome[,7][state==dfOutcome[,7]])<=0) stop("invalid state")
        if ( outcome!="heart attack"  
             &outcome!="heart failure" 
             &outcome!="pneumonia"     ) stop("invalid outcome")
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (outcome=="heart attack") {
                sColName<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome=="heart failure") {
                sColName<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome=="pneumonia"     ) {
                sColName<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        dfState <- subset( dfOutcome,
                           (State==state), 
                           select=c("Hospital.Name",sColName)
        ) ##dfOutcome contains now only hospital name and outcome values
        colnames(dfState)[1]="Name"
        colnames(dfState)[2]="Outcome"
        
        vNAs<-dfState$Outcome=="Not Available"
        dfState<-data.frame(dfState$Name[!vNAs],dfState$Outcome[!vNAs]) #without NAs
        colnames(dfState)[1]="Name"
        colnames(dfState)[2]="Outcome"
        
        dfState[,2]<-as.numeric(as.character(dfState[,2])) #coercing outcome to numeric
        bestRate<-min(dfState[,2],na.rm=TRUE)
        
        o<-order(dfState[,2],dfState[,1]); 
        bestHospitals<-dfState[o,] #ordered by outcome and name
        as.character(bestHospitals[1,1])
}
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.
# Here is some sample output from the function:
# source("best.R")
#best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

# Save your code for this function to a file named best.R.
# Use the submit script provided to submit your solution to this part. 
# There are 3 tests that need to be passed for this part of the assignment.
