## This function is to find the best hospital in the state for specific outcome
## Function takes (1) State and (2) Outcome as input parameters

best <- function(state, outcome) {
    
    ## Read outcome data to df
    df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## convert column 11 : 30.Day.Death..Mortality..Rates.from.Heart.Attack to numeric
    suppressWarnings(df_outcome[, 11] <- as.numeric(df_outcome[, 11]))
    ## convert column 17 : 30.Day.Death..Mortality..Rates.from.Heart.Failure to numeric
    suppressWarnings(df_outcome[, 17] <- as.numeric(df_outcome[, 17]))
    ## convert column 23 : 30.Day.Death..Mortality..Rates.from.Pneumonia to numeric
    suppressWarnings(df_outcome[, 23] <- as.numeric(df_outcome[, 23]))
    
    ## Build the list of state codes 
    ## R base object "state" has the 50 states - but doesnot include Union Territories
    ## Build a character vector "union_ter" with Union Territories and DC
    union_ter <- c( "GU", "PR", "UM", "VI", "DC")
    us_states <- sort(append(state.abb, union_ter ))
    
    ## Check whether state is present in R State dataset - error if not present
    if(!is.element(state, us_states))
        stop("invalid state")
    else
        ## Filter the Outcome data frame for the selected state 
        sel_state <- df_outcome[which(df_outcome$State==state),]
    
    ## Check whether outcome is either "heart attack", "heart failure" or "pneumonia"
    if(!is.element(outcome, c('heart attack', 'heart failure', 'pneumonia')))
        stop("invalid outcome")
    else if(outcome=="heart attack"){
        ## Filter the Outcome data with valid Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        sel_state_outcome <- 
            sel_state[which(!is.na(sel_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        
        ## Sort filtered data based on 1. Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
        ## and then by Hospital.Name
        sort_state_outcome <- sel_state_outcome[order(
                                    sel_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    sel_state_outcome$Hospital.Name),]
    } else if (outcome=="heart failure"){
        ## Filter the Outcome data with valid Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        sel_state_outcome <- 
            sel_state[which(!is.na(sel_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        
        ## Sort filtered data based on 1. Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure 
        ## and then by Hospital.Name
        sort_state_outcome <- 
            sel_state_outcome[order(sel_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                    sel_state_outcome$Hospital.Name),]
        
    } else if (outcome=="pneumonia"){
        ## Filter the Outcome data with valid Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        sel_state_outcome <- 
            sel_state[which(!is.na(sel_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        
        ## Sort filtered data based on 1. Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        ## and then by Hospital.Name
        sort_state_outcome <- 
            sel_state_outcome[order(sel_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                    sel_state_outcome$Hospital.Name),]
    }  
    
    ## Return hospital name in that state with lowest 30-day death
    ## This will be the 1st row 2nd column from the sort_state_outcome
    sort_state_outcome[1,2]
}