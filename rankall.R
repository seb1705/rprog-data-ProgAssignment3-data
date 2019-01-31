## Ranking hospitals in all states
## This function takes 2 arguments 
##  1. outcome  : outcome name
##  2. num      : hospital ranking
## Function returns returns a 2-column data frame containing 
##   the hospital in each state that has the ranking specied in num 


rankall <- function(outcome, num = "best") {
  
    ## Read outcome data to df
    df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## convert column 11 : 30.Day.Death..Mortality..Rates.from.Heart.Attack to numeric
    suppressWarnings(df_outcome[, 11] <- as.numeric(df_outcome[, 11]))
    ## convert column 17 : 30.Day.Death..Mortality..Rates.from.Heart.Failure to numeric
    suppressWarnings(df_outcome[, 17] <- as.numeric(df_outcome[, 17]))
    ## convert column 23 : 30.Day.Death..Mortality..Rates.from.Pneumonia to numeric
    suppressWarnings(df_outcome[, 23] <- as.numeric(df_outcome[, 23]))
    
    ## Check whether outcome is either "heart attack", "heart failure" or "pneumonia"
    if(!is.element(outcome, c('heart attack', 'heart failure', 'pneumonia')))
        stop("invalid outcome")
    else if(outcome=="heart attack"){
        ## Filter the Outcome data with valid 
        ##   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        sel_outcome <- 
            df_outcome[which(!is.na(
                df_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        
        ## Sort filtered data based on 
        ##   1. Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
        ##   2. Hospital.Name
        sort_outcome <- 
            sel_outcome[order(sel_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    sel_outcome$Hospital.Name),]
    } else if (outcome=="heart failure"){
        ## Filter the Outcome data with valid Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        sel_outcome <- 
            df_outcome[which(!is.na(df_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        
        ## Sort filtered data based on 
        ## 1. Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure 
        ## and 2. Hospital.Name
        sort_outcome <- 
            sel_outcome[order(sel_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                    sel_outcome$Hospital.Name),]
        
    } else if (outcome=="pneumonia"){
        ## Filter the Outcome data with valid 
        ## Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        sel_outcome <- 
            df_outcome[which(!is.na(df_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        
        ## Sort filtered data based on 
        ## 1. Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        ## and 2. Hospital.Name
        sort_outcome <- 
            sel_outcome[order(sel_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                    sel_outcome$Hospital.Name),]
    }  
    
    ## Create an empty data frame to return the list
    return_list <- data.frame(hospital = character(), state = character())

    
    ## Function rankhospitalbystate called from lapply
    ## Takes 3 arguments 
    ##  1. State
    ##  2. Outcome data frame
    ##  3. Rank 
    rankhospitalbystate <- function(i_state, i_data, i_rank){
        ## Filter df for each state passed into the function
        sel_state <- i_data[which(i_data$State==i_state),]
        
        ## Identify row to return based on rank parameter  
        if (nrow(sel_state) == 0)
            ## if filtered DF is empty, then set row_index to 1st 
            row_index <- 1
        else if (i_rank == "best")
            ## best = 1st row in the sorted data frame
            row_index <- 1
        else if (i_rank == "worst")
            ## worst = last row in the sorted data frame
            row_index <- nrow(sel_state)
        else 
            ## if not best or worst, then it has to be a number
            row_index <- as.integer(i_rank)
        
        ## Build a new data frame with hospital and rank
        new_row <- data.frame(hospital = sel_state[row_index,2], state = i_state)
        
        ## using <<- assigning value to return_list in parent environment
        return_list <<- rbind(return_list, new_row)
        invisible(sel_state[row_index,2])
    }
    ## Build the list of state codes 
    ## R base object "state" has the 50 states - but doesnot include Union Territories
    ## Build a character vector "union_ter" with Union Territories and DC
    union_ter <- c( "GU", "PR", "UM", "VI", "DC")
    us_states <- sort(append(state.abb, union_ter ))
    
    ## For each state, find the hospital of the given rank
    lapply(us_states, rankhospitalbystate, i_data = sort_outcome, i_rank = num)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return_list 
}