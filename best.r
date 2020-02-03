best <- function(state, outcome) {
  ## Read outcome data.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##We need the column names
  df <- as.data.frame(cbind(data[,2],
                            data[,7],
                            data[,11],
                            data[,17],
                            data[,23]),
                      stringsAsFactors = FALSE)
  colnames (df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  #Get the parameters
  requested_outcome <- outcome
  requested_state <- state
  
  ## Check that state and outcome are valid
 valid_states = unique(df$state)
 if(!requested_state %in% valid_states) {
   stop("Invalid State")
 }
 
 valid_outcomes = c("hear attack", "heart failure", "pneumonia")
 if(!requested_outcome %in% valid_outcomes) {
   stop("Invalid Outcome")
 }
  
  ## Return hospital name in that state with lowest 30-day death
 ## Get the state data
 ##state_data <- df$state == requested_state # this returns true/false
 
 state_indexes <- which(df$state== requested_state)
 
 #Get the data based on the indexes
 state_data <- df[state_indexes,]
 
 #Get the data based on the outcome 
 temp_data <- as.numeric(state_data[, eval(requested_outcome)])

 #get the min value, this is for the sorting
 min_value<- min(temp_data, na.rm = TRUE)
 
 ## rate
 result <- state_data[,"hospital"][which(temp_data == min_value)]
 return(result)
}


