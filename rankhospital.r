rankhospital <- function(state, outcome, rank = "best") {
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
  
  valid_outcomes = c("heart attack", "heart failure", "pneumonia")
  if(!requested_outcome %in% valid_outcomes) {
    stop("Invalid Outcome")
  }
  
  if(is.numeric(rank)) {
    state_indexes <- which(df$state== requested_state)
    
    #Get the data based on the indexes
    state_data <- df[state_indexes,]
    #Get the data based on the outcome 
    state_data[, eval(requested_outcome)] <- as.numeric(state_data[, eval(requested_outcome)])
    state_data <- state_data[order(state_data[, eval(requested_outcome)], state_data[,"hospital"]),]
    result <- state_data[, "hospital"][rank]
  }
  
  ## If something like "best" comes in
  if(!is.numeric(rank)) {
    
    if(rank == "best") {
      result <- best(state,outcome)
    }
    if(rank == "worst") {
      
      state_indexes <- which(df$state== requested_state)
      
      #Get the data based on the indexes
      state_data <- df[state_indexes,]
      #Get the data based on the outcome 
      state_data[, eval(requested_outcome)] <- as.numeric(state_data[, eval(requested_outcome)])
      state_data <- state_data[order(state_data[, eval(requested_outcome)], state_data[,"hospital"], decreasing = TRUE),]
      result <- state_data[, "hospital"][1]
    }
    
  }
  

  return(result)
}


