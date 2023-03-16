
library(magrittr)
library(dplyr)

best <- function(state, outcome) {
  
  ## Read outcome data
  data_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Create a unique list of STATE in this dataset.
  # IMPORTANT: There are 54 states, which is wrong.
  # However, I found out this dataset contains DC and other territories.
  states_outcome <- base::unique(data_outcome$State)
  
  # List of outcomes
  list_disease <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  # Valid STATE and Valid OUTCOME
  if ((state %in% states_outcome) & (outcome %in% list_disease)) {
    # Subsetting the raw data.
    data_tidy <- data_outcome %>% select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
    
    # Renaming columns to be readable.
    colnames(data_tidy) <- c("hospital_name", "hospital_state","heart_attack", "heart_failure", "pneumonia")
    
    # Uniforming the names with underscore to match during the select.
    outcome_ <- sub(pattern = " ", replacement = "_", x = outcome)
    
    # Converting character into numeric.
    data_tidy$heart_attack <- as.numeric(data_tidy$heart_attack)
    data_tidy$heart_failure <- as.numeric(data_tidy$heart_failure)
    data_tidy$pneumonia <- as.numeric(data_tidy$pneumonia)
    
    data_tidy %>%
      select(hospital_name,hospital_state,all_of(outcome_)) %>%
      filter(hospital_state == state) %>%
      na.omit() %>% select(-hospital_state) %>%
      arrange(across(all_of(outcome_)), hospital_name) %>%
      head(1) %>%
      select(hospital_name) %>%
      as.character() -> message
  }
  
  # Something is wrong with the inputs
  # The state input in not a valid one
  if (!(state %in% states_outcome) & (outcome %in% list_disease)) {
    return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid state', state, outcome)))
  }
  
  # The outcome in not valid.
  if ((state %in% states_outcome) & !(outcome %in% list_disease)) {
    return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid outcome', state, outcome)))
  }
  
  # Both state and outcome has typos
  if (!(state %in% states_outcome) & !(outcome %in% list_disease)) {
    return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid state and outcome', state, outcome)))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  ## rate
  return(message)
}

source("best.R")