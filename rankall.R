library(magrittr)
library(dplyr)

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data_outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  # Adding the row number
  data_outcome <- data_outcome %>% tibble::rownames_to_column()
  
  # Create a unique list of STATE
  states_outcome <- base::unique(data_outcome$State)
  
  # List of outcomes
  list_disease <- c("heart attack", "heart failure", "pneumonia")
  
  # Subsetting the raw data.
  # I want the death data of the last 30 Day all and other columns I will ignore.
  data_tidy <- data_outcome %>%
    select(rowname, Hospital.Name,
           State,
           starts_with("Hospital.30.Day.Death"))
  
  # Renaming columns to be more readable.
  colnames(data_tidy) <- c("rowname",
                           "hospital_name",
                           "hospital_state",
                           "heart_attack",
                           "heart_failure",
                           "pneumonia")
  
  # Uniforming the colnames with underscore to match during the select.
  outcome_ <- sub(pattern = " ",
                  replacement = "_",
                  x = outcome)
  
  # Converting character into numeric.
  # The read.csv has forced all columns to be character (colClasses = "character")
  data_tidy$heart_attack <- as.numeric(data_tidy$heart_attack)
  data_tidy$heart_failure <- as.numeric(data_tidy$heart_failure)
  data_tidy$pneumonia <- as.numeric(data_tidy$pneumonia)
  
  # Checking if the operator has inserted a valid outcome.
  if (outcome %in% list_disease) {
    
    # Creating a group and ranking by state. 
    data_rank_state <- data_tidy %>%
      group_by(hospital_state) %>%
      mutate(ranking = order(order(across(all_of(outcome_)), hospital_name))) 
    
    # The num input has two conditions:
    # 1. numeric
    # 2. character
    
    # Checking if the num input is character:
    if (is.character(num)) {
      
      # However, there are 2 conditions in this case of character:
      # 1. best -> I need to show the best hospital
      # 2. worst -> I need to show the worst hospital
      
      # IN CASE: the num variable is the character's worst.
      if (num == "worst") {
        
        # MAX will give the worst hospital in that outcome.
        data_rank_state %>%
          filter(!is.na(across(all_of(outcome_)))) %>%
          filter(ranking == max(ranking, na.rm = TRUE)) %>%
          select(hospital_name,hospital_state) %>%
          arrange(hospital_state, all_of(hospital_name)) -> output
        
        # Return the hospital with the worst outcome.
        return(output)
      }
      
      # IN CASE: the num variable is the character's best.
      else if (num == "best") {
        
        # MIN will select the best hospital in that outcome.
        data_rank_state %>%
          filter(ranking == min(ranking, na.rm = TRUE)) %>%
          select(hospital_name,hospital_state) %>%
          arrange(hospital_state, all_of(hospital_name)) -> output
        
        # Return the hospital with the best outcome.
        return(output)
      }
      
      # The script only accepts characters: best and worst.
      # Any other characters will throw an error.
      else {
        return(cat("Please, check the num input."))
      }
    }
    
    # Checking if the num input is a numeric variable and the num input and if it is a single number.
    # should be a single value (not a vector).
    if (is.numeric(num) & length(num) == 1) {
      
      # We have also two conditions here:
      # 1. If the num input is out of the boundaries
      # 2. A single number between 1 and the number of hospital.
      
      # IN CASE the num input is between 1 the number of rows of the dataset.
      if (num >= 1 & num < nrow(data_rank_state)) {
        
        # Filter will select the exactly num rank.
        data_rank_state %>%
          filter(ranking == num) %>%
          select(hospital_name,hospital_state) %>%
          arrange(hospital_state, all_of(hospital_name)) -> output
        
        # Creating the rows of state with NA.
        merge(x = data_tidy %>% select(hospital_state) %>% unique() %>% arrange(hospital_state),
              y = output,
              all = TRUE) -> output
        
        # Return the hospital in the "num position" in that outcome.
        return(output)
      }
      
      # The num input is wrong because is negative, zero or above the number of hospital.
      else {
        # Following the instructions. For num above of the number of hospital return NA.
        return(NA)
      }
    }
    
    # Warning the operator about a problem in num input.
    else if (length(num) >= 2) {
      return(print("Please, make sure your num input is a single value."))
    }
  }
  
  # Something is wrong with the inputs
  
  # The outcome in not valid.
  else if (!(outcome %in% list_disease)) {
    return(base::cat(base::sprintf('Error in rankhospital(outcome = "%s", num = "%s") : invalid outcome', State, num)))
  }
}
source("rankall.R")
