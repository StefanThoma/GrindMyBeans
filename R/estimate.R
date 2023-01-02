

#' fit model for grind size estimation
#'
#' @param extraction_time 
#' @param ground_fineness 
#'
#' @return
#' @export
#'
#' @examples
fit_model <- function(data, extraction_time, ground_fineness) {
  # Load the necessary packages
  library(tidyverse)
  
  # Load the data
  data <- read_csv("grinding_data.csv")
  
  # Fit the model
  coffee_model <- lm(extraction_time ~ ground_fineness, data = data)
  
  # Predict the extraction time for different levels of ground fineness
  predictions <- data.frame(ground_fineness = seq(0, 20, by = 0.1)) %>%
    mutate(extraction_time = predict(coffee_model, newdata = .))
  
  # Find the ground fineness that corresponds to an extraction time of 28 seconds
  target_extraction_time <- 28
  target_ground_fineness <- predictions %>% filter(extraction_time == target_extraction_time) %>% pull(ground_fineness)
  
  # Calculate the change in ground fineness required to get 28 seconds extraction time
  change_in_ground_fineness <- target_ground_fineness - data %>% pull(ground_fineness)
  
  # Print the result
  print(change_in_ground_fineness)
}



#' Title
#'
#' @param coffee_model 
#' @param . 
#' @param extraction_time 
#' @param ground_fineness 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
guess_grind <- function(data, coffee_model, extraction_time, ground_fineness) {
  
  
  
  # Predict the extraction time for different levels of ground fineness
  predictions <- data.frame(ground_fineness = seq(0, 20, by = 0.1)) %>%
    mutate(extraction_time = predict(coffee_model, newdata = .))
  
  # Find the ground fineness that corresponds to an extraction time of 28 seconds
  target_extraction_time <- 28
  target_ground_fineness <- predictions %>% filter(extraction_time == target_extraction_time) %>% pull(ground_fineness)
  
  # Calculate the change in ground fineness required to get 28 seconds extraction time
  change_in_ground_fineness <- target_ground_fineness - data %>% pull(ground_fineness)
  
  # Print the result
  print(change_in_ground_fineness)

}