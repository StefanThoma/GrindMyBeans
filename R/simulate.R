# Load the necessary packages
library(tidyverse)

# Set the seed for reproducibility
set.seed(123)

a <- 5
b <- -10
c <- 8
d <- 2
e <-3
# Create the data frame
data <- data.frame(
  grind_size = runif(100, 0, 5)) %>% 
  mutate(extraction_time = (a + b*grind_size + c*grind_size^2 + d*grind_size^3)*e)
  
plot(extraction_time~grind_size, data = data)
# Fit the model
coffee_model <- lm(extraction_time ~ grind_size, data = data)

# Calculate the extraction time for each grind size
data <- data %>% mutate(extraction_time = predict(coffee_model, newdata = .))

# Filter out values outside of the desired range
data <- data %>% filter(extraction_time > 10 & extraction_time < 50)

# Normalize the extraction time to 28 seconds
data <- data %>% mutate(extraction_time = 28 / mean(extraction_time) * extraction_time)
