library(tidyverse)

## FUNCTION STRUCTURE
# function.name <- function(arguments) 
# {
#   computations on the arguments
#   ...other code
# }

# Example function
add_one <- function(number) {
  
  return(number+1)
}

# Try the function
add_one(5)


### Example using a vector
num_vec <- c(1,2,3,4,5)

# Using apply
sapply(num_vec, add_one)
lapply(num_vec, add_one)

# Using tidyverse
num_vec %>% map_dbl(add_one)
num_vec %>% map_chr(add_one)
num_vec %>% map(add_one)


### Example using a dataframe
df <- data.frame(A = c(1,2,3),
                 B = c(10,20,30))


## Run ftn on a specific column, keep dataframe
output_var <- df %>% select(B) %>% add_one()

## Run ftn on a specific column, keep dataframe
output_var2 <- df %>% mutate_at(vars(B), add_one)

## Row by row, apply function, then bind output 
output_var3 <- df %>% split(1:nrow(df)) %>% map(add_one) %>% bind_rows() 


### See MIMICS examples


