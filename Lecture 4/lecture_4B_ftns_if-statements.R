# Biol 6692: Lecture 4, part 2
# Example functions, apply, if statements
# Good reference for apply ftns: https://www.guru99.com/r-apply-sapply-tapply.html

# Basic function examples
####################################
my_ftn <- function(a,b,c) {
  x <- a + b
  y <- x * a
  z <- c - y

  return(list(x,y,z))
}

# Test out the function
my_ftn(a=1, b=2, c=3) # same as: my_ftn(1,2,3)

# Create dummy dataframe
my_df <- data.frame(vals = c(1,2,3,4,5))

my_ftn2 <- function(x) {

  v1 <- x + 5
  v2 <- v1 * 2
    
  return(v2)
}

# Apply function 
my_df$v2 <- sapply(my_df$vals, my_ftn2)

# Take a look at the resulting dataframe
print(my_df) 


###########################
# IF statements
###########################

# ifelse example
a <- 2
cond_val <- ifelse(a == 1,"YES","NO")

# if statement example
a = 2
b = 5
c = 7
result = ""

# | is used for 'OR'
# & is used for 'AND'

# if else chain
if (a == 1 | b < 5) {
  result <- "chain link 1"
} else if (c == 7) {
  result <- "chain link 2"
} else {
  result <- "chain link 3"
}

# Print result
print(result)
