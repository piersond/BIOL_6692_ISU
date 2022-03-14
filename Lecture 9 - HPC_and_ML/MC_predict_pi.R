# Monte Carlo example
# Predict value of pi

iterations = 100000
  
# generate the random set of (x, y) points
x <- runif(n = iterations, min = 0, max = 1)
y <- runif(n = iterations, min = 0, max = 1)
  
# calculate 
sum_sq_xy <- sqrt(x^2 + y^2) 
  
# see how many points are within circle
index_within_circle <- which(sum_sq_xy <= 1)
points_within_circle = length(index_within_circle)
  
# estimate pi
pi_est <- 4 * points_within_circle / iterations
