
# Monte Carlo example
# Predict value of pi

iterations = 100000

# generate the random set of (x, y) points
randxy <- data.frame(x = runif(n = iterations, min = -1, max = 1),
                     y = runif(n = iterations, min = -1, max = 1))

# calculate sum coord square
randxy$sum_sq_xy <- sqrt(randxy$x^2 + randxy$y^2) 

# see how many points are within circle
randxy$in_circle <- ifelse(abs(randxy$sum_sq_xy) <= 1, "red", "blue")
randxy$in_circle_bin <- ifelse(abs(randxy$sum_sq_xy) <= 1, 1, 0)

# Plot
for(i in 1:iterations/10) {
  flush.console()
  row = i*100
  title_pi = paste0("pi estimate = ", as.character(round(4*(length(which(randxy$in_circle_bin[1:row] == 1))/row),2)))
  plot(randxy$x[1:row], randxy$y[1:row], col=randxy$in_circle[1:row], 
       ylim=c(-1,1), xlim=c(-1,1), 
       pch=16,
       main=title_pi)
  Sys.sleep(.05)
}
