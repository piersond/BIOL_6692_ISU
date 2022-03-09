# Random forest example
# Adapted from https://kevintshoemaker.github.io/NRES-746/RandomForests.html

### Let's predict mpg from engine displacment using the mtcars data

### Create a single decision tree
library(rpart)
library(rpart.plot)
tree <- rpart(formula = mpg~disp, data = mtcars, 
              control = rpart.control(maxdepth = 3, # max tree layers
                                      minbucket = 1, # min n in a bucket 
                                      cp = 0)) # complexity cost
# Plot the tree
prp(tree, faclen = 0, cex = 0.8, extra = 1)


## Now, what if we think weight is an important predictor too? 
# Make a multi-variable tree
tree <- rpart(formula = mpg~disp+wt, data = mtcars, 
              control = rpart.control(maxdepth = 5, 
                                      minbucket = 1, 
                                      cp = 0))
# Plot the tree
prp(tree, faclen = 0, cex = 0.8, extra = 1)

# Is the tree actually any good at prediction?
# How to prevent over-fitting, use the cross-validated error summary
printcp(tree)
plotcp(tree) # Lower n is better. Only use +n is error is reduced

# Automatic tree pruning
ptree<- prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

# Plot best tree
prp(ptree, faclen = 0, cex = 0.8, extra = 1)


### To continue on to a full Random Forest example, see website:
# https://kevintshoemaker.github.io/NRES-746/RandomForests.html





