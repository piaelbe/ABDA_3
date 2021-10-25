# Applied Bayesian Data Analysis
# Assignment 3
# Part A
# Pia Elbe

# Find the first part of 3A in the document.

###################################### Task A  ###################################################

# Set up
cat("\014")
graphics.off()
rm(list=ls())
setwd("~/Desktop/bayesian_assignments/assignment_2")

# this is task A.2

# 2.	Solve Exercise 5.1 on page 118 using a function that takes an input argument that represents the 
# person’s sequential test results and returns the posterior probability of having the disease and not 
# having the disease.

values <- c(":(", ":)", "+|:(", "+|:)", "-|:(", "-|:)")
p <- c(0.001, 0.999, 0.99, 0.05, 0.01, 0.95)
prob <- setNames(as.list(p), values)
# Now I made the symbols correspond to the probability numbers (prob). 

prob$'+' = prob$`+|:)`*prob$`:)` + prob$`+|:(`*prob$`:)` 
prob$`+` # Reads the probability, just to be sure that these are correct.

prob$'-' = prob$`-|:)`*prob$':)' + prob$`-|:(`*prob$`:(`
prob$`-`

prob$`:(|+` = prob$`+|:(`*prob$`:(` / prob$`+`
prob$`:(|+`

prob$`:(|-` = prob$`-|:(`*prob$`:(` / prob$`-`
prob$`:(|-`

post <- function (T) {
  
  prior <- c(prob$':(', prob$':)') # This is the prior because it is the probability of first not having disease and then having disease.
  likelihood = c(1.0 , 1.0)
  
  # Now to apply it to the model:
  post = as.numeric(likelihood) * prior / (sum(as.numeric(likelihood) * prior))
  
 ######## 'let T=’+-’ be an input argument string to your function' 
  # representing a random individual who first tested positive and then negative.
  # We should be able to call T later (not as a vector). Then T should take the prior and then give the posterior.
  T <- paste(T, collapse = " ")
  print(T) #just checking that it works.
  
  # calculating the posterior distribution using bayes rule.
  
  for (i in T) {
    
    likelihood <- c( prob[paste(i,'|:(',sep = "")], prob[paste(i,'|:)',sep = "")])
    
    post <- as.numeric(likelihood)*prior/(sum(as.numeric(likelihood)*prior))
    
    prior = post
    
  }
  
  #Now to make the output the two element array of being sick and testing positive and not being sick and testing positive.
  names(post) <- c(':(|T', ':)|T') 
  print(post)
  
}

post("+++---")

# Use the last paragraph in section 5.1.2 (on page 104-105) and section 5.2.1 (page 107-108) to explain how the algorithm works sequentially, starting 
# from the prior probabilities (i.e. without any test results) and then moving on sequentially with access to new test results. If the function is given 
# an empty string T = ‘’ it returns the prior probabilities and then sequentially updates the posterior.

# EXPLANATION
# The prior is whether the disease is present or absent. In table 5.4, this is indicated by the marginal probability. 
# The conditional probabilities take the hit rate and false alarm rate into account.
# With an observation (a test of the disease), we can focus attention on only that case and apply Bayes' rule. - this is the sequential part.
# Conditional probabiliities = posterior probabilities.
