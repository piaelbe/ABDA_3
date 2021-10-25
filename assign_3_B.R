# Applied Bayesian Data Analysis
# Assignment 3
# Part A
# Pia Elbe

###################################### Task B  ###################################################

# Set up
cat("\014")
graphics.off()
rm(list=ls())
setwd("~/Desktop/bayesian_assignments/assignment_2")

# Finally we will try to take the first steps in applying Bayesian models on data.
# We will start by reproducing the “inferring bias in a coin” example in the book (Ch 5 & 6). As pointed out by the author, this is a toy example but 
# the same model applies to many different situations that you may be faced with (see e.g. the analogies given by the author on page 73 and 124 in 
# the book).

#  1.	The probability of each outcome is given by the Bernoulli distribution Eq. (6.1). x^y*(1.0-x)^(1.0-y)

#  2.	It is a discrete distribution of the outcome  y=0 or  y=1 for a fixed theta. Plot the outcome probabilities given  theta=0.5 and  
#  theta=0.25 using a bar or a stem plot.

# The first part is from the previous assignment, but here I modify the code for a bernoulli distribution.
# Define variables -------------------------------------------------------------
n_flips <- 1000	# Specify the total number of flips
set.seed(555555) # Set to constant random seed
# Generate a bernoulli distribution. flipsequence1 is for theta=0.5. flipsequence2 is for theta=0.25
flipsequence1 = rbern(n_flips, # Number of flips, rbern generates random bernoulli data.
                      prob = 0.5 # change this bit for the 0.25 biased coin and 0.5 fair coin
)

flipsequence2 = rbern(n_flips, # Number of flips, rbern generates random bernoulli data.
                      prob = 0.25 # change this bit for the 0.25 biased coin and 0.5 fair coin
)

flipsequence1 # for theta 0.5
flipsequence2 # for theta 0.25

# Compute the outcomes for each flipsequence -------------------------------------

headsTotal1 = sum(flipsequence1) # Heads are 1.
tailsTotal1 = n_flips - headsTotal1 # This is the number of tails. Tails are 0.

headsTotal2 = sum(flipsequence2) # Heads are 1.
tailsTotal2 = n_flips - headsTotal2 # This is the number of tails. Tails are 0.

# Putting the data into the graph using a data frame.
df <- data.frame(result=rep(c("tails", "heads"), each=2),
                  theta=rep(c("0.25", "0.5")),
                  outcomes=c(tailsTotal2, tailsTotal1, headsTotal2, headsTotal1))
head(df) # this will print the first bit of the data to be used in the graph. note the table is not straightforward.

# Here is the graph. Axis treated as discrete variable.
df$theta<-as.factor(df$theta)
ggplot(data=df, aes(x=theta, y=outcomes, fill=result)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

#  3.	Given a certain outcome, say y=1, we can plot the likelihood of our parameter theta. Note that when evaluating (6.1) with respect to theta for a fixed 
#  outcome y=1, it is called a likelihood function and it is not a probability distribution since it will not integrate to 1. For y=0 and y=1 plot the 
#  likelihood function for 'theta belongs to [0,1]'.

# I have Eq. 6.1 now entered directly for the next part.

# for y = 1
f <- function(x){
  return((x^1)*((1.0-x)^(1.0-1)))
}

curve(expr=f, from = 0, to = 1)

#for y = 0
f <- function(x){
  return((x^0)*((1.0-x)^(1.0-0)))
}

curve(expr=f, from = 0, to = 1)

{}

#  4.	Now we will assume independence, row one in Eq. (6.2), and that the coin flips are independent of each other. It means that knowledge of the outcome 
#  of one flip do not change the outcome probabilities for other flips. It seems like a valid assumption unless the coin is made out of thin aluminium and 
#  we bend it more and more from flip to flip (click here to see a funny example of a bent coin).
#  a.	Implement two functions:
#  i.	the first is just Eq. (6.1) and 

bern <- function (y, theta) {
  (theta^y)*((1-theta)^(1-y))  
}

#  ii.	the other function is just the product of the first function over the data as the first row in Eq. (6.2).
#  See examples in Figure 1 and 2.

likelihood <- function (y, theta) {
  prod(bern(y, theta))
}

#  Create a vector of n coin flips, using your script from Assignment 2. Evaluate the likelihood function for theta = 0.5 for n= 10, 1000, 100000. 
#  What happens for large values of n?  

n_flips <- 10	# Specify the total number of flips
set.seed(11) # Set to constant random seed
# Generate a random sample
flipsequence10 = sample(x=c(0,1), # Sample from coin (heads=1, tails=0)
                      prob=c(.5,.5), # Define a fair coin
                      size=n_flips, # Number of flips
                      replace=TRUE ) # Sample with replacment

n_flips <- 1000	# Specify the total number of flips
set.seed(11) # Set to constant random seed
# Generate a random sample
flipsequence1000 = sample(x=c(0,1), # Sample from coin (heads=1, tails=0)
                      prob=c(.5,.5), # Define a fair coin
                      size=n_flips, # Number of flips
                      replace=TRUE ) # Sample with replacment

n_flips <- 100000	# Specify the total number of flips
set.seed(11) # Set to constant random seed
# Generate a random sample
flipsequence100000 = sample(x=c(0,1), # Sample from coin (heads=1, tails=0)
                          prob=c(.5,.5), # Define a fair coin
                          size=n_flips, # Number of flips
                          replace=TRUE ) # Sample with replacment

likelihood <- function (y, theta) {
  prod(bern(y, theta))
}

likelihood10 <- likelihood(flipsequence10, 0.5)
likelihood1000 <- likelihood(flipsequence1000, 0.5) 
likelihood100000 <- likelihood(flipsequence100000, 0.5)

# The bigger the sample, the closer the likelihood is to zero. This is not useful, since the program doesn't
# show the smallest values with enough detail.

#  b.	A product, as in Eq. (6.2), should be avoided as it may easily lead to overflow or underflow. 
#  Instead, we will often (always) in this course use the log-likelihood C function, i.e. under the logarithm the product can be expressed as a summation, 
#  to avoid these problems. Please read the section on why we work with the log-likelihood here. Implement the logarithm versions of these two functions, 
#  i.e. the log-likelihood and the log-pdf, using the trick of logarithm and summation instead.

logbern <- function (y, theta) {
  
  y*log(theta) + ((1-y)*log(1-theta))
  
}

loglikelihood <- function (y, theta) {
  
  sum(logbern(y, theta))
  
}

#  Can you evaluate the log-likelihood for larger n without problems of under- or overflow?  
# Yes, now even the biggest n gives an answer.

loglikelihood10 <- loglikelihood(flipsequence10, 0.5)
loglikelihood1000 <- loglikelihood(flipsequence1000, 0.5) 
loglikelihood100000 <- loglikelihood(flipsequence100000, 0.5)

#  c.	At the final step, if absolutely necessary, one can exponentiate the log-likelihood function to obtain the likelihood value. However, for this 
#  particular example it will still be problematic as exponentiating large negative values will result in zero. 
#  d.	Plot the likelihood function (use either the likelihood or preferable exp(log-likelihood)) with respect to 'theta belongs to [0,1]' given:
#  i.	  y=[1]
#  ii.  y=[1,1]
#  iii.	y=[1,1,0,1]

# First, I make the possible y combinations as generic vectors.
y <- list(c(1), c(1,1), c(1,1,0,1))

# This is copied from Marc Villa.
theta <- seq(0, 1, length.out = 1000)
l <- seq(0.0, 0.0, length.out=length(theta)) # 'Initializing a vector of the same length as seq of theta values'

for(t in y){
  for (i in 1:length(theta)){
    
    l[i] <- exp(loglikelihood(t,theta[i]))
    
  }
  plot(x=theta, y=l, type="l")
  par(new=TRUE)
}

#  e.	Explain the behaviour you see in terms of the likelihood of theta for the particular data sequences.

#EXPLANATION
# This can be explained using the coin example. 
# If our prior is one heads, there will be a far greater chance of tossing another heads, so the line is diagonal towards 1.0.
# If the prior is two heads, there is even less chance of tails the next time.
# With a prior of two heads, one tails, and another heads, the function is shifted over a little away from 1.0.

#  i.	Assume that the prior distribution is p(theta) = beta(theta,1,1) a beta distribution as in Eq. (6.8). Here we use a=1 and b=1 to obtain a uniform distribution indicating 
#  that we have no particular prior beliefs of theta’s whereabouts. It can be anywhere with equal probability in the interval [0,1].

#  ii.	Use the posterior distribution in (6.8) and make new plots for the data sets given in the bullet (4.d) above. Please implement the log-posterior, 
#  taking advantage of the trick we learned before, and at the very utmost expression take the exponential.

######## THIS NEXT BIT DOESN'T WORK ################
########################################################
y <- list(c(1), c(1,1), c(1,1,0,1)){}

N = length(y)
z = sum(y)
theta = flipsequence1000
a =1
b =1

# y <- list(c(1), c(1,1), c(1,1,0,1))
y = 1
N = length(y)
z = 3
theta = seq(0, 1, length.out = 1000)
a =1
b =1

logposterior <- function(theta, N, a, b){

  ((z+a-1)*log(theta) + (N-z+b-1)*log(1-theta)) / (lbeta(z+a,N-z+b))
  
}

# only works with a data frame.#######################
geom_function(fun = logposterior)

geom_function(fun=logposterior, aes(x=theta, y=logposterior))

plot(x=theta, y=logposterior, type="l")
length(y)


######################################################

# I need to try it with this instead: dbeta(x=theta, shape1, shape2, ncp = 0, log = FALSE)
# Where v, w, and z are different values of z. 

n = 20
v = 1
w = 2
z = 17
theta = seq(0, 1, length.out = 1000) 
# theta = rbinom(1000, 1,.5)
theta
# theta = flipsequence1000 # with this theta, there is a beta prior, so the distribution is flat.
a = 1
b = 1

# This is for z is equal to 1. 
posteriorV <- dbeta (x = theta, shape1 = v+a, shape2 = n-v+b)

plot(x=theta, y=posteriorV, type = "line")

# Here z is W and equal to 2.
posteriorW <- dbeta (x = theta, shape1 = w+a, shape2 = n-w+b)

lines(x=theta, y=posteriorW, type = "line")

# This is the last plot in figure 6.4
posteriorZ <- dbeta (x = theta, shape1 = z+a, shape2 = n-z+b)

lines(x=theta, y=posteriorZ, type = "line")



#  1.	What is the difference between these figures and the figures you did previously using the likelihood?
# This includes the prior to make the posterior, instead of just the likelihood.
# If the prior is a beta distribution, the posterior is a beta distribution. The mean of the posterior is
# a weighted combination of the mean of the prior and the proportion of heads in the data. These figures take
# the prior beta distribution and apply it to the data to get a beta distribution for the posterior.

#  2.	Compare the mathematical expression of the likelihood in (6.1) and (6.2) with expression (6.8) for a=1 and b=1. Can the differences in the expressions 
#  explain the difference seen in the figures?
# I need help to understand the difference. 

#  3.	As in Task A.2.a the estimation process can also be updated sequentially. Due to the property that the prior distribution is of the same form as the 
#  posterior, i.e. a conjugate prior distribution, this sequential updating can here also be seen directly in the expression (6.8) for this example, as noted 
#  in the last paragraph in section 6.3. Read it and understand it!
# Also not sure if I understand it.

#  iii.	Assume now more informative prior information and reproduce Figure 6.4 in the book. Note that the last three figures in column 3 you have already 
#  managed to work out from the above tasks. 
#  1.	To see the relationship between mode and sample size versus a and b in the beta distribution, see Appendix 6.6. If you are in a hurry, skip legends, 
#  labels, HDI, title etc. 
# I have been skipping legends and labels already...oops.

# Using the mode
# a = priorMode * (n - 2) + 1
# b = priorMode * (n - 2) + 1

# Let's make prior mode = 0.5 --that's the left side of Figure 6.4

a = 0.5 * (n - 2) + 1
b = 0.5 * (n - 2) + 1

# Graph mode = 0.5. this is the beta prior. 

n = 20
z = 17
theta = seq(0, 1, length.out = 1000) 

priorU <- dbeta (x = theta, shape1 = z+a, shape2 = n-z+b)

plot(x=theta, y=priorU, type = "line")

# The likelihood for prior with mode = 0.5

likelihoodU <- (theta^z)*((1.0-theta)^(n-z))
plot(theta, likelihoodU, type = "line")

# And finally for mode = 0.5, the posterior distribution.

posteriorU <- dbeta (theta, shape1 = z+a, shape2 = n-z+b)
plot(theta, posteriorU, type = "line")

# Let's make prior mode = 0.75 --that's the center column of Figure 6.4

a = 0.75 * (20 - 2) + 1
b = 0.75 * (20 - 2) + 1

# Graph prior mode = 0.75. this is the beta prior

n = 20
z = 17
theta = seq(0, 1, length.out = 1000) 

posteriorQ <- dbeta (x = theta, shape1 = z+a, shape2 = n-z+b)

plot(x=theta, y=posteriorQ, type = "line")



# Using the mean instead of mode
# a = priorMean * n
# b = (1 - priorMean) *n
# so the shape parameters are really just the mean and sample size?



#  b.	We have here been using Bayesian analysis the hard way. The final task is to read the summary in Section 6.5 carefully. It will recap some of the 
#  challenges we are faced with trying to figure out the denominator in (5.9) (i.e. the normalizing factor) and how we can overcome this using other 
#  techniques that we will soon learn.




