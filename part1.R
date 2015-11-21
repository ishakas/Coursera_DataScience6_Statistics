set.seed(123456)

lambda = .2
n = 40
number_of_simulations = 10000

# question 1
# get the means for 40 random variables having expontential distributions. collect 1000 samples
# with lambda = .2
means = NULL
for (i in 1:number_of_simulations) means = c(means, mean(rexp(n,lambda)))

# histogram
hist(means)

# calculate sample means
mean(means)

# theoretical means of exponential distribution is 1/lambda
theo_mean = 1/lambda

# question 3
# show variance of average
sample_var <- var(means)
sample_var

# theoretical variance
# the variance of exponential distribution is 1/lambda2
exp_var <- 1/(lambda)^2

# the formula for finding sample variance for the means, n=40
theo_var <-  exp_var/n
theo_var

# question 3
# drawing the distribution for expontential distribution with lambda of 0.2
curve(dexp(x,lambda), 0,n)


# draw the distribution of means 
hist(means , freq = FALSE, density=10, col="darkgray", breaks=90)
lines(density(means), col="blue")
curve(dnorm(x,mean=mean(means),sd=sqrt(var(means))),min(means-1),max(means), add=TRUE, col="red", lwd=2)

# draw the exponential distribution of 1000 simulations having lambda of .2
random_var_exp_distributed = rexp(number_of_simulations, lambda)
hist(random_var_exp_distributed, freq = FALSE)

# draw the theoretical density
curve(dexp(x,.2), min(random_var_exp_distributed),max(random_var_exp_distributed), add=TRUE, col="red")


### other stuff
coll = NULL
for (i in 1:1000) coll = c(coll, mean(rexp(40,.2)))
hist(coll)
abline(v = mean(coll), lwd=2, col="red")

var(rexp(40,.2))
###
