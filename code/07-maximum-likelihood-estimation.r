# ************************************************
### simon munzert
### maximum likelihood estimation
# ************************************************

source("packages.r")
source("functions.r")


### Writing functions in R -----------------------

# R is a functional programming lanugage, i.e. it provides many tools for the creation and manipulation of functions
# you can do virtually anything with functions: assign them to variables, store them in lists, pass them as arguments to other functions, ...
# very helpful in obeying the "don't repeat yourself" a.k.a. DRY principle

f <- function(x, y) x^2 + 3*y
f
f(3, 2)

# function that returns the mean of a vector
my_mean <- function(my_vector) {
  mean <- sum(my_vector)/length(my_vector) 
  mean
}
my_mean(c(1, 2, 3))
my_mean

my_mean <- function(my_vector) sum(my_vector)/length(my_vector) 
my_mean(c(1, 2, 3))

# another function that finds the remainder after division ("modulo operation")
remainder <- function(num = 10, divisor = 4) {
  remain <- num %% divisor
  remain
}
remainder()
args(remainder)

## let's study the "functions.r" file now!


### Exercise: Functions --------------------------
# 1. program a function ultimateAnswer() that always returns the number 42!
# 2. program a function normalize() that normalizes a numeric vector x to mean(x) = 0 and sd(x) = 1!



### Probability distributions in R -----------

# for an overview of implemented distributions, see
?Distributions

## the Normal distribution

# rnorm() performs random draws from the distribution
y <- rnorm(10000, mean = 0, sd = 1) 
plot(y)
plot(density(y))

# dnorm() gives the density of the distribution
dnorm(0, mean = 0, sd = 1) 
z_scores <- seq(-3, 3, by = .1)

dvalues <- dnorm(z_scores)
plot(z_scores, dvalues, type = "l", main = "pdf of the Standard Normal", xlab = "Z score") 

# pnorm() gives the distribution function
pnorm(0)
pnorm(-1)
pnorm(1.96)
pnorm(1.96, lower.tail = FALSE)

pvalues <- pnorm(z_scores)
plot(z_scores, pvalues,  type = "l", main = "cdf of the Standard Normal", xlab= "Z score", ylab="Probability Density") 

## the Binomial distribution
p <- 0.5
n <- 100
size <- 5
tmp <- rbinom(n, size, p) # number of observations, number of trials, probability
table(tmp)
plot(table(tmp), xlab = "Sum of 1 in N", ylab = "Occurrence in n = 100")
text(4.5, c(27, 24, 21), c(paste0("n = ", n), paste0("p = ", p), paste0("N = ", size)))



## Example 1: Coin flips ----------------------

# get 100 random draws from a binomial distribution with p = .4 --> the estimate we're actually interested in
set.seed(1234)
flips <- rbinom(n = 100, size = 1, prob = .4)
table(flips)

# define log-likelihood function
binom_loglik <- function(x, p) { # x: vector of results/observed 0s and 1s, p: probability of success 
  llik <- sum(dbinom(x, size = 1, prob = p, log = TRUE)) # calculate log likelihood with pdf for binomial distribution
  return(-llik) # return the negative log likelihood 
}

# visually identify parameter value for which negative likelihood is minimized (= likelihood is maximized)
llks <- vector()
probs <- seq(0, 1, .01)
for (i in seq_along(probs)) {
  llks[i] <- - sum(dbinom(flips, size = 1, prob = probs[i], log = TRUE))
}
plot(probs, -llks, type = "l")
abline(v = probs[which.min(llks)], col = "red", lty = 2)

# numerically optimize log-likelihood
result <- optim(par = .5, fn = binom_loglik, x = flips, method = 'Brent', lower = 0, upper = 1)
result
# par: initial values of parameter to be estimated, we assume a fair coin
# fn: function to be minimized
# lower/upper: since probability paramenter can only be within the unit interval



## Example 2: Linear model ---------------------

# prepare data, run OLS
data("wage1")
wage_ols <- lm(wage ~ educ, data = wage1)
summary(wage_ols)
y <- wage1$wage
X <- cbind(1, wage1$educ)

# define likelihood function...
# ... but how? Here are three options:
  # 1. minimizing the sum of squared residuals (not an actual ML estimate)
  # 2. maximizing the log-likelihood for normally distributed residuals
  # 3. maximizing the log-likelihood for normally distributed DVs

# 1. Squared residuals
ols <- function(y, X, b) { 
  res <- y - X %*% b # calculate residuals
  return(sum(res^2, na.rm = TRUE))
}
# this function calculates the sum of squared residuals
# OLS is defined to return coefficients that give the smallest possible
# sum of squared residuals

# 2. normally distributed residuals
normal_loglik <- function(theta, y, X) {
  b <- theta[-length(theta)] # b's
  sigma <- theta[length(theta)] # sigma
  res <- y - X %*% b # residuals
  return(-sum(dnorm(res, mean = 0, sd = sigma, log = TRUE), na.rm = TRUE))
}
# implies the following assumption on the distribution of e:
# y = f(X, b) + e with e ~ N(0, sigma)

# 3. normally distributed y
normal2_loglik <- function(theta, y, X) {
  b <- theta[-length(theta)] # b's
  sigma <- theta[length(theta)] # sigma
  yhat <- X %*% b # predicted values for y 
  return(-sum(dnorm(y, mean = yhat, sd = sigma, log = TRUE), na.rm = TRUE))
}
# an equivalent formulation is
# y ~ f(theta, sigma) with theta = g(X, b)


# OLS solution
result_ols <- optim(par = c(1, 1), fn = ols, y = y, X = X, method = "BFGS")
result_ols

# optimize log-likelihoods
result_normal <- optim(par = c(1, 1, 1), fn = normal_loglik, y = y, X = X, method = "BFGS")
result_normal

result_normal2 <- optim(par = c(1, 1, 1), fn = normal2_loglik, y = y, X = X, method = "BFGS")
result_normal2

# compare again with OLS
summary(wage_ols)



## Assessing estimation uncertainty ---------------------

result <- optim(par = c(1, 1, 1), fn = normal_loglik, y = y, X = X, hessian = TRUE) # set hessian = TRUE to optain Hessian matrix, which contains the second-order partial derivates

inv_hess <- solve(result$hessian) # take the inverse of the Hessian to get the Fisher information matrix

sigma <- sqrt(diag(inv_hess)) # the square root of the diagonals are then the standard errors

estimates_df <- data.frame(params = result$par,
                           ci95lo = result$par - 1.96 * sigma,
                           ci95hi = result$par + 1.96 * sigma)
rownames(estimates_df) <- c("beta_0", "beta_1", "sigma2")
estimates_df






