# ************************************************
### simon munzert
### logit and probit
# ************************************************

source("packages.r")
source("functions.r")


### Running a logit model -----------------------

data("turnout") # example dataset from the Zelig package
?turnout
tabyl(turnout$vote)

# estimation with glm function
logit_out <- glm(vote ~ age + educate + income,
                 family = binomial, data = turnout)
summary(logit_out)

# estimation with manually programmed logit likelihood function
# see also http://www.stat.cmu.edu/~cshalizi/uADA/12/lectures/ch12.pdf
logit_loglik <- function(theta, y, X){
  b <- theta
  logl <- sum(     y  * log(1/(1 + exp(- X %*% b))) + # for y = 1
              (1 - y) * log(1/(1 + exp(X %*% b)))) # for y = 0
  return(-logl)
}	

logit_out_manual <- optim(rep(1,4), logit_loglik, y = turnout$vote, X = as.matrix(cbind(1,(turnout[,c("age", "educate", "income")]))), method = "BFGS") 
logit_out_manual  


### Running a probit model ---------------------

# estimation with glm function
probit_out <- glm(vote ~ age + educate + income,
                 family = binomial(link = "probit"), data = turnout)
summary(probit_out) # careful - the coefficients are not directly comparable with those from the logit model! (Divide logit coefficients by approximately 1.6 to arrive at probit coefficients; see http://andrewgelman.com/2006/06/06/take_logit_coef/)

# again, a little simulation
n <- 100
x <- rnorm (n) 
a <- 1.5 
b <- 1 
y <- rbinom (n, 1, invlogit(a + b*x)) 
M1 <- glm (y ~ x, family=binomial(link="logit")) 
summary (M1)
M2 <- glm(y ~ x, family=binomial(link="probit")) 
summary (M2)



### From log odds to odds ratios and probabilities ---

summary(logit_out)
cbind(log_odds <- round(coef(logit_out), 2),
      odds_ratios <- round(exp(coef(logit_out)), 2),
      probabilities <- round(1/(1+exp(-coef(logit_out))), 2))

# look at an empty model again to understand how the log odds and the probability are related
logit_out_empty <- glm(vote ~ 1,
                 family = binomial, data = turnout)
summary(logit_out_empty)
summary(turnout$vote)
1/(1+exp(-coef(logit_out_empty)))


# here's a little logit to probability function
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(.5)
logit2prob(-.5)


### Predict marginal effects with the margins package ---

library(margins)
summary(logit_out)
margins(logit_out, type = "link") # this equals the log odds

margins(logit_out, type = "response") # this equals marginal probabilities

margins(logit_out, at = list(age = c(25, 60),
                             educate = c(10, 10),
                             income = c(3, 3)))

marginal_effects(logit_out) # unit-specific marginal effects with respect to all variables 


### Visualize probabilities - graphical solution ---
df <- data.frame(age = min(turnout$age):max(turnout$age), educate = 10, income = 4) 
model_preds <- predict(logit_out, newdata = df, type = 'response', se.fit = TRUE)
df$prediction <- model_preds$fit 
df$lower <- model_preds$fit - 1.96 * model_preds$se.fit 
df$upper <- model_preds$fit + 1.96 * model_preds$se.fit

# graph predicted probabilities
ggplot(df) + geom_line(aes(x = age, y = prediction)) + theme_bw() + geom_ribbon(aes(ymin = lower, ymax = upper, x = age), alpha = .3)

# alternatively, use cplot() command from the margins package
cplot(logit_out, "age")

# graph marginal effects
cplot(logit_out, "age", what = "effect")



### Goodness of fit ----------------------------

# Log-Likelihood
LogLik <- logLik(logit_out)
LogLik

# McFadden's Pseudo-R^2                                 
logit_out_empty <- glm(vote ~ 1, family = binomial, data = turnout) # estimate empty model 
Pseudo_R2 <- 1 - (as.numeric(logLik(logit_out)))/(as.numeric(logLik(logit_out_empty)))
Pseudo_R2

# Likelihood ratio Chi-squared
chi2_test_stat <- 2*(as.numeric(logLik(logit_out)) - as.numeric(logLik(logit_out_empty)))
lr_test <- 1 - pchisq(chi2_test_stat, 1)
chi2_test_stat
lr_test

# AIC
k <- length(logit_out$coef)
AIC <- 2 * k - 2 * logLik(logit_out)
AIC

# BIC
N <- length(logit_out$y)
BIC <- log(N) * k - 2 * logLik(logit_out)
BIC

library(stats4)
AIC(logit_out)
BIC(logit_out)


### Precision and recall -----------------------

turnout$vote_pred_link <- predict(logit_out, type = "link")
turnout$vote_pred_prob <- predict(logit_out, type = "response")
turnout$vote_pred <- ifelse(turnout$vote_pred_prob > .5, 1, 0)

plot(turnout$vote_pred_link, turnout$vote_pred_prob) # links versus response

tab <- table(turnout$vote, turnout$vote_pred)
colnames(tab) <- c("pred vote NO", "pred vote YES")
rownames(tab) <- c("rep vote NO", "rep vote YES")
tab

# precision:
sum(turnout$vote_pred == 1 & turnout$vote == 1) / sum(turnout$vote_pred == 1)

# recall:
sum(turnout$vote_pred == 1 & turnout$vote == 1) / sum(turnout$vote == 1)




## Interactions in logit models -------------

turnout$edu_cat <- cut(turnout$educate, breaks=c(0, 10, 13, Inf), labels=c("low", "mid", "high"))
table(turnout$edu_cat)

logit_out_int <- glm(vote ~ edu_cat + income + edu_cat*age,
                     family = binomial, data = turnout)
summary(logit_out_int)


df <- expand.grid(edu_cat = c("low", "mid", "high"),
                  age = min(turnout$age):max(turnout$age),
                  income = 5,
                  stringsAsFactors = F)

model_preds <- predict(logit_out_int, newdata = df, type = 'response', se.fit = TRUE)
df$prediction <- model_preds$fit 

cols = c("black", "red", "blue")
plot(df$age, df$prediction, cex = 0)
edu_cat_values <- unique(df$edu_cat)
for(i in seq_along(edu_cat_values)) { 
  with(dplyr::filter(df, edu_cat == edu_cat_values[i]), lines(age, prediction, col = cols[i]))
}


# also see 
browseURL("https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html") 
# --> section "Interactions in Logit"





## Simulate data -------------

set.seed(123)
n = 500
x <- rnorm(n)
z <- 1 + 2 * x + rnorm(n, 0, .2) # linear combination with noise
prob = 1/(1+exp(-z))  # pass through inv-logit function
plot(density(prob))
y = rbinom(n,1,prob)  # bernoulli response variable
table(y)

dat <- data.frame(x = scales::rescale(x, to = c(0, 10000))
, y = y, prob = prob, stringsAsFactors = F)

# plot relationship
pdf(file="../output/logit-viz-1.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, dat$y, col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
dev.off()


# plot relationship
pdf(file="../output/logit-viz-1-jitter.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, jitter(dat$y, .25), col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
dev.off()


pdf(file="../output/logit-viz-2.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, dat$y, col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
abline(lm(y~x, data = dat), col = "red", lwd = 2)
dev.off()

pdf(file="../output/logit-viz-3.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, dat$y, col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(y ~ x, data=dat, family = binomial)
newdat <- data.frame(x = seq(min(dat$x), max(dat$x),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ x, newdat, col="red", lwd=2)
dev.off()

pdf(file="../output/logit-viz-4.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, dat$y, col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(y ~ x, data=dat, family = binomial)
newdat <- data.frame(x = seq(min(dat$x), max(dat$x),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ x, newdat, col="red", lwd=2)
abline(v=c(2000, 4000), col = "black", lwd = 3)
arrows(2000, .3, 4000, .3, lwd = 3)
dev.off()

pdf(file="../output/logit-viz-5.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$x, dat$y, col = "blue", xlab = "Income", ylab = "Voted (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(y ~ x, data=dat, family = binomial)
newdat <- data.frame(x = seq(min(dat$x), max(dat$x),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ x, newdat, col="red", lwd=2)
abline(v=c(6000, 8000), col = "black", lwd = 3)
arrows(6000, .3, 8000, .3, lwd = 3)
dev.off()


## Probability and cumulative density functions --------------

# probability and cumulative density functions of the logistic distribution
z_scores <- seq(-5, 5, by = .1)
dlogistic_values <- dlogis(z_scores, 0, 1)
plot(z_scores, dlogistic_values,  type = "l", main = "pdf of the Logistic", xlab= "Z score", ylab="Probability Density", col = "blue") 

dlogistic_values <- plogis(z_scores, 0, 1)
plot(z_scores, dlogistic_values,  type = "l", main = "cdf of the Logistic", xlab= "Z score", ylab="Probability Density", col = "blue") 
# playing with the scale parameter
lines(z_scores, plogis(z_scores, 0, 2), col = "red") 
lines(z_scores, plogis(z_scores, 0, .5), col = "red")
# playing with the location parameter
lines(z_scores, plogis(z_scores, -2, 1), col = "green")
lines(z_scores, plogis(z_scores, 2, 1), col = "green")

# probability and cumulative density functions of the standard normal distribution
z_scores <- seq(-5, 5, by = .1)
dnorm_values <- dnorm(z_scores, 0, 1)
plot(z_scores, dnorm_values,  type = "l", main = "pdf of the Standard Normal", xlab= "Z score", ylab="Probability Density", col = "blue") 

dnorm_values <- pnorm(z_scores, 0, 1)
plot(z_scores, dnorm_values,  type = "l", main = "cdf of the Standard Normal", xlab= "Z score", ylab="Probability Density", col = "blue") 


