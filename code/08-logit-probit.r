# ************************************************
### simon munzert
### logit and probit
# ************************************************

source("packages.r")
source("functions.r")


### Running a logit model -----------------------

data("turnout") # example dataset from the Zeliig package
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
  logl <- sum(-y*log(1+exp(- X %*% b)) # for y = 1
              - (1-y)*log(1+exp(X %*% b))) # for y = 0
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
                                     
# BIC
# see J. Scott Long and Jeremy Freese 2000. Stata Technical Bulletin STB-56:34-40
D.M.mod <- as.numeric(-2*LogLik)
N <- length(logit_out$y)
k <- length(logit_out$coef)
df.mod <- N - k
BIC.mod <- D.M.mod - df.mod*log(N)
BIC.mod


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



### From log odds to odds ratios and probabilities ---

summary(logit_out)
cbind(log_odds <- round(coef(logit_out), 4),
      odds_ratios <- round(exp(coef(logit_out)), 4),
      probabilities <- round(1/(1+exp(-coef(logit_out))), 4))

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




## Interactions in logit models -------------

logit_out_int <- glm(vote ~ educate + income*age,
                 family = binomial, data = turnout)
summary(logit_out_int)

hist(turnout$income)

df <- data.frame(educate = 12, age = rep(min(turnout$age):max(turnout$age), 10), income = rep(1:10, each = diff(range(turnout$age))+1))
model_preds <- predict(logit_out, newdata = df, type = 'response', se.fit = TRUE)
df$prediction <- model_preds$fit 

plot(df$age, df$prediction, cex = 0)
income_values <- unique(df$income)
for(i in income_values) { 
 with(filter(df, income == income_values[i]), lines(age, prediction))
  }
#plot(df$income, df$prediction)

# also see 
browseURL("https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html") 
# --> section "Interactions in Logit"


## Credit Default data -------------

?Default
dat <- Default
dat$def <- ifelse(as.numeric(dat$default) == 2, 1, 0)
table(dat$def)

pdf(file="../output/logit-viz-1.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$balance, dat$def, col = "blue", xlab = "Balance", ylab = "Credit default (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
dev.off()

pdf(file="../output/logit-viz-2.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$balance, dat$def, col = "blue", xlab = "Balance", ylab = "Credit default (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
abline(lm(def~balance, data = dat), col = "red", lwd = 2)
dev.off()

pdf(file="../output/logit-viz-3.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$balance, dat$def, col = "blue", xlab = "Balance", ylab = "Credit default (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(def ~ balance, data=dat, family = binomial)
newdat <- data.frame(balance = seq(min(dat$balance), max(dat$balance),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ balance, newdat, col="red", lwd=2)
dev.off()

pdf(file="../output/logit-viz-4.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$balance, dat$def, col = "blue", xlab = "Balance", ylab = "Credit default (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(def ~ balance, data=dat, family = binomial)
newdat <- data.frame(balance = seq(min(dat$balance), max(dat$balance),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ balance, newdat, col="red", lwd=2)
abline(v=c(500, 1000), col = "black", lwd = 3)
arrows(500, .3, 1000, .3, lwd = 3)
dev.off()

pdf(file="../output/logit-viz-5.pdf", height=4, width=6, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(4,4,1.2,.5))
plot(dat$balance, dat$def, col = "blue", xlab = "Balance", ylab = "Credit default (No/Yes)")
abline(h=0:1, lty = 3)
abline(h=.5, lty = 3)
# logit fit
fit = glm(def ~ balance, data=dat, family = binomial)
newdat <- data.frame(balance = seq(min(dat$balance), max(dat$balance),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
lines(vs ~ balance, newdat, col="red", lwd=2)
abline(v=c(1500, 2000), col = "black", lwd = 3)
arrows(1500, .3, 2000, .3, lwd = 3)
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


