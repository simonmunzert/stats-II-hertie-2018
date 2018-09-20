# ************************************************
### simon munzert
### interaction models
# ************************************************

source("packages.r")
source("functions.r")


# ************************************************
# Implementing interactive relationships ---------

data("wage1")
?wage1

# without interaction
summary(wage_model <- lm(wage ~ exper + tenure, data = wage1))

# with interaction
summary(wage_model <- lm(wage ~ exper + tenure + exper*tenure, data = wage1))

# with interaction, multiplicative variable created manually
wage1$experXtenure <- wage1$exper * wage1$tenure
View(select(wage1, wage, exper, tenure, experXtenure))
summary(wage_model <- lm(wage ~ exper + tenure + experXtenure, data = wage1))

# with interaction, constitutive variables automatically added
summary(wage_model <- lm(wage ~ exper*tenure, data = wage1))



# ************************************************
# Discrete constitutive terms --------------------

data("wage1")
?wage1

# recode continuous variable to discrete variable
table(wage1$edu)
wage1$educ_cat <- car::recode(wage1$educ, "0:9='low' ; 
                                           10:14='medium' ; 
                                           15:18='high'",
                                           as.factor.result = TRUE,
                                           levels = c("low", "medium", "high"))

# run model
summary(wage_model <- lm(wage ~ educ_cat*female, data = wage1))



# ************************************************
# Replicating Brambor et al. Fig. 2 --------------

# simulation setup
set.seed(123)
N <- 500
x <- runif(N, 0, 3)
z <- rbinom(N, 1, .5)
e <- rnorm(N, 0, .5)
b0 <- 2
b1 <- 0
b2 <- 2
b3 <- 2
y <- b0 + b1*x + b2*z + b3*x*z + e
dat <- as.data.frame(y = y, x = x, z = z)
# plot points
plot(x[z==0], y[z==0], pch = 1, ylim = c(0, 10), xlab = "X", ylab = "Y")
points(x[z==1], y[z==1], pch = "+")
# compute models
model_full <- lm(y ~ x*z, data = dat)
model_red  <- lm(y ~ x*z - z, data = dat)
# add lines, full model
abline(coef = c(coef(model_full)[1], coef(model_full)[2]), lwd = 3)
abline(coef = c(coef(model_full)[1] + coef(model_full)[3], coef(model_full)[2] + coef(model_full)[4]), lwd = 3)
# add lines, reduced model
abline(coef = c(coef(model_red)[1], coef(model_red)[2]), lwd = 3, col = "red")
abline(coef = c(coef(model_red)[1], coef(model_red)[2] + coef(model_red)[3]), lwd = 3, col = "red")



# ************************************************
# Plotting marginal effects ----------------------

## the interplot packacge

# continuous * continuous setup
summary(wage_model <- lm(wage ~ exper*tenure, data = wage1))

# tenure as moderator
interplot(m = wage_model, var1 = "exper", var2 = "tenure", hist  = TRUE) +
  xlab('Tenure') +
  ylab('Estimated coefficient for experience') +
  ggtitle('Estimated coefficient of experience\non wage by tenure') +
  theme(plot.title = element_text(face='bold'))

# experience as moderator
interplot(m = wage_model, var1 = "tenure", var2 = "exper", hist  = TRUE) +
  xlab('Experience') +
  ylab('Estimated coefficient for tenure') +
  ggtitle('Estimated coefficient of tenure\non wage by experience') +
  theme(plot.title = element_text(face='bold'))

# continuous * discrete setup
wage1$woman <- as.factor(wage1$female)
summary(wage_model <- lm(wage ~ exper*woman, data = wage1))

# tenure as moderator
interplot(m = wage_model, var1 = "exper", var2 = "woman") +
  xlab('Woman') +
  ylab('Estimated coefficient for experience') +
  ggtitle('Estimated coefficient of experience\non wage by gender') +
  theme(plot.title = element_text(face='bold'))

# experience as moderator
interplot(m = wage_model, var1 = "woman", var2 = "exper", hist  = TRUE) +
  xlab('Woman') +
  ylab('Estimated Coefficient for gender') +
  ggtitle('Estimated Coefficient of gender\non wage by experience') +
  theme(plot.title = element_text(face='bold'))

# for more ways to tweak the plots, see
browseURL("https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html")



# ************************************************
# Diagnosing non-linearity and common support ----

library(interflex)
# Mac users facing installationg problems should check out
# browseURL("http://yiqingxu.org/software/interaction/RGuide.html")
# and perform the listed Terminal calls

# again, the model
summary(wage_model <- lm(wage ~ exper*tenure, data = wage1))

# raw plots
inter.raw(Y = "wage", D = "tenure", X = "exper", data = wage1)
inter.raw(Y = "wage", D = "exper", X = "tenure", data = wage1)
inter.raw(Y = "wage", D = "female", X = "exper", data = wage1)


# plots of Generalized Additive Model (GAM)
inter.gam(Y = "wage", D = "exper", X = "tenure", data = wage1)

# binning
inter.binning(Y = "wage", D = "female", X = "exper", Z = c("tenure"), data = wage1)
inter.binning(Y = "wage", D = "exper", X = "tenure", Z = c("female"), data = wage1, Xdistr = "density")
inter.binning(Y = "wage", D = "exper", X = "tenure", Z = c("female"), data = wage1, cutoffs = (seq(0, 40, 10)))

# kernel estimation
inter.kernel(Y = "wage", D = "exper", X = "tenure", Z="female", data = wage1, nboots = 200, parallel = TRUE, cores = 3)



