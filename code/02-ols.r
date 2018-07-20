# ************************************************
### simon munzert
### ols and multiple regression
# ************************************************

source("packages.r")
source("functions.r")



# ************************************************
# Bivariate regression, OLS estimation -----------

# load data
data("wage1")

# inspect relationship
qplot(educ, wage, data = wage1)
plot(wage1$educ, wage1$wage)
cor(wage1$wage, wage1$educ)
with(wage1, cor(educ, wage))

# run linear model
wage_model <- lm(wage ~ educ, data = wage1)
summary(wage_model)
coef(wage_model)

# export model
stargazer(wage_model, single.row = TRUE, header = FALSE, style = "apsr", type = "html", out = "../output/wage_model_ols.html")
browseURL("../output/wage_model_ols.html")

# manual computation
covariance_xy <- with(wage1, cov(wage, educ))
variance_x <- var(wage1$educ)
covariance_xy / variance_x # beta_1
mean(wage1$wage) - (covariance_xy / variance_x)*mean(wage1$educ) # beta_0



# ************************************************
# Multiple regression, OLS estimation ------------

# run linear model
wage_model <- lm(wage ~ educ + female, data = wage1)
summary(wage_model)
coef(wage_model)

# manual computation for beta_educ

# step 1: isolate part of X that is linearly independent from Z
wage_model_xz <- lm(educ ~ female, data = wage1)
resid_xz <- residuals(wage_model_xz)

# step 2: explain Y by part of X that is independent from Z
wage_model_yx <- lm(wage1$wage ~ resid_xz)
coef(wage_model_yx)

## continue in analogy to arrive at point estimate for beta_female.



# ************************************************
# How are coefficients from simple and -----------
# multiple OLS estimates related? ----------------

wage_model <- lm(wage ~ educ, data = wage1)
coef_simple <- coef(wage_model)[2] 

wage_model_mult <- lm(wage ~ educ + female, data = wage1)
coef_multiple <- coef(wage_model_mult)[2]

wage_model_zx <- lm(female ~ educ, data = wage1)
coef_simple_zx <- coef(wage_model_zx)[2]

# these two are the same:
coef_simple
coef_multiple + coef(wage_model_mult)[3]*coef(wage_model_zx)[2]



# ***************************************************
# Effects of controls on coefficients of interest ---

wage_model_1 <- lm(wage ~ educ, data = wage1)
wage_model_2 <- lm(wage ~ educ + female, data = wage1)
wage_model_3 <- lm(wage ~ educ + female + nonwhite, data = wage1)
wage_model_4 <- lm(wage ~ educ + female + nonwhite + profocc, data = wage1)

stargazer(wage_model_1, wage_model_2, wage_model_3, wage_model_4, header = FALSE, style = "apsr", type = "html", out = "../output/wage_model_multiple.html")
browseURL("../output/wage_model_multiple.html")
stargazer(wage_model_1, wage_model_2, wage_model_3, wage_model_4, header = FALSE, title = "Regressing wage on...", omit.table.layout = "dln", style = "apsr", font.size = "scriptsize", df = FALSE, report = "vcs", single.row = FALSE, no.space = TRUE, type = "latex", out = "../output/wage_model_multiple.tex")

cor(select(wage1, wage, educ, female, nonwhite, profocc)) 



# ************************************************
# Post-estimation bias: simulation ---------------

set.seed(42)
N <- 500
x <- rnorm(N, 0, 1)
ed <- rnorm(N, 0, .5)
d <- .8*x + ed  # d is mediator
ey <- rnorm(N, 0, .5)
y <- x + d + ey

cor(cbind(y, x, d))
plot(x, y)

# complete "true" models
summary(lm(d ~ x))
summary(lm(y ~ x))

# model with induced post-treatment bias
summary(lm(y ~ x + d))



# ************************************************
# Multicollinearity: detection -------------------

data(bwght2)
?bwght2
vars <- c("mage", "meduc", "monpre", "npvis", "fage", "feduc", "cigs", "drink", "male")
b <- paste(vars, collapse = "+")
frmla <- as.formula(paste("bwght ~ ", b, sep = ""))

# example: perfect collinearity
summary(lm(bwght ~ fwhte + fblck + foth, data = bwght2))

# full model
model_out <- lm(frmla, data = bwght2)
summary(model_out)

# inspect correlation matrix
cor(bwght2[,vars], use = "pairwise.complete.obs")

# inspect scatterplots
plot(bwght2[,vars])

# evaluate variance inflation factors
vif(model_out) # function from car package



# ************************************************
# Anscombe's quartett ----------------------------

?anscombe
summary(anscombe)

# correlation
sapply(1:4, function(x) cor(anscombe[, x], anscombe[, x+4]))

# variance
sapply(5:8, function(x) var(anscombe[, x]))

# linear regression
summary(lm(y1 ~ x1, data = anscombe))
summary(lm(y2 ~ x2, data = anscombe))
summary(lm(y3 ~ x3, data = anscombe))
summary(lm(y4 ~ x4, data = anscombe))

# plot
ff <- y ~ x
par(mfrow = c(2, 2))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2, xlim = c(3, 19), ylim = c(3, 13))
  model_out <- lm(ff, data = anscombe)
  abline(model_out, col = "blue")
}
mtext("Anscombe's 4 regression data sets", outer = TRUE, cex = 1.5)



# ************************************************
# Functional form misspecification ---------------

N <- 500
x <- runif(N, 0, 1)
y <- log(x) + rnorm(N, 0, .3)

# look at model
summary(lm(y ~ x))
summary(lm(y ~ log(x)))

# plot relationship
par(mfrow = c(1, 3))
# misspecified
plot(x, y); abline(lm(y ~ x), col = "red")
# correctly specified
dat <- data.frame(x = sort(x), ypred = predict(lm(y ~ log(x)), newdata = data.frame(x = sort(x))))
plot(x, y); lines(dat$x, dat$ypred, col = "red")
# correctly specified
plot(log(x), y); abline(lm(y ~ log(x)), col = "red")

# RESET test
resettest(lm(y ~ x))
resettest(lm(y ~ log(x)))





