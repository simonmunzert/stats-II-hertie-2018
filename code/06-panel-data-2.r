# ************************************************
### simon munzert
### panel data II
# ************************************************

source("packages.r")
source("functions.r")


## CRAN Task View on econometrics (including panel data models)
browseURL("https://cran.r-project.org/web/views/Econometrics.html")


# ************************************************
# Example: Comparative Political Dataset ---------

dat <- read_dta("../data/CPDS-1960-2015.dta")
dat <- dplyr::select(dat, year, country, iso, gov_left2, socexp_t_pmp, instcons, unemp, eu)
head(dat)



# ************************************************
# Panel data econometrics with R: the plm package

library(plm)

# inspect dimensionality
pdim(dat, index = c("iso", "year"))

# panel data estimators
plm() # plm can do (amongst others):
  # pooling:           --> model = "pooling" (same as lm())
  # fixed effects:     --> model = "within"
  # random effects:    --> model = "random"
  # first differences: --> model = "fd"

# various tests
mtest() # Arellano-Bond test of serial correlation for GMM models based on the residuals
pbgtest() # Breusch-Godfrey test of serial correlation for (the idiosyncratic component of) errors in panel models (pooling, within, or random) based on the residuals
pdwtest () # Durbin-Watson test of serial correlation for (the idiosyncratic component of) errors in panel models (pooling, within, or random) based on the residuals; don't use in models with autoregressive effects!
phtest() # (Durbin-Wu-)Hausman test based on the difference of the vectors of coefficients of (usually) a FE and RE model
purtest() # Unit root tests

# extract fixed effects
fixef()

# make data balanced
make.pbalanced()

# extract indices of panel data
indices <- index(model_fe)
table(indices$iso, indices$year)

# for more information, see
browseURL("https://cran.r-project.org/web/packages/plm/vignettes/plm.pdf")



# ************************************************
# Implementing the Fixed-Effects estimator -------

# pooled OLS
summary(model_pooled <- lm(socexp_t_pmp ~ gov_left2 + instcons  + unemp, data = dat))
hist(dat$gov_left2)
# interplot(model_pooled, var1 = "gov_left2", var2 = "instcons")
# interplot(model_pooled, var1 = "gov_left2", var2 = "unemp")

# pooled OLS with plm
summary(model_pooled_plm <- plm(socexp_t_pmp ~ gov_left2 + instcons  + unemp, data = dat, index = c("iso", "year"), model = "pooling"))

# fixed-effects estimator, manually
summary(model_fe <- lm(socexp_t_pmp ~ gov_left2 + instcons  + unemp + iso, data = dat))

# fixed-effects estimator with plm
summary(model_fe_plm <- plm(socexp_t_pmp ~ gov_left2 + instcons + unemp, data = dat, index = c("iso", "year"), model = "within"))

# F test for fixed effects
pFtest(model_fe_plm, model_pooled_plm) # F test FE vs pooling model




# ************************************************
# How do fixed effects look like? ----------------

# visualize fixed effects
fixed_effects <- c(coef(model_fe)[1], 
                   coef(model_fe)[1] + coef(model_fe)[5:(length(coef(model_fe)))])
fe_dat <- data.frame(iso = unique(model_fe$model$iso),
                     fixed_effect = fixed_effects)

# we could've had this easier...
fixef(model_fe_plm)

# plot fixed effects
ggplot(fe_dat, aes(x = reorder(iso, fixed_effect), y = fixed_effect)) + geom_bar(stat = "identity") + labs(x = "")

# compute raw means
dat_sum <- group_by(dat, iso) %>% summarize(n_obs = n(), 
                                            mean_socexp = mean(socexp_t_pmp, na.rm = T))

# merge model fixed effects with raw means: don't confuse fixed effects with "normal levels" or something - they simply account for unobserved constant effects, i.e. the portion of y in i that is not explained by the other, time-varying covariates and that is unit-constant across time
fe_dat <- merge(fe_dat, dat_sum, by = "iso", all.x = TRUE)
plot(fe_dat$fixed_effect, fe_dat$mean_socexp)
text(fe_dat$fixed_effect, fe_dat$mean_socexp, fe_dat$iso)



# ******************************************************
# LSDV/FE estimation: Is demeaning and introducing 
# unit dummies really equivalent?

set.seed(1)
x = rnorm(100)
fe = rep(rnorm(10), each = 10)
id = rep(1:10,each = 10)
ti = rep(1:10, 10)
e = rnorm(100)
y = x + fe + e
dat_sim = data.frame(y, x, id, ti)

# standard FE model
reg_fe = plm(y ~ x, model = "within", index = c("id", "ti"), data = dat_sim)
summary(reg_fe)

# FE model using demeaning
y_dem = y - tapply(y, id, mean)[id]
x_dem = x - tapply(x, id, mean)[id]

reg_dem = lm(y_dem ~ -1 + x_dem) # note that we do not estimate the intercept because we have demeaned the data
summary(reg_dem) # note that the standard error is wrong. We would need to account for that we are losing degrees of freedom by taking out the fixed effects.


# ******************************************************
# What about time-fixed effects?

  # would pick up variation in the outcome that occurs over time and is not attributed to other explanatory variables
  # --> capture the influence of aggregate (time-series) trends
  # --> could help you deal with heteroskedasticity across time, i.e. variance of the errors increasing or decreasing with time

# fixed-effects estimator with plm
summary(model_fe_time <- plm(socexp_t_pmp ~ gov_left2 + instcons + gov_left2 + unemp, data = dat, index = c("iso", "year"), model = "within", effect = "twoways"))

# effect = 
  # individual --> cross-sectional fixed effects
  # time --> time fixed effects
  # twoways --> time and cross-sectional fixed effects a.k.a. twoway fixed effects

# test for the need of time-fixed effects
pFtest(model_fe_time, model_fe_plm)

# Breusch-Pagan test for heteroskedasticity across time
plmtest(model_fe_plm, effect = c("time"), type = ("bp"))





# ************************************************
# Implementing the Random-Effects estimator ------

summary(model_re_plm <- plm(socexp_t_pmp ~ gov_left2 + instcons + unemp, data = dat, index = c("iso", "year"), model = "random"))

format(coef(model_fe_plm), scientific = FALSE)
format(coef(model_re_plm), scientific = FALSE)

# Hausman test
phtest(model_fe_plm, model_re_plm)



# *****************************************
# Using Panel-Corrected Standard Errors (PCSEs) --


# Beck and Katz 1995: 
  # TSCS data, such as country panels, often tackled with models that do not account properly for complex error structures 
  # common problem: panel heteroskedasticity, where each country has its own error variance
  # contemporaneous correlation of errors: due to common shocks, error for one country might be correlated with errors for other countries in same year
  # autocorrelation: errors for a given country are correlated with previous errors for that country

# it's a mess!
# this has an impact on your standard errors --> observations are more similar than they should be
# PCSE estimator suggested as one remedy for this problem - it's pretty popular in political science
# there are others on the market, such as Driscoll and Kraay (1998)’s spatial correlation consistent (SCC) estimator


# extract complete cases
dat_sub <- dplyr::select(dat, socexp_t_pmp, gov_left2, instcons, unemp, iso, year)
dat_sub <- dat_sub[complete.cases(dat_sub),]

# run OLS
summary(model_pooled <- lm(socexp_t_pmp ~ gov_left2 + instcons + unemp, data = dat_sub))

# panel-corrected standard errors
model_pcse <- pcse(model_pooled, groupN = dat_sub$iso, groupT = dat_sub$year)
summary(model_pcse)

# pcse() cannot be applied on plm objects - there is another solution:
summary(model_pooled <- plm(socexp_t_pmp ~ gov_left2 + instcons + unemp, data = dat_sub, index = c("iso", "year"), model = "pooling"))
plm2 <- coeftest(model_pooled, vcov=function(x) vcovBK(x, type = "HC1", cluster = "time"))
plm2


