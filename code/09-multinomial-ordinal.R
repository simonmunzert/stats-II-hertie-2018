# ************************************************
### simon munzert
### ordered and nominal outcomes
# ************************************************

source("packages.r")
source("functions.r")


### Run an ordered logit model -----------------------

# load MASS library
library(MASS)

## Example data from 1977, 1989 GSS:  Attitudes towards working mothers
## "A working mother can establish just as warm and secure of a relationship
##  with her child as a mother who does not work." Strongly disagree (SD), Disagree (D), Agree (A), Strongly agree (SA)

# load data
workmom <- read.csv("../data/ordwarm2.csv", header=TRUE, sep=",")

workmom$warmf <- factor(workmom$warm, labels=c("Strongly Disagree",
                                               "Disagree",
                                               "Agree",
                                               "Strongly Agree"))

workmom77 <- workmom[workmom$yr89==0, ]
workmom89 <- workmom[workmom$yr89==1, ]

# descriptives
workmom77 %>% group_by(male) %>% summarize(warm = mean(warm == 3 | warm == 4)) # agree or strongly agree
workmom77 %>% group_by(white) %>% summarize(warm = mean(warm == 3 | warm == 4)) # agree or strongly agree
workmom77 %>% group_by(age > 60) %>% summarize(warm = mean(warm == 3 | warm == 4)) # agree or strongly agree

## covariates: 
# male respondent; 
# white respondent; 
# age of respondent;
# years of education of respondent;
# prestige of respondent's occupation (% considering prestigious)

# Use MASS::polr to do ordered logit (for probit, simply change method to "probit")
warm77_olog <- polr(warmf ~ male + white + age + ed + prst, data = workmom77, method = "probit", na.action = na.omit)
summary(warm77_olog)
stargazer(warm77_olog, ord.intercepts = TRUE, type="text")

# predicted values
predict_class <- predict(warm77_olog, type = "class")
predict_probs <- predict(warm77_olog, type = "probs")

# predicted vs. true
table(predict_class, workmom77$warmf)

# Some questions: 
# 1. An increase on which variables makes "Agree" and "Strongly agree" answers more likely?
# 2. Which probabilities for each category would we predict for a white, 25-year old with 15 years of education and job prestige of .8?
predict(warm77_olog, newdata = data.frame(male = 1, white = 1, age = 25, ed = 15, prst = .8), type = "probs") # single case

# by hand for category 1 ("Strongly Disagree")
pnorm(warm77_olog$zeta[1], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1)

# by hand for category 2 ("Disagree")
pnorm(warm77_olog$zeta[2], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1) - 
  pnorm(warm77_olog$zeta[1], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1)

# by hand for category 3 ("Agree")
pnorm(warm77_olog$zeta[3], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1) - 
  pnorm(warm77_olog$zeta[2], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1)

# by hand for category 4 ("Strongly Agree")
1 - pnorm(warm77_olog$zeta[3], mean = sum(warm77_olog$coefficients * c(1, 1, 25, 15, .8)), sd = 1)


### Visualize predicted probabilities from  ordinal logit model -----------------------

library(ZeligChoice)
library(magrittr)

# run model
z_out <- zelig(warmf ~ male + white + age + ed + prst, data = workmom77, model = "ologit")
summary(z_out)

# simulate probabilities
male_yes <- setx(z_out, male = 1) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
male_no <- setx(z_out, male = 0) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
white_yes <- setx(z_out, white = 1) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
white_no <- setx(z_out, white = 0) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
age_25 <- setx(z_out, age = 25) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
age_65 <- setx(z_out, age = 65) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
edu_low <- setx(z_out, ed = 8) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame
edu_high <- setx(z_out, ed = 18) %>% sim(z_out, x = .) %>% extract2("sim.out") %>% extract2("x") %>% extract2("ev") %>% extract2(1) %>% as.data.frame

# get summary statistics
get_summary <- function(x) {
  out <- matrix(ncol = 3, nrow = 4)
  out[,1] <- colMeans(x)
  out[,2] <- apply(x, 2, quantile, .05)
  out[,3] <- apply(x, 2, quantile, .90)
  out <- as.data.frame(out)
  names(out) <- c("mean", "ci90lo", "ci90hi")
  out
}

sims_list <- list(male_yes, male_no, white_yes, white_no, age_25, age_65, edu_low, edu_high)
sims_sum_list <- list()
for (i in 1:length(sims_list)) {
  sims_sum_list[[i]] <- get_summary(sims_list[[i]])
  
}

labels <- c("Male", "Female", "White", "Nonwhite", "Age = 25", "Age = 65", "Education = low", "Education = high")
sd_mean <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "mean1"]
d_mean <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "mean2"]
a_mean <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "mean3"]
sa_mean <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "mean4"]

sd_ci90lo <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90lo1"]
d_ci90lo <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90lo2"]
a_ci90lo <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90lo3"]
sa_ci90lo <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90lo4"]

sd_ci90hi <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90hi1"]
d_ci90hi <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90hi2"]
a_ci90hi <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90hi3"]
sa_ci90hi <- unlist(sims_sum_list)[names(unlist(sims_sum_list)) == "ci90hi4"]

sd_sims_sum <- sapply(sims_sum_list, `[`, 1,) %>% t  %>% apply(2, as.numeric) %>% as.data.frame %>% arrange(mean)
d_sims_sum <- sapply(sims_sum_list, `[`, 2,) %>% t  %>% apply(2, as.numeric) %>% as.data.frame %>% arrange(mean)
a_sims_sum <- sapply(sims_sum_list, `[`, 3,) %>% t  %>% apply(2, as.numeric) %>% as.data.frame %>% arrange(mean)
sa_sims_sum <- sapply(sims_sum_list, `[`, 4,) %>% t  %>% apply(2, as.numeric) %>% as.data.frame %>% arrange(mean)


# generate plot
par(mfrow=c(4, 1))
par(mar=c(2, 8, 2, 1))
# strongly disagree
plot(sd_sims_sum$mean, seq_along(sd_sims_sum$mean), xlim = c(0, .5), ylim = c(.5, 8.5), pch = 19, xlab = "", ylab = "", yaxt = "n", main = "Probability 'Strongly disagree'")
for (i in seq_along(sd_sims_sum$mean)) {
  segments(y0 = i, y1 = i, x0 = sd_sims_sum$ci90lo[i], x1 = sd_sims_sum$ci90hi[i])
}
axis(2, at = seq_along(sd_sims_sum$mean), labels = labels[order(sd_mean)], las = 1)
abline(v=seq(0,1,.1), lty = 2, col = "grey")
# disagree
plot(d_sims_sum$mean, seq_along(d_sims_sum$mean), xlim = c(0, .5), ylim = c(.5, 8.5), pch = 19, xlab = "", ylab = "", yaxt = "n", main = "Probability 'Disagree'")
for (i in seq_along(d_sims_sum$mean)) {
  segments(y0 = i, y1 = i, x0 = d_sims_sum$ci90lo[i], x1 = d_sims_sum$ci90hi[i])
}
axis(2, at = seq_along(d_sims_sum$mean), labels = labels[order(d_mean)], las = 1)
abline(v=seq(0,1,.1), lty = 2, col = "grey")
# agree
plot(a_sims_sum$mean, seq_along(a_sims_sum$mean), xlim = c(0, .5), ylim = c(.5, 8.5), pch = 19, xlab = "", ylab = "", yaxt = "n", main = "Probability 'Agree'")
for (i in seq_along(a_sims_sum$mean)) {
  segments(y0 = i, y1 = i, x0 = a_sims_sum$ci90lo[i], x1 = a_sims_sum$ci90hi[i])
}
axis(2, at = seq_along(a_sims_sum$mean), labels = labels[order(a_mean)], las = 1)
abline(v=seq(0,1,.1), lty = 2, col = "grey")
# strongly agree
plot(sa_sims_sum$mean, seq_along(sa_sims_sum$mean), xlim = c(0, .5), ylim = c(.5, 8.5), pch = 19, xlab = "", ylab = "", yaxt = "n", main = "Probability 'Strongly agree'")
for (i in seq_along(sa_sims_sum$mean)) {
  segments(y0 = i, y1 = i, x0 = sa_sims_sum$ci90lo[i], x1 = sa_sims_sum$ci90hi[i])
}
axis(2, at = seq_along(sa_sims_sum$mean), labels = labels[order(sa_mean)], las = 1)
abline(v=seq(0,1,.1), lty = 2, col = "grey")




### Run a multinomial logit model -----------------------

gator <- read_csv("../data/alligator.csv") # data example from Agresti (2002)

# food: (1) fish, (2) invertebrates, (3) = other
# size: in meters
# female: 0/1

# Q: "How does alligator size and sex influence food choice?"

# estimate model
library(nnet) # for multinom()

model_out <- multinom(food ~ size + female, data = gator)
summary(model_out)
stargazer(model_out, type="text")
stargazer(model_out, title = "Explaining Alligators' diets", column.labels = c("$\\text{log}(\\frac{\\text{Pr}(\\text{invertebrate})}{\\text{Pr}(\\text{fish})})$", "$\\text{log}(\\frac{\\text{Pr}(\\text{other})}{\\text{Pr}(\\text{fish})})$"), model.numbers = FALSE, dep.var.labels.include = FALSE, style = "apsr", font.size = "scriptsize", single.row = FALSE, no.space = TRUE, type = "latex", out = "../output/alligators_multinomial.tex")


# Some questions: 
  # 1. How does the alligator's size influence the choice of an invertebrate diet vs. a fish diet?
        # A: A 1 meter increase in length makes the ODDS that an alligator will eat invertebrates rather than fish 
        exp(-2.53)
        # times smaller
  # 2. How does the alligator's size influence the choice of an invertebrate diet vs. an "other" diet?
        # A: A 1 meter increase in length makes the ODDS that an alligator will eat invertebrates rather than "other" food 
        exp(-2.53 - 0.13)
        # times smaller
  # 3. How does the alligator's size influence the choice of a fish diet vs. an "other" diet?
        # A: A 1 meter increase in length makes the ODDS that an alligator will eat fish rather than "other" food 
        exp(0 - 0.13)
        # times smaller
# --> Big alligators are more likely to east mostly fish and "other" food, relative to invertebrates
        

### Visualize predicted probabilities from  multinomial logit model -----------------------

# retrieve probabilities
predict_out <- predict(model_out, type = "probs") %>% as.data.frame()
names(predict_out) <- c("prob_fish", "prob_invert", "prob_other")
gator_pred <- cbind(gator, predict_out)  

# plot probabilities
pdf(file="../output/multinom_predprobs.pdf", height=4, width=7, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(5,5,1.5,.5))
dat_male <- filter(gator_pred, female == 0) %>% arrange(prob_fish)
plot(dat_male$size, dat_male$prob_fish, type = "l", ylim = c(0, 1), col = "blue", main = "Male alligators", ylab = "Probability primary diet is...", xlab = "Size of alligator (meters)")
lines(dat_male$size, dat_male$prob_invert, col = "red")
lines(dat_male$size, dat_male$prob_other, col = "brown")
text(1.5, dat_male$prob_fish[1]+.1, "Fish", col = "blue")
text(1.5, dat_male$prob_invert[1]+.1, "Invertebrates", col = "red")
text(1.5, dat_male$prob_other[1]+.1, "Other", col = "brown")
dev.off()


### Another option: present differences in probabilities  -----------------------

# ZeligChoice package allows to simulate outcomes from a variety of logit and probit models (add-on to Zelig package)
library(ZeligChoice)

gator$food2 <- car::recode(gator$food, "1=3;3=1") # last category is base category in zelig(); we want to keep order from above 
z_out <- zelig(food2 ~ size + female, model = "mlogit", data = gator)
summary(z_out)

# set contrasts on size variable
size_small <- setx(z_out, size = 1.5)
size_big <- setx(z_out, size = 3.5)

# simulate predicted probabilities
s_out <- sim(z_out, x = size_small, x1 = size_big)
s_out
#plot(s_out)

# extract first differences, summary statistics
sim_out_fd <- s_out$sim.out$x1$fd[[1]] %>% as.data.frame()
names(sim_out_fd) <- c("prob_other", "prob_invert", "prob_fish")
sim_out_summary <- 
  summarize(sim_out_fd, fish_mean = mean(prob_fish),
                      fish_95lo = quantile(prob_fish, .025),
                      fish_95hi = quantile(prob_fish, .975),
                      invert_mean = mean(prob_invert),
                      invert_95lo = quantile(prob_invert, .025),
                      invert_95hi = quantile(prob_invert, .975),
                      other_mean = mean(prob_other),
                      other_95lo = quantile(prob_other, .025),
                      other_95hi = quantile(prob_other, .975)
)
sim_out_summary   

# generate plot
pdf(file="../output/multinom_predfds.pdf", height=4, width=7, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(5, 7, 2, 1))
plot(sim_out_summary$fish_mean, 3, xlim = c(-1, 1), ylim = c(.5, 3.5), pch = 19, xlab = "Difference in probability\nthis is Alligator's primary diet", ylab = "", yaxt = "n", main = "Large alligators (3.5 meters) compared to small (1.5 meters)")
points(sim_out_summary$invert_mean, 2, xlim = c(-1, 1), ylim = c(.5, 3.5), pch = 19)
points(sim_out_summary$other_mean, 1, xlim = c(-1, 1), ylim = c(.5, 3.5), pch = 19)
segments(y0 = 3, y1 = 3, x0 = sim_out_summary$fish_95lo, x1 = sim_out_summary$fish_95hi)
segments(y0 = 2, y1 = 2, x0 = sim_out_summary$invert_95lo, x1 = sim_out_summary$invert_95hi)
segments(y0 = 1, y1 = 1, x0 = sim_out_summary$other_95lo, x1 = sim_out_summary$other_95hi)
axis(2, at = c(3, 2, 1), labels = c("Fish", "Invertebrates", "Other food"), las = 1)
abline(v=0)
abline(v=seq(-1,1,.25), lty = 2)
dev.off()



