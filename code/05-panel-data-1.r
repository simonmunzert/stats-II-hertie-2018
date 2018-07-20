# ************************************************
### simon munzert
### panel data I
# ************************************************

source("packages.r")
source("functions.r")


## CRAN Task View on econometrics (including panel data models)
browseURL("https://cran.r-project.org/web/views/Econometrics.html")



# ************************************************
# Example: Comparative Political Dataset ---------

dat <- read_dta("../data/CPDS-1960-2015.dta")
dat <- filter(dat, emu == 1) # only EMU countries

hist(dat$gov_left2)
hist(dat$socexp_t_pmp)

# plot small multiples of share of left parties and social expenditures
ggplot(data=dat, aes(x=year,y=gov_left2)) +
  geom_line(stat="identity") +
  ylim(0, 100) +
  labs(y = "Left parties in government (% seats in parliament)",
       x = "Year") +
  geom_line(data = dat, aes(x = year, y = socexp_t_pmp*5), color = "blue") +
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Social expenditures [%GDP]")) + 
  facet_wrap(~iso, ncol = 6) +
  theme(axis.text.x = element_text(angle=90))
#ggsave("../output/cpds-govleft-exp.pdf", width = 20, height = 12, units = "cm")


# select variables
?select
dat <- dplyr::select(dat, year, country, iso, eu, emu, gov_left2, socexp_t_pmp, instcons, unemp)
head(dat)

# make data frame wide
dat_wide <- reshape(as.data.frame(dat), direction = "wide", timevar = "year", idvar = c("country", "iso"))
View(dat_wide)

# make data frame long
dat_long <- reshape(dat_wide, idvar = c("country", "iso"), varying = 3:104, direction = "long")

 

# ************************************************
# Pooled cross-sections example: -----------------
# Fertility across time in the US ----------------

data("fertil1")
?fertil1

summary(lm(kids ~ age + educ + black + age + I(age^2) + east + northcen + west + farm + othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84, data = fertil1))

# dealing with multiple dummies across many models can be cumbersome - here's an alternative way to construct your formula:
controls <- c("age", "educ", "black", "age", "I(age^2)", "east", "northcen", "west", "farm", "othrural", "town", "smcity")
year_dummies <- paste0("y", seq(74, 84, 2))
fmla <- paste("kids", paste(c(controls, year_dummies), collapse = " + "), sep = " ~ ")
summary(lm(as.formula(fmla), data = fertil1))

# questions:
  # where is y72?
  # which years have no significant difference from 1972?
  # did fertility change much over time? when?
  # why is age squared?
  # how would you model changes in an effect over time?



# ************************************************
# Diff-in-diff example: --------------------------
# Workers' compensation and injury duration ------

data("injury")
?injury

# inspect dependent variable
summary(injury$durat)
hist(injury$durat)
injury$ldurat <- log(injury$durat)
hist(injury$ldurat)

# visualize difference in both samples
dat <- filter(injury, afchnge == 0)
plot1 <- ggplot(dat, aes(x = ldurat)) + 
  geom_histogram(data = filter(dat, highearn == 0), fill = "red", alpha = 0.2) +
  geom_histogram(data = filter(dat, highearn == 1), fill = "blue", alpha = 0.2)

dat <- filter(injury, afchnge == 1)
plot2 <- ggplot(dat, aes(x = ldurat)) + 
  geom_histogram(data = filter(dat, highearn == 0), fill = "red", alpha = 0.2) +
  geom_histogram(data = filter(dat, highearn == 1), fill = "blue", alpha = 0.2)

grid.arrange(plot1, plot2, ncol=2)

# separate models by year
summary(lm(ldurat ~ highearn, data = filter(injury, afchnge == 0)))
summary(lm(ldurat ~ highearn, data = filter(injury, afchnge == 1)))

# diff-in-diff model
summary(lm(ldurat ~ highearn*afchnge, data = injury))



# ************************************************
# Differencing example: --------------------------
# Sleeping vs. working ---------------------------

data("slp75_81")
?slp75_81
dat <- slp75_81

dat$slpnap_d <- dat$slpnap81 - dat$slpnap75 # differencing the data - super easy in wide format!
plot(dat$slpnap_d, dat$cslpnap) # should be the same

# bring into long format (only necessary for first task - and to prove how cumbersome reshaping can be)
dat$age81 <- dat$age75 + 6
dat_wide <- dplyr::select(dat, male, age75, educ75, gdhlth75, marr75, slpnap75, totwrk75, yngkid75, age81, educ81, gdhlth81, marr81, slpnap81, totwrk81, yngkid81) %>% as.data.frame()
names(dat_wide)
dat_long <- reshape(dat_wide, varying = 2:15, v.names = c("age", "educ", "gdhlth", "marr", "slpnap", "totwrk", "yngkid"), idvar = "id_var", direction = "long")

# estimate model with pooled data, no differencing
summary(lm(slpnap ~ totwrk + educ + marr + yngkid + gdhlth, data = dat_long))

# first-difference model
summary(lm(cslpnap ~ ctotwrk + ceduc + cmarr + cyngkid + cgdhlth, data = dat))






# ************************************************
# Tidying data frames with tidyr -----------------

# Hadley Wickham's paper on tidy data:
browseURL("https://www.jstatsoft.org/article/view/v059i10")

# Wickham is proponent of "one column, one variable" paradigm
# corresponds to "long" format for panel data sets
# fit nicely with dplyr, ggplot and similar packages that operate on data frames
# tidyr essentially provides yet another set of functions to deal with reshaping data frames to fit these rules:
  # each variable must have its own column
  # each observation must have its own row
  # each value must have its own cell

# related to reshape2 and reshape, although tidyr is not designed for general reshaping
# gather() ~ melt() 
  # --> takes multiple columns and gathers them into key-value pairs 
  # --> wide to long
# spread() ~ cast() 
  # --> takes two columns (key and value) and spreads in to multiple columns 
  # --> long to wide

# additional functions:
# separate()
  # --> pull apart a column that represents multiple variables
# unite()
  # --> complement to separate()

# example: gather()
# demo("so-17481212")
race <- read.table(header = TRUE, check.names = FALSE, text = "
                   Name    50  100  150  200  250  300  350
                   Carla  1.2  1.8  2.2  2.3  3.0  2.5  1.8
                   Mace   1.5  1.1  1.9  2.0  3.6  3.0  2.5
                   Lea    1.7  1.6  2.3  2.7  2.6  2.2  2.6
                   Karen  1.3  1.7  1.9  2.2  3.2  1.5  1.9
                   ")
race
race_long <- gather(race, key = Time, value = Score, -Name, convert = TRUE) 
race_long
race_long %>% arrange(Name, Time)

# example: spread()
# demo("so-16032858")
results <- data.frame(
  Ind = paste0("Ind", 1:10),
  Treatment = rep(c("Treat", "Cont"), each = 10),
  value = 1:20
)
results
spread(results, key = Treatment, value = value)

# example: separate()
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x, c("A", "B"))
df %>% separate(x, c("A", "B"), sep = -2)
df %>% separate(x, c("A", "B"), sep = "\\.")

# example 2: separate() - every row doesn't split into the same number of pieces
df <- data.frame(x = c("a", "a b", "a b c", NA))
df %>% separate(x, c("a", "b"))
# the same behaviour but no warnings, fill with missing values on specified side
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
df %>% separate(x, c("a", "b"), extra = "drop", fill = "left")
# do not drop extra pieces, only splits at most length(into) times
df %>% separate(x, c("a", "b"), extra = "merge", fill = "right")
df %>% separate(x, c("a", "b", "c"), extra = "merge", fill = "right")

# example: separate_rows()
df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6"),
  stringsAsFactors = FALSE
)
df
separate_rows(df, y, z, convert = TRUE)

# example: unite()
df <- data.frame(
  country = rep(c("Afghan", "Brazil", "China"), each = 2),
  century = rep(c("19", "20"), 3),
  year = rep(c("99", "00"), 3),
  stringsAsFactors = FALSE
)
df
unite(df, century, year, col = "year", sep = "")
