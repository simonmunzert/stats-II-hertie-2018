# ************************************************
### simon munzert
### R exercises
# ************************************************

source("packages.r")
source("functions.r")


### practice at home using Swirl -----------------

# check out the Swirl project - it's fantastic!
browseURL("http://swirlstats.com/")

# install Swirl
install.packages("swirl")
library(swirl)

# launch Swirl
swirl()

simpleCondition()# install a course, for instance
install_course("R Programming")
#install_course("Regression Models")
install_course("Getting and Cleaning Data")

# check out which courses are available
browseURL("https://github.com/swirldev/swirl_courses")

# | When you are at the R prompt (>):
# | -- Typing skip() allows you to skip the current question.
# | -- Typing play() lets you experiment with R on your own; swirl will ignore what you do
# | -- ... UNTIL you type nxt() which will regain swirl's attention.
# | -- Typing bye() causes swirl to exit. Your progress will be saved.
# | -- Typing main() returns you to swirl's main menu.
# | -- Typing info() displays these options again.



### some R exercises --------------------------


## Exercise 1

# a) Consider the following vector. What's the mean of all values?
# b) And what's the median? What about the variance and the standard deviation?
# c) Compute the sum of all values.

x <- c(34, 56, 55, 87, NA, 4, 77, NA, 21, NA, 39)
x[!is.na(x)]
mean(x, na.rm = TRUE)
median(x, na.rm = TRUE)
var(x, na.rm = TRUE)
sd(x, na.rm = TRUE)
sum(x, na.rm = TRUE)


## Exercise 2

# a) Create a vector with 5 of your friends' names.
# b) Get the length of the vector.
# c) Extract the first two friends of the vector.
# d) Extract the second and the last observation from the vector.
# e) Sort your friends by their names.
# f) Reverse the direction of the above sort.

friends <- c("Homer", "Marge", "Bart", "Lisa", "Maggie")
length(friends)
friends[1:2]
friends[c(1, 5)]
friends[c(1, length(friends))]
sort(friends)
rev(sort(friends))
sort(friends, decreasing = TRUE)


## Exercise 3

# a) Here are some variables in vector format. Combine these by creating a data frame!
# b) Add a new variable, height, with the following values: 182, 170, 163, 175, 165, 150
# c) Visualize the relationship between age and height!

age <- c(22, 25, 18, 20, 32, 15)
name <- c("James", "Matthew", "Bill", "Lucie", "Olivia", "Stella")
gender <- c("M", "M", "M", "F", "F", "F")


## Exercise 4

# a) Calculate the root of 729.
# b) Draw a random sample of n = 50 of the sample 1:100 with replacement.
# c) Draw 20 values from the Uniform distribution on the interval from 0 to 10.


## Exercise 5

# a) Import the dataset "btw17_kerg.csv", which contains district-level results for the 2017 German federal election!
# b) Generate a set of new variables, cdsu1, cdsu1_last, cdsu2, cdsu2_last, which combines the votes for CDU and CSU!
# b) Generate the shares of the second vote for all major parties and compare them with the published results!
# c) Identify the district(s) in which the SPD improved its share of the vote!
# d) Generate a new variable that indicates the district winning party (i.e. the party with the most first votes!)
# e) Build a bar chart that visualizes the vote shares for all parties! The following link might help: http://www.statmethods.net/graphs/bar.html

dat <- read_tsv("../data/btw17_kerg.csv")
dat[is.na(dat)] <- 0
dat <- mutate(dat, cdsu1 = cdu1 + csu1,
                   cdsu1_last = cdu1_last + csu1_last,
              )



## Exercise 6

# a) Load the following dataset into R, which contains data on immigration to Ellis Island (1892-1924) by trip.
# b) What is the total number of immigration cases in the dataset?
# c) From where did the ship with the most immigrants arrive?
# d) How many unique ships (according to the name) are in the dataset?
# e) Which ship carried the most immigrants in total?
# f) Create a date variable using the lubridate package. 
# g) Aggregate the count variable by date (daily level) and generate a time series plot using these data.

url <- "https://raw.githubusercontent.com/hopperrr/ellis-immigration-by-ship/gh-pages/data/trips.tsv"




