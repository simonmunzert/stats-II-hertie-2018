# ************************************************
### simon munzert
### introduction to R
# ************************************************

source("packages.r")
source("functions.r")


# ************************************************
# DEALING WITH VECTORS -----------------------

# numeric vectors
x <- c(4,8,15,16,23,42)
x
class(x)
length(x)
summary(x)

# character vectors
countries <- c("Germany", "France", "Netherlands", "Belgium")
countries
paste(countries, collapse = " ")
paste("Hello", "world!", sep = " ")
paste0("Hello", "world!")
c(countries, "Poland")
class(countries)

# logical vectors
x > 15
x == sqrt(225)

# logical and relational operators
# <,>,>=,<=,==,!=, is.na(), & (logical AND), | (logical OR), ! (logical NOT)

# missing values
y <- c(1,10,NA,7,NA,11)
sum(y)
sum(y, na.rm = TRUE)
!is.na(y) # not: y == NA
y*3

# seq and rep
seq(1, 10, 3)
rep(c(1, 2, 3), 2)
rep(c(1, 2, 3), each = 2)
seq_along(x)

# sorting
vec1 <- c(2, 20, -5, 1, 200)
vec2 <- seq(1, 5)
sort(vec1, decreasing = FALSE) 
order(vec1, decreasing = FALSE)
vec2[order(vec1)]
vec3 <- c(1,10,NA,7,NA,11)
vec4 <- vec3[!is.na(vec3)]
vec4

# vectors with mixed element types are not possible
z <- c(1,2,"Bavaria", 4)
z
str(z)

# variables
zz <- c(1,2,Bavaria,4,5,6) # error
Bavaria <- 3
zz <- c(1,2,Bavaria,4,5,6)
zz
str(zz)

# transform vector type
zzchar <- as.character(zz)
zznum <- as.numeric(zzchar)

# combine vectors
xzz <- c(x,zz)

# subsetting
countries[2]
xzz[1:6] # works, too: xzz[seq(1,6)], xzz[c(1,2,3,4,5,6)]
xzz[c(2, 5, 10)]
xzz[-1]
xzz[Bavaria]
xzz[seq(0, 10, by = 2)]
xzz[c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)]
y
y[is.na(y)]
y[!is.na(y)]
y[y > 5 & !is.na(y)]

countries
countries[3] <- "Switzerland"
countries

xzz
xzz[c(1 ,3 ,5 )] <- c(100,110,120)
xzz_new <- xzz
xzz_new[xzz <= 100] <- 0
xzz_new[xzz > 100] <- 1
xzz_new
ifelse(xzz > 100, 1, 0)


