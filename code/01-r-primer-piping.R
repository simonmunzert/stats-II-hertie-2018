# ************************************************
### simon munzert
### introduction to R
# ************************************************

source("packages.r")
source("functions.r")

## piping with magrittr ----------

# what is piping?
  # structures sequences of data operations as "pipes, i.e. left-to-right (as opposed to from the inside and out)
  # serves the natural way of reading ("do this, then this, then this, ...")
  # avoids nested function calls
  # improves "cognitive performance" of code writers and readers
  # minimizes the need for local variables and function definitions
  # why name "magrittr"?
browseURL("https://upload.wikimedia.org/wikipedia/en/b/b9/MagrittePipe.jpg")


# basic example  -----------------
# [source: https://goo.gl/sWz2DH]

# traditional way of writing code
dat <- babynames 
dim(dat)
dat_filtered <- filter(dat, name == "Kim")
dat_grouped <- group_by(dat_filtered, year, sex)
dat_sum <- summarize(dat_grouped, total = sum(n))
qplot(year, total, color = sex, data = dat_sum, geom = "line") +
  ggtitle('People named "Kim"')

# traditional, even more awkward way of writing code
dat <- summarize(group_by(filter(babynames, name == "Kim"), year, sex), total = sum(n))
qplot(year, total, color = sex, data = dat, geom = "line") +  ggtitle('People named "Kim"')

# magrittr style of piping code
babynames %>%
  filter(name %>% equals("Kim")) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>%
  qplot(year, total, color = sex, data = ., geom = "line") %>%
  add(ggtitle('People named "Kim"')) %>%
  print


# syntax and vocabulary -------------------------
# [source: https://goo.gl/SKnPn7]

# by default, the left-hand side (LHS) will be piped in as the first argument of the function appearing on the right-hand side (RHS)
# %>% may be used in a nested fashion, e.g. it may appear in expressions within arguments. This is used in the mpg to kpl conversion
# when the LHS is needed at a position other than the first, one can use the dot,'.', as placeholder
# whenever only one argument is needed--the LHS--, the parentheses can be omitted



# overview of available aliases
?extract

babynames[1:2,]
babynames %>% .[1:2,]
babynames %>% extract(,1:2)



# when rather not to use the pipe-----
# [source: https://goo.gl/EZ4La8]

# when your pipes are really long (Hadley Wickham suggests > 10 steps); because then code becomes difficult to read again. creating intermediate objects might help
# when you have multiple inputs or outputs



# one more thing ---------------------

# there is a rival pipelining package on the block, 'pipeR'
# the main differences are highlighted here:
browseURL("https://renkun.me/blog/2014/08/08/difference-between-magrittr-and-pipeR.html")
# the author tried to implement even more intuitive and predictable parsing ruls for the piping operator, especially to avoid ambiguities in piping to the first argument or not when dots are used
# for large-scale operations, pipeR might work faster than magrittr

