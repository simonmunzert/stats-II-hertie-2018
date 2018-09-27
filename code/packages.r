
# install packages from CRAN
p_needed <- c("readr", # imports spreadsheet data
              "haven", # imports SPSS, Stata and SAS files
              "labelled", # helpers to work with variable labels
              "tidyr", # suite for tidying data
              "ggplot2", # advanced graphics suite
              "gridExtra", # more functionality for ggplot2 plot
              "stargazer", # nice formatting of regression table output
              "babynames", # US baby names provided by the SSA
              "nycflights13", # dataset on all 336776 flights departing from NYC in 2013
              "wooldridge", # datasets used in Wooldridge
              "car", # functions from "Compaion to Applied Regression"
              "lmtest", # functions for additional regression diagnostics (including RESET test),
              "broom", # tidying model output
              "interplot", # plot effects of interaction terms
              "margins", # calculate and visualize marginal effects
              "interflex", # interaction diagnostics and flexible estimation
              "xts", # working with time-series data
              "tseries", # functions for time series analysis
              "lubridate", # working with dates and times
              "plm", # panel data econometrics
              "pcse", # panel-corrected standard errors à la Beck and Katz (1995)
              "readxl", # import Excel data
              "ggthemes",
              "Zelig",
              "ISLR",
              "janitor",
              "dplyr",  # provides neat functions for data frame manipulation,
              "magrittr"
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
