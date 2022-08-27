# Load the dataset into the environment and neccessary libraries
library('stargazer')
library('ggplot2')

# Dataset summary: contains annaual information on beer sales, beer taxes, and cigarret taxes from 1981 - 2007 in US (47 states)
beer_data = read.csv("D:/Semester 2 2022/ECOM20001/Assignment 1/as1_beer.csv")

# Report summary statistic
tax_st = stargazer(beer_data,
                   summary.stat = c('n', 'mean', 'sd', 'median', 'min', 'max'),
                   type = 'text', title = 'Descriptive Statistic for Beer Dataset',
                   out = 'beer_sumstat.txt')
# Compute standard deviation for each field
beercons_sd = sd(beer_data$beercons)
beertax_sd = sd(beer_data$beertax)
cigtax_sd = sd(beer_data$cigtax)

#Change from Rstudio