# Load the dataset into the environment and neccessary libraries
library('stargazer')
library('ggplot2')
library('dplyr')

# Question 1
# Dataset summary: contains annaual information on beer sales, beer taxes, and cigarret taxes from 1981 - 2007 in US (47 states)
beer_data = read.csv("D:/Semester 2 2022/ECOM20001/Assignment 1/as1_beer.csv")

# Report summary statistic
tax_st = stargazer(beer_data,
                   summary.stat = c('n', 'mean', 'sd', 'median', 'min', 'max'),
                   type = 'text', title = 'Descriptive Statistic for Beer Dataset',
                   out = 'beer_sumstat.txt')

# Question 2
# Compute sample means for each field
beercons_mean = mean(beer_data$beercons)
beertax_mean = mean(beer_data$beertax)
cigtax_mean = mean(beer_data$cigtax)

# Compute sample standard deviation for each field
beercons_sd = sd(beer_data$beercons)
beertax_sd = sd(beer_data$beertax)
cigtax_sd = sd(beer_data$cigtax)

# Compute 95% Confidence Interval for each field 

# Beercons
beercons_95_low = beercons_mean - 1.96 * beercons_sd
beercons_95_high = beercons_mean + 1.96 * beercons_sd

# Beertax
beertax_95_low = beertax_mean - 1.96 * beertax_sd
beertax_95_high = beertax_mean + 1.96 * beertax_sd

# Cigtax
cigtax_95_low = cigtax_mean - 1.96 * cigtax_sd
cigtax_95_high = cigtax_mean + 1.96 * cigtax_sd


# Question 3

beer_data2 = beer_data %>%
  mutate(hightax = if_else(beertax >= median(beertax),
                           1,
                           0))
beer_plot = ggplot(beer_data2, aes(x = beercons, fill = as.character(hightax))) +
  geom_density(alpha = 0.25)

beer_plot + labs(title = "Density Plot for Beer Sales",
                 x = "Beer sales (gallons per capita)",
                 y = "Density",
                 fill = "Hightax value") +
            theme_bw() +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                  axis.title = element_text(face = "italic", size = 10))

# Question 4

# Compute mean for beercons if hightax = 1 and hightax = 0

beercons_ht1_mean = mean(beer_data2$beercons[beer_data2$hightax == 1])
beercons_ht0_mean = mean(beer_data2$beercons[beer_data2$hightax == 0])

beercons_ht1_sd = sd(beer_data2$beercons[beer_data2$hightax == 1])

# Conduct hypothesis testing:
t_stat = (beercons_ht1_mean - beercons_ht0_mean)/beercons_ht1_sd
p_value = 2 * pnorm(-abs(t_stat))


















