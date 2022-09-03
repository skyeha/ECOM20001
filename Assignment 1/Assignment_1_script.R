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
                   out = 'beer_sumstat.png')

# Question 2
# Compute sample means for each variable
beercons_mean = mean(beer_data$beercons)
beertax_mean = mean(beer_data$beertax)
cigtax_mean = mean(beer_data$cigtax)

# Compute sample standard error for each variable
beercons_se = sd(beer_data$beercons)/sqrt(length(beer_data$beercons))
beertax_se = sd(beer_data$beertax)/sqrt(length(beer_data$beertax))
cigtax_se = sd(beer_data$cigtax)/sqrt(length(beer_data$cigtax))

# Compute 95% Confidence Interval for each variable 

# Beercons
beercons_95_low = beercons_mean - 1.96 * beercons_se
beercons_95_high = beercons_mean + 1.96 * beercons_se

# Beertax
beertax_95_low = beertax_mean - 1.96 * beertax_se
beertax_95_high = beertax_mean + 1.96 * beertax_se

# Cigtax
cigtax_95_low = cigtax_mean - 1.96 * cigtax_se
cigtax_95_high = cigtax_mean + 1.96 * cigtax_se


# Question 3

# adding a column of hightax straight into the dataset using dplyr.
beer_data2 = beer_data %>%
  mutate(hightax = if_else(beertax >= median(beertax),
                           1,
                           0))
beer_density = ggplot(beer_data2, aes(x = beercons, fill = as.character(hightax))) +
  geom_density(alpha = 0.25)

beer_density + labs(title = "Density Plot for Beer Sales",
                 x = "Beer sales (gallons per capita)",
                 y = "Density",
                 fill = "") +
            theme_bw() +
            scale_fill_manual(labels = c("Beer tax < $0.440", "Beer tax >= $0.440"), values = c("Blue", "Red")) +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                  axis.title = element_text(face = "italic", size = 10))
beer_density

# Question 4

# Compute mean for beercons if hightax = 1 and hightax = 0
beercons_ht1_mean = mean(beer_data2$beercons[beer_data2$hightax == 1])
beercons_ht0_mean = mean(beer_data2$beercons[beer_data2$hightax == 0])

# Compute standard deviation and standard errors
beercons_ht1_sd = sd(beer_data2$beercons[beer_data2$hightax == 1])
beercons_ht0_sd = sd(beer_data2$beercons[beer_data2$hightax == 0])

beercons_ht1_se = beercons_ht1_sd/sqrt(length(beer_data2$beercons[beer_data2$hightax == 1]))
beercons_ht0_se = beercons_ht0_sd/sqrt(length(beer_data2$beercons[beer_data2$hightax == 0]))

mean_diff = beercons_ht1_mean - beercons_ht0_mean
se_diff = sqrt((beercons_ht1_se**2 + beercons_ht0_se**2))

# Conduct hypothesis testing:
t_stat = (mean_diff - 0)/se_diff
p_value = 2 * pnorm(-abs(t_stat)) 

# 95% CI for mean diff
mean_diff_upper = mean_diff + 1.96 * se_diff
mean_diff_lower = mean_diff - 1.96 * se_diff

diff_percentage = (abs(mean_diff)/beercons_ht0_mean) * 100 

# There is a decrease of approximately 4% when hightax went from 0 to 1.

# Question 5

#scatter plot of beertax against beercons
beer_scatter = ggplot(beer_data2, aes(beertax, beercons)) + geom_point(colour = 'orange')
beer_scatter + labs(title = "Scatter Plot for Beer Tax Against Beer Sales",
                  x = "Beer tax (dollars per gallon)",
                  y = "Beer sales (gallons per capita)") +
                theme_bw() +
                theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                      axis.title = element_text(face = "italic", size = 10)) +
                geom_smooth(method = lm, se = FALSE, colour = "blue")


cor_beertax_cons = cor(beer_data2$beertax, beer_data2$beercons)

# Question 6

# Single linear regression for beertax and beercons
reg_1 = lm(beercons~beertax, beer_data2)
summary(reg_1)

# Single linear regressions for cigtax and beercons, 
reg_2 = lm(beercons~cigtax, beer_data2)
summary(reg_2)

# standard deviation for beertax and cigtax
beertax_sd = sd(beer_data2$beertax)
cigtax_sd = sd(beer_data2$cigtax)

# Increase of 1 standard-deviation in beertax
change_1 = -0.30387 * beertax_sd
change_1

# increase of 1-standard-deviation in cigtax
change_2 = -0.0012506 * cigtax_sd
change_2

# Question 7

# scatter plot for beercons and cigtax

cons_cig_scatter = ggplot(beer_data2, aes(cigtax, beercons)) + geom_point(colour = 'orange')
cons_cig_scatter = cons_cig_scatter + labs(title = "Scatter Plot for Cigarrette Tax Against Beer Sales",
                        y = "Beer sales (dollars per gallon)",
                        x = "Ciggarette tax (dollars per pack)") +
                    theme_bw() +
                    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                          axis.title = element_text(face = "italic", size = 10)) +
                    geom_smooth(method = lm, se = FALSE, colour = "blue")
cons_cig_scatter

# scatter plot for beertax and cigtax

beer_cig_scatter = ggplot(beer_data2, aes(beertax, cigtax)) + geom_point(colour = "orange")
beer_cig_scatter = beer_cig_scatter + labs(title = "Scatter Plot for Beer Tax Against Cigarrette Tax",
                        x = "Beer tax (dollars per gallon)",
                        y = "Cigarrette tax (dollars per pack)") +
                    theme_bw() +
                    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                          axis.title = element_text(face = "italic", size = 10)) +
                    geom_smooth(method = lm, se = FALSE, colour = "blue")

beer_cig_scatter

# QUestion 8
reg_before_1994 = lm(beercons[year <= 1994]~beertax[year <= 1994], beer_data2)
summary(reg_before_1994)

reg_after_1994 = lm(beercons[year > 1994]~beertax[year > 1994], beer_data2)
summary(reg_after_1994)

# Hypothesis testing for difference in beertax coefficients

coef_diff = -0.43945 - (-0.06844)
se_diff2 = sqrt(0.06201**2 + 0.09079**2)

t_stat2 = (coef_diff - 0)/se_diff2
p_value2 = 2*pnorm(-abs(t_stat2)) # = 0.000739545

# since p value is less than 0.05, we reject the null hypothesis and there is statistically significant difference
# between the coefficients of the two sets.