# Load the dataset into the environment and neccessary libraries
library('stargazer')
library('ggplot2')
library('dplyr')
library('AER')

# Load the dataset firstly
setwd("D:/Semester 2 2022/ECOM20001/Assignment 2")
sleepData <- read.csv("D:/Semester 2 2022/ECOM20001/Assignment 2/as2_sleep.csv")
View(sleepData) # View dataset
dim(sleepData) # get number of observations and variables

# Question 1: Summary statistic for the dataset.
sleepStat = stargazer(sleepData,
                   summary.stat = c('n', 'mean', 'sd', 'median', 'min', 'max'),
                   type = 'text', title = 'Descriptive Statistic for Sleep Dataset',
                   out = 'sleepStat.png')
# Question 2: Constructing scatter plots

#sleep vs educ
scatPlot1 = ggplot(sleepData, aes(educ, sleep)) + geom_point(colour = 'orange') +
  labs(title = "Scatter Plot for Sleep Against Education",
       y = "Sleep duration (minutes/week)",
       x = "Education attainment (years)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "italic", size = 10)) +
  geom_smooth(method = lm, se = FALSE, colour = "blue")
scatPlot1

# sleep vs age
scatPlot2 = ggplot(sleepData, aes(age, sleep)) + geom_point(colour = 'orange') +
  labs(title = "Scatter Plot for Sleep Against Age",
       y = "Sleep duration (minutes/week)",
       x = "Age (years)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "italic", size = 10)) +
  geom_smooth(method = lm, se = FALSE, colour = "blue")
scatPlot2

# educ vs age
scatPlot3 = ggplot(sleepData, aes(age, educ)) + geom_point(colour = 'orange') +
  labs(title = "Scatter Plot for Years of Education Attainment Against Age",
       y = "Education attainment (years)",
       x = "Age (years)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "italic", size = 10)) +
  geom_smooth(method = lm, se = FALSE, colour = "blue")
scatPlot3

#Question 3:

# Sign for sleep against educ: -
# Sign for age and educ : -
# Direction of bias = sign(-) x sign(-) = +
# The coefficient is larger than the actual value since there is a positive bias?

# Question 4:

#Reg 1: sleep ~ educ
reg1 = lm(sleep~educ, sleepData)
cov1 = vcovHC(reg1, type = "HC1") # accounting for heteroskedasticity 
se1 = sqrt(diag(cov1))

#Reg 2: sleep ~ educ + age
reg2 = lm(sleep~educ + age, sleepData)
cov2 = vcovHC(reg2, type = "HC1");
se2 = sqrt(diag(cov2))

#Reg 3: sleep ~ educ + age + gdhlth
reg3 = lm (sleep~educ + age + gdhlth, sleepData)
cov3 = vcovHC(reg3, type = "HC1")
se3 = sqrt(diag(cov3))

#Reg 4: sleep ~ educ + age + gdhlth + smsa + union + selfe
reg4 = lm(sleep~educ + age + gdhlth + smsa + union + selfe, sleepData)
cov4 = vcovHC(reg4, type = "HC1")
se4 = sqrt(diag(cov4))

#Reg 5: sleep ~ educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid
reg5 = lm(sleep~educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid, sleepData)
cov5 = vcovHC(reg5, type = "HC1")
se5 = sqrt(diag(cov5))

# Build the regression model
stargazer(reg1, reg2, reg3, reg4, reg5, type="text",
          se=list(se1, se2, se3, se4, se5),
          digits=2, 
          dep.var.labels=c("Number of minutes sleep per week"),
          covariate.labels=
            c("Number of years of education attainment",
              "Age",
              "Self-reported Health",
              "Live in US urban area",
              "Part of a union",
              "Self-employed",
              "Marrital status",
              "Number of years Married",
              "Have children that is less than 3 years old"),
          out="q4_output.txt")
# Question 5:
# 5A
# The inclusion of the ommitted variable age has make the coefficient for educ get less negative (a decrease in magnitude)
# This reflects the finding in question 3 where there is a positive bias
# The coef on educ now move closer to the actual value

#5B
# Another change is due to health condition is also another ommitted variable
tStat5B = (-11.90 - 0)/5.77
pValue = 2 * pnorm(-abs(tStat5B))
pValue
# less than 0.05. Hence, it's statistically significantly diff from 0.

#5C
coefDiff5C = -11.90 - (-9.95)
seDiff5C = sqrt((5.77**2 + 5.84**2))
tStat5C = (coefDiff5C - 0)/seDiff5C
pValue5C = 2 * pnorm(-abs(tStat5C))
pValue5C
# much greater than 0.05. Hence, we failed to reject null hypothesis. It is statiscally significantly indifferent from 0.

#5D

#5E
# using waldtest()
waldtest(reg5, vcov = vcovHC(reg5, 'HC1'))

# F-stat: 2.3055 ; df = 9; p-val = 0.01482 ( < 0.05)
# 

# Question 6:
q6Reg1 = reg5
q6Cov1 = cov5
q6se1 = se5

q6Reg2 = lm(nap~educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid, sleepData)
q6Cov2 = vcovHC(q6Reg2, type = "HC1")
q6se2 = sqrt(diag(cov5))

# Build the regression table
stargazer(q6Reg1, q6Reg2, type="text",
          se=list(q6se1, q6se2),
          digits=2, 
          dep.var.labels=c("Number of minutes sleep per week", "Number of minutes napping per week"),
          covariate.labels=
            c("Number of years of education attainment",
              "Age",
              "Self-reported Health",
              "Live in US urban area",
              "Part of a union",
              "Self-employed",
              "Marrital status",
              "Number of years Married",
              "Have children that is less than 3 years old"),
          out="q6_output.txt")

coefDifq6 = -9.95 - (-4.85)
seDifq6 = sqrt(5.84**2 + 5.84**2)
tStatq6 = (coefDifq6 - 0)/seDifq6
pValueq6 = 2 * pnorm(-abs(tStatq6))
pValueq6

# p-value > 0.1; hence we failed to reject the null hypothesis. Their difference is statistically significantly
# indifferent from 0.

# Question 8:

q7Reg1 = lm(sleep ~ totwrk + educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid, sleepData)
q7cov1 = vcovHC(q7Reg1, type = 'HC1')
q7se1 = sqrt(diag(q7cov1))

q7Reg2 = lm(sleep ~ totwrk + educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid, sleepData[sleepData$male == 1,])
q7cov2 = vcovHC(q7Reg2, type = 'HC1')
q7se2 = sqrt(diag(q7cov2))

q7Reg3 = lm(sleep ~ totwrk + educ + age + gdhlth + smsa + union + selfe + marr + yrsmarr + yngkid, sleepData[sleepData$male == 0,])
q7cov3 = vcovHC(q7Reg3, type = 'HC1')
q7se3 = sqrt(diag(q7cov3))

stargazer(q7Reg1, q7Reg2, q7Reg3, type = 'text',
          se = list(q7se1, q7se2, q7se3),
          digits = 2,
          dep.var.labels=c("Number of minutes sleep per week"),
          column.labels = c("Full Dataset", "Male", "Female"),
          covariate.labels=
            c("Number of minutes working per week", 
              "Number of years of education attainment",
              "Age",
              "Self reported health is \"Excellent\" or \"Good\"",
              "Lives in a US urban area",
              "Part of a union",
              "Self-employed",
              "Marrital status",
              "Number of years married",
              "Have a child less than 3 at home"),
          out="q7_output.txt")

# Question 8

# Conducting t-test for Reg1 on totwrk
tStatq8 = (-0.13 - 0)/0.02
pValueq8 = 2 * pnorm(-abs(tStatq8))
pValueq8

# p-value less than 0.05, reject null hypothesis. It is statistically significantly different from 0.
# a increase in 5 hours = 300 minutes of working  results in a decrease of 39 minutes for sleeping.

#8B
linearHypothesis(q7Reg1, c("marr = yngkid"), vcov = vcovHC(q7Reg1, "HC1"))

#F-stat = 1.1792. p-value = 0.2779. AT 5% significance level, we fail to reject the null hypothesis.
# The coefficient for marr and yngkid are statistically significantly indifferent

#8C



