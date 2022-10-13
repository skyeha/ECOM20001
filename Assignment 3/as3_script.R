#Set the work directory and initialise libraries
setwd("D:/Semester 2 2022/ECOM20001/Assignment 3")
library('stargazer')
library('ggplot2')
library('dplyr')
library('AER')

#Load dataset
wineData = read.csv("as3_wine.csv")

#### Question 1
#Scatter plot between price and score
q1Scat = ggplot(wineData, aes(score, price)) + geom_point(colour = 'orange') +
  labs(title = "Relationship Between Price of a Wine Bottle Against Taste Score",
       y = "Price ($/bottle)",
       x = "Scores") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "italic", size = 10)) +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), col = "blue")
  #geom_smooth(method = lm, formula = y ~ poly(x,3), se = FALSE, colour = "blue")
q1Scat

# The relationship appear to be non-linear and looks more like a quadratic relationship between price and score.
# Visually, there seems to be no inflection point in the data.

#======================================================================#

### Question 2
#Create non-linear terms for polynomial regressions
wineData$score3 = (wineData$score)**3
wineData$score2 = (wineData$score)**2

regCubic = lm(price~score + score2 + score3, data = wineData)
covCubic = vcovHC(regCubic, type = "HC1")
seCubic = sqrt(diag(covCubic))

regQuadra = lm(price~score + score2, data = wineData)
covQuadra = vcovHC(regQuadra, type = "HC1")
seQuadra = sqrt(diag(covQuadra))

regLin = lm(price~score, data = wineData)
covLin = vcovHC(regLin, type = "HC1")
seLin = sqrt(diag(covLin))

stargazer(regCubic, regQuadra, regLin, type = 'text',
          se=list(seCubic, seQuadra, seLin),
          digits = 2,
          dep.var.labels = c("Price in dollars per bottle of wine"),
          covariate.labels = c(
            'Tasting score',
            'Tasting score squared',
            'Tasting score cubed',
            'Constant'
          ),
          out = "q2_reg.txt")

coeftest(regCubic, vcov = covCubic)

# Testing for coefficient on score cubed. Null hypothesis is that B3 = 0. Since p-value is > 0.1
# => we accept the null hypothesis that score cubed has no explanation for the variation in price.

# Hence, we move on to testing for coefficient on score squared. Null hypothesis is that B2 = 0 and alt. hypothesis 
# is B2 != 0. Since p-value < 0.01. At the 5% significance level, we reject the null hypothesis that score squared
# has no explanation for the variation price.

# We do not move on to the next order and stop

#======================================================================#

### Question 3

reg1 = regQuadra
cov1 = covQuadra
se1 = seQuadra

reg2 = lm(price~score + score2 + pinotnoir + cabernet + merlot + syrah, data = wineData)
cov2 = vcovHC(reg2, type = "HC1")
se2 = sqrt(diag(cov2))

reg3 = lm(price~score + score2 + pinotnoir + cabernet + merlot + syrah +
            napa + bayarea + sonoma + scoast + carn + sierra + mendo + wash +
            d1991 + d1992 + d1993 + d1994 + d1995 + d1996 + d1997 + d1998 + d1999, data = wineData)
cov3 = vcovHC(reg3, type = "HC1")
se3 = sqrt(diag(cov3))

stargazer(reg1, reg2, reg3, type = "text",
          digits = 3,
          se = list(se1, se2, se3),
          omit = c('napa','bayarea','sonoma','scoast','carn','sierra','mendo','wash','othloc','d1990',
                   'd1991','d1992','d1993','d1994','d1995','d1996','d1997','d1998','d1999'),
          notes.label = 'The dummy variables for region and years are controlled for, though not visible',
          out = "q3_reg.txt")



#======================================================================#

### Question 4

coeftest(reg3, vcov = vcovHC(reg3, "HC1"))
# We conduct a t-test whether coef for score squared.
#h0: score square coef = 0 ; h1: score squared coef != 0
#t-stat = 16.4788 and p-value < 2.2e-16 < 0.05
# Hence, at the 5% significance level, we reject the null hypothesis that there is no linear relationship between
# price and score.
# It's sampling distribution assuming a large random sample is a normal distribution.

#======================================================================#

### Question 5
# Use F-test
linearHypothesis(reg3, c("pinotnoir = cabernet","cabernet = merlot", 
                      "merlot= syrah"), vcov = vcovHC(reg3, "HC1"))


# null hypothesis: pinotnoir, carbernet, merlot, and syrah has the same effect on wine price
# alt hypothesis: these wines types have different effects on wine price
# F-stat = 115.91 and p-value < 0.001.
# At the 5% significance level, we reject the null hypothesis. That is we reject the fact that these 4 wine types
# has the same impact.
# It's sampling distribution with an assumptions of a large random sample is a normal distribution

#======================================================================#

### Question 6
## From 80 -> 85
## Model 1 ( Reg 1)

model1A = data.frame(score = 80, score2 = 80**2)
model1B = data.frame(score = 85, score2 = 85**2)

price1A = predict(reg1, newdata = model1A)
price1B = predict(reg1, newdata = model1B)

model1Effect = price1B - price1A
model1Effect

FtestModel1 = linearHypothesis(reg1, c("5*score + 825*score2=0"), vcov = cov1)
FstatModel1 = FtestModel1[2,3]

seModel1 = abs(model1Effect)/sqrt(FstatModel1)
Model1CI95H = model1Effect + 1.96*seModel1
Model1CI95L = model1Effect - 1.96*seModel1


# partial effects = $7.411
# standard error = $0.326
# 95% CI = [6.773, 8.049]

# Model 2
model2A = data.frame(score = 80, score2 = 80**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0)
model2B = data.frame(score = 85, score2 = 85**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0)
price2A = predict(reg2, newdata = model2A)
price2B = predict(reg2, newdata = model2B)

model2Effect = price2B - price2A
model2Effect

FtestModel2 = linearHypothesis(reg2, c("5*score + 825*score2=0"), vcov = cov2)
FtestModel2
FstatModel2 = FtestModel2[2,3]

seModel2 = abs(model2Effect)/sqrt(FstatModel2)
Model2CI95H = model2Effect + 1.96*seModel2
Model2CI95L = model2Effect - 1.96*seModel2

# partial effects = $7.395
# standard error = $0.328
# 95% CI = [6.753, 8.038]

#Model 3
model3A = data.frame(score = 80, score2 = 80**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0,
                     napa = 0, bayarea = 0, sonoma = 0, scoast = 0, carn = 0, sierra = 0, mendo = 0, wash = 0,
                     d1991 = 0, d1992 = 0, d1993 = 0, d1994 = 0, d1995 = 0, d1996 = 0, d1997 = 0, d1998 = 0, d1999 = 0)
model3B = data.frame(score = 85, score2 = 85**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0,
                     napa = 0, bayarea = 0, sonoma = 0, scoast = 0, carn = 0, sierra = 0, mendo = 0, wash = 0,
                     d1991 = 0, d1992 = 0, d1993 = 0, d1994 = 0, d1995 = 0, d1996 = 0, d1997 = 0, d1998 = 0, d1999 = 0)
price3A = predict(reg3, newdata = model3A)
price3B = predict(reg3, newdata = model3B)

model3Effect = price3B - price3A
model3Effect

FtestModel3 = linearHypothesis(reg3, c("5*score + 825*score2=0"), vcov = cov3)
FstatModel3 = FtestModel2[2,3]

seModel3 = abs(model3Effect)/sqrt(FstatModel3)
Model3CI95H = model3Effect + 1.96*seModel3
Model3CI95L = model3Effect - 1.96*seModel3

model3Effect
seModel3
Model3CI95H
Model3CI95L

# partial effect = $4.847
# standard error = 0.215
# 95% CI = [4.425, 5.268]

#=================================#

## From 85 -> 90
# Model 1
model1A = data.frame(score = 85, score2 = 85**2)
model1B = data.frame(score = 90, score2 = 90**2)

price1A = predict(reg1, newdata = model1A)
price1B = predict(reg1, newdata = model1B)

model1Effect = price1B - price1A
model1Effect

FtestModel1 = linearHypothesis(reg1, c("5*score + 875*score2=0"), vcov = cov1)
FstatModel1 = FtestModel1[2,3]

seModel1 = abs(model1Effect)/sqrt(FstatModel1)
Model1CI95H = model1Effect + 1.96*seModel1
Model1CI95L = model1Effect - 1.96*seModel1


# partial effects = $17.930
# standard error = $0.545
# 95% CI = [16.8629, 18.997]

# Model 2
model2A = data.frame(score = 85, score2 = 85**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0)
model2B = data.frame(score = 90, score2 = 90**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0)
price2A = predict(reg2, newdata = model2A)
price2B = predict(reg2, newdata = model2B)

model2Effect = price2B - price2A
model2Effect

FtestModel2 = linearHypothesis(reg2, c("5*score + 875*score2=0"), vcov = cov2)
FtestModel2
FstatModel2 = FtestModel2[2,3]

seModel2 = abs(model2Effect)/sqrt(FstatModel2)
Model2CI95H = model2Effect + 1.96 * seModel2
Model2CI95L = model2Effect - 1.96 * seModel2

# partial effects = $17.289
# standard error = $0.535
# 95% CI = [16.241, 18.338]

# Model 3
model3A = data.frame(score = 85, score2 = 85**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0,
                     napa = 0, bayarea = 0, sonoma = 0, scoast = 0, carn = 0, sierra = 0, mendo = 0, wash = 0,
                     d1991 = 0, d1992 = 0, d1993 = 0, d1994 = 0, d1995 = 0, d1996 = 0, d1997 = 0, d1998 = 0, d1999 = 0)
model3B = data.frame(score = 90, score2 = 90**2, pinotnoir = 0, cabernet = 0, merlot = 0, syrah = 0,
                     napa = 0, bayarea = 0, sonoma = 0, scoast = 0, carn = 0, sierra = 0, mendo = 0, wash = 0,
                     d1991 = 0, d1992 = 0, d1993 = 0, d1994 = 0, d1995 = 0, d1996 = 0, d1997 = 0, d1998 = 0, d1999 = 0)
price3A = predict(reg3, newdata = model3A)
price3B = predict(reg3, newdata = model3B)

model3Effect = price3B - price3A
model3Effect

FtestModel3 = linearHypothesis(reg3, c("5*score + 875*score2=0"), vcov = cov3)
FstatModel3 = FtestModel2[2,3]

seModel3 = abs(model3Effect)/sqrt(FstatModel3)
Model3CI95H = model3Effect + 1.96*seModel3
Model3CI95L = model3Effect - 1.96*seModel3

model3Effect
seModel3
Model3CI95H
Model3CI95L
# partial effect = $15.271
# standard error = 0.472
# 95% CI = [14.345, 16.197]

## Conclusion
# The findings reflects that result from previous question that the model is indeed quadratic.
# Going from score of 80 -> 85, we see a normal change which is represent by the line in the scatter plot.
# Going from score of 85->90, however, we see a change that is more than double than that of the change going from
# 80->85. This is due to the fact that the slope when going from 85->90 is much steeper than the slop going from 80->85.
# negative slope ?
#======================================================================#

### Question 7

wineData$log_price = log(wineData$price)
wineData$log_score = log(wineData$score)

regQ7 = lm(log_price ~ log_score + pinotnoir + cabernet + merlot + syrah +
             napa + bayarea + sonoma + scoast + carn + sierra + mendo + wash +
             d1991 + d1992 + d1993 + d1994 + d1995 + d1996 + d1997 + d1998 + d1999, data = wineData)
covQ7 = vcovHC(regQ7, "HC1")
seQ7 = sqrt(diag(covQ7))

stargazer(regQ7, type = "text",
          digits = 3,
          se = list(seQ7),
          omit = c('napa','bayarea','sonoma','scoast','carn','sierra','mendo','wash','othloc','d1990',
                   'd1991','d1992','d1993','d1994','d1995','d1996','d1997','d1998','d1999'),
          notes.label = 'The dummy variables for region and years are controlled for, though not visible',
          out = "q7_reg.txt")

# A 1% increase score will yield a %5.709 increase in price. That is, the elasticity of price with respect to score 
# is 5.709.

#======================================================================#

### Question 8
# tStat = 5.709 - 1/0.137
# pValue = 2*pnorm(-abs(tStat))
# pValue

linearHypothesis(regQ7, c("log_score = 1"), vcov = covQ7)
coeftest(regQ7, vcov = covQ7)

#Not sure which test is correct but all test give p-value < 0.05. At the 5% significance level, we reject the
# null hypothesis. That is we reject the fact that there is a unitary elasticity between price and score. 

#======================================================================#

### Question 9
# create interactive variables
wineData$logScore_pinotnoir = wineData$pinotnoir*wineData$log_score
wineData$logScore_cabernet = wineData$cabernet*wineData$log_score 
wineData$logScore_merlot = wineData$merlot*wineData$log_score 
wineData$logScore_syrah = wineData$syrah*wineData$log_score 
wineData$logScore_nonvarietal = wineData$nonvarietal*wineData$log_score 

regQ9 = lm(log_price ~ log_score + pinotnoir + cabernet + merlot + syrah + logScore_pinotnoir +
             logScore_cabernet + logScore_merlot + logScore_syrah +
             napa + bayarea + sonoma + scoast + carn + sierra + mendo + wash +
             d1991 + d1992 + d1993 + d1994 + d1995 + d1996 + d1997 + d1998 + d1999, data = wineData)
covQ9 = vcovHC(regQ9, "HC1")
seQ9 = sqrt(diag(covQ9))
stargazer(regQ9, regQ7, type ="text",
          digits = 3,
          se = list(seQ9, seQ7),
          omit = c('napa','bayarea','sonoma','scoast','carn','sierra','mendo','wash','othloc','d1990',
                   'd1991','d1992','d1993','d1994','d1995','d1996','d1997','d1998','d1999'),
          notes.label = 'The dummy variables for region and years are controlled for, though not visible',
          # label = c("regQ9", "regQ7"),
          out = "q9_reg.html")

# That is, the elasticities of price with respect to score for pitnoir , cabernet, merlot, and syrah
# are -1.326, -0.098, -2.613, -2.610. However, the -0.098 coefficient on 
# logScore_cabernet in column (2) of the table above is statistically insignificant,
# which implies scores do not have statistically significantly 
# different prices from having a wine type of cabernet.

#======================================================================#

### Question 10

linearHypothesis(regQ9, c("logScore_pinotnoir = logScore_cabernet",
                          "logScore_cabernet = logScore_merlot", 
                         "logScore_merlot= logScore_syrah"), 
                 vcov = vcovHC(regQ9, "HC1"))


# null hypothesis: the elasticity of price with respect to score is the same for
# the 4 wine types, pinotnoir, cabernet, merlot, syrah.
# alt hypothesis: the elasticity of price with respect to score is different
# for the 4 wine types
# F-stat = 26.781 and p-value < 0.05.
# At the 5% significance level, we reject the null hypothesis. That is we reject the fact that the elasticity of price with respect to score is the same for
# the 4 wine types, pinotnoir, cabernet, merlot, syrah.
# It's sampling distribution with an assumptions of a large random sample is a normal distribution












