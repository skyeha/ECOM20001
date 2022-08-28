#**********************************************************************************************
# Tutorial 11 Code
# By: David Byrne
# Objectives: Logarithmic Regression and Interactions
#**********************************************************************************************


#**********************************************************************************************
# 1. SET WORKING DIRECTORY, LOAD DATA AND PACKAGES

## Set the working directory for the tutorial file
setwd("/Users/byrned/Dropbox/Teaching/20001/Tutorials/Tutorial11")

## Load stargarzer package for summary statistics and regression tables
library(stargazer)

## Load Applied Econometrics Package for testing and heteroskedasticity robust standard errors
library(AER)

## Load dataset on income and height
mydata1=read.csv(file="tute11_cps.csv")

## List the variables in the dataset named mydata1
names(mydata1)

## Dimension of the dataset
# 15052 observations (individuals)
# 5 variables: year, ahe, bachelor, female, age
dim(mydata1)


#**********************************************************************************************
# 2. COMBINING LOGARITHMIC AND INTERACTIONS REGRESSIONS

# A. CREATE LOGARITHMIC VARIABLES AND INTERACTIONS

## Create logarthimic variables for ahe and age
mydata1$log_ahe=log(mydata1$ahe)
mydata1$log_age=log(mydata1$age)

# The as.numeric() command can be used to create dummy variables in R
## Create year of sample dummy variables for the regression
mydata1$d1992=as.numeric(mydata1$year==1992)  #d1992 is a dummy variable that equals one if year=1992, and 0 otherwise
mydata1$d2012=as.numeric(mydata1$year==2012)  #d2012 is a dummy variable that equals one if year=2012, and 0 otherwise

## Create interactive variables involving female and age
mydata1$female_age=mydata1$female*mydata1$age

## Create the interaction between female and log(age)
mydata1$female_log_age=mydata1$female*mydata1$log_age


# B. RUN INTERACTIVE LOGARITHMIC REGRESSIONS

## Regression of log(ahe) on age, allowing for differential gender effects with bachelor, + controls
reg1=lm(log_ahe~age+female_age+female+bachelor+d1992,data=mydata1)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))

## Regression of log(ahe) on log(age), allowing for differential gender effects with bachelor, + controls
reg2=lm(log_ahe~log_age+female_log_age+female+bachelor+d1992,data=mydata1)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, type="text",
          se=list(se1, se2),
          digits=3, 
          dep.var.labels=c("Log(AHE)"),
          covariate.labels=
            c("Age",
              "Age x Female",
              "Log(Age)",
              "Log(Age) x Female",
              "Female",
              "Bachelor Degree",
              "1992 Dummy",
              "Constant"),          
          out="reg_output1.txt")   # Output results to your director in a text file



# C. COMPUTE PARTIAL EFFECTS AND THEIR CONFIDENCE INTERVALS

## Elasticity of ahe with respect to age for females is the sum of the 
# log_age and female_log_age coefficients in reg2
coef_log_age=summary(reg2)$coefficients[2, 1]
coef_female_log_age=summary(reg2)$coefficients[3, 1]
ahe_elasticity_female=coef_log_age+coef_female_log_age

## Fstatistic for Test that Sum of Coefficients Equals 0: log_age+female_log_age=0
# Note: this continues to exactly follow the general approach for computing standard errors for partial effects in nonlinear
# regression models on slides 19 to 23 of lecture note 8, where the change here is whether the female dummy equals 1
Ftest=linearHypothesis(reg2,c("log_age+female_log_age=0"),vcov = vcovHC(reg2, "HC1"))

## Recover the Fstat from the joint test results in Ftest
Fstat=Ftest[2,3]
sprintf("Fstat %f", Fstat)

## Compute the standard error for the partial effect we computed, dahe (see slide 21 of Lecture note 8) 
se_ahe_elasticity_female=abs(ahe_elasticity_female)/sqrt(Fstat)

## 95% CI for the partial effect we computed, dahe
ahe_elasticity_female_ci95L=ahe_elasticity_female-se_ahe_elasticity_female*1.96
ahe_elasticity_female_ci95H=ahe_elasticity_female+se_ahe_elasticity_female*1.96

## Outputting results
sprintf("ahe-age elasticity for females: %f", ahe_elasticity_female)
sprintf("SE of ahe-age elasticity for females: %f", se_ahe_elasticity_female)
sprintf("95 CI lower bound ahe-age elasticity for females: %f", ahe_elasticity_female_ci95L)
sprintf("95 CI upper bound ahe-age elasticity for females: %f", ahe_elasticity_female_ci95H)


#**********************************************************************************************
# 3. COMBINING INTERACTIONS: DIFFERENCE-IN-DIFFERENCES MODELS

# A. CREATE INTERACTIONS

## Create logarthimic variables for ahe and age
mydata1$bachelor_d2012=mydata1$bachelor*mydata1$d2012


# B. RUN DIFFERENCE-IN-DIFFERENCE REGRESSION

## DiD regression of log(ahe) 
reg3=lm(log_ahe~bachelor+d2012+bachelor_d2012+female+age,data=mydata1)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg3, type="text",
          se=list(se3),
          digits=3, 
          dep.var.labels=c("Log(AHE)"),
          covariate.labels=
            c("Bachelor Degree",
              "2012 Dummy",
              "Bachelor Degree X 2012 Dummy",
              "Female",
              "Age",
              "Constant"),          
          out="reg_output2.txt")   # Output DiD results to your director in a text file


