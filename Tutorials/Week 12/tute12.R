#**********************************************************************************************
# Tutorial 12 Code
# By: David Byrne
# Objectives: Time Series Regression
#**********************************************************************************************


#**********************************************************************************************
# 1. SET WORKING DIRECTORY, LOAD DATA AND PACKAGES

## Set the working directory for the tutorial file
setwd("/Users/byrned/Dropbox/Teaching/20001/Tutorials/Tutorial12")

## Load stargarzer package for summary statistics and regression tables
library(stargazer)

## Load Applied Econometrics Package for testing and heteroskedasticity robust standard errors
library(AER)

## Load dataset on consumer search and petrol prices
mydata=read.csv(file="tute12_petrol.csv")

## List the variables in the dataset named mydata1
names(mydata)

## Dimension of the dataset
# 48 observations (weeks in Perth)
# 4 variables: week, search, price, cost
dim(mydata)

#**********************************************************************************************
# 2. VARIABLE CREATION: LAGS, DIFFERENCES, SEASONAL DUMMIES

# Re-scale search variable in terms of 10000's searches per-week
# Important for making AR(p) and ADL(p,q) models more interpretable
mydata$search=mydata$search/10000

# A. CONSTRUCTING LAGS OF VARIABLES 

# We can use the head() command to construct lage variables
# In general, head(var,-k) will return the variable "var" without
# the first k observations. We can use to create lagged variables that 
# shift a variable forward, and fill in the first k observations with 
# "NA" to create a lagged variable

# Note: there are other more elegant ways to construct lagged variables in 
# R which requires the use of time series packages in R, but this is more involved
# and are left for ECOM30002: Econometrics 2 when time series is covered extensively

# First and second lag of price
# Use View(mydata) to check that these are indeed lagged prices
mydata$price_lag1=c(NA,head(mydata$price, -1))      # Shifts the price variable forward 1 places, fills in first data point with NA
mydata$price_lag2=c(NA,NA,head(mydata$price, -2))   # Shifts the price variable forward 2 places, fills in first data point with NA

# First and second lag of cost
mydata$cost_lag1=c(NA,head(mydata$cost, -1))      
mydata$cost_lag2=c(NA,NA,head(mydata$cost, -2))   

# First and second lag of search
mydata$search_lag1=c(NA,head(mydata$search, -1))      
mydata$search_lag2=c(NA,NA,head(mydata$search, -2))   


# B. CONSTRUCTING FIRST DIFFERENCES OF VARIABLES

# The diff() command computes the first difference of a variable. For instance
# diff(mydata@search) returns a new variable that is the difference in search
# week-to-week. For observation #2, diff(mydata@search) would return 
# 79815-75265=4550, the difference in search between week 1 and 2 of the sample
# Notice that you cannot compute a difference for the first observation of the sample
# because you there is no observation that comes before the first observation. 

# Compute first difference of price variable and its two lags
# Note that the "NA" part fills in "NA" for the first observation for the differenced
# variable, which we cannot compute because no value comes before the first observation
mydata$d_price=c(NA,diff(mydata$price))
mydata$d_price_lag1=c(NA,diff(mydata$price_lag1))
mydata$d_price_lag2=c(NA,diff(mydata$price_lag2))

# First difference of the cost variable
mydata$d_cost=c(NA,diff(mydata$cost))
mydata$d_cost_lag1=c(NA,diff(mydata$cost_lag1))
mydata$d_cost_lag2=c(NA,diff(mydata$cost_lag2))

# First difference of the search variable
mydata$d_search=c(NA,diff(mydata$search))
mydata$d_search_lag1=c(NA,diff(mydata$search_lag1))
mydata$d_search_lag2=c(NA,diff(mydata$search_lag2))


# C. SEASONAL DUMMIES

# Run preliminary seasonality regressions with search, prices, costs
# Create quarterly dummy variables, week 1 is the first week of January 2018
mydata$sum=as.numeric(mydata$week>=1 & mydata$week<=13)    # Summer dummy (Quarter 3)
mydata$aut=as.numeric(mydata$week>=14 & mydata$week<=26)   # Autumn dummy (Quarter 4)
mydata$win=as.numeric(mydata$week>=27 & mydata$week<=39)   # Winter dummy (Quarter 1)
mydata$spr=as.numeric(mydata$week>=40 & mydata$week<=52)   # Spring dummy (Quarter 2)


#**********************************************************************************************
# 2. SUMMARY STATISTICS, TIME SERIES PLOTS, AUTOCORRELATIONS: acf(), diff()

## Summary Statistics
stargazer(mydata, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", title="Descriptive Statistics",
          out="sumstats1.txt")


# A. TIME SERIES PLOTS

# Plotting retail prices and costs (note you could also do this in ggplot2() if you like)
pdf("line_price_cost.pdf")
plot(mydata$week,mydata$price, type="l",col="blue", ylim=c(120,160),
     xlab="Week",ylab="Retail Price, Wholesale Cost (cpl)",
     main="Retail Petrol Prices")
     lines(mydata$cost, col = "forestgreen",type="l",lty=2)
     legend("topleft", legend=c("Retail Price", "Wholesale Cost"), 
     col=c("blue","forestgreen"), lty=c(1,2),cex = 0.7)
dev.off()

# Plotting web search activity
pdf("line_search.pdf")
plot(mydata$week,mydata$search,
     type="l",col="red",xlab="Week",ylab="Total Weekly Website Hits",main="Platform Website Hits")
dev.off()


# B. AUTOCORRELATIONS IN LEVELS

# We can compute autocorrelations for time series variables using the acf() command in R
# As described in lecture note 9 slides 18-20, autocorrelations are useful for visualizing 
# the degree of persistence in time series variables for different lags
# The R-output presents that autocorrelations at 0,1,2,3,... lags, where the autocorrelation
# at the lag=0 is always 1 because it is the correlation of a variable with itself. This is unlike
# at lag=1 for example, which is the autocorrelation between a variable and its lag. Always
# ignore the autocorrelation at lag=0 and focus on the autocorrelations at lags=1,2,3,...

# Note also that the R output by default presents 95% confidence intervals as dashed lines 
# for the autocorrelations at different lags which is centered around 0 (meaning 0 autocorelation at a given lag). 
# So you can tell if an autocorrelation is statistically significant if its value
# is larger than the upper or lower bound of the 95% confidence interval. This gives a sense of 
# how many lags you might need to include in your time series models to properly model the degree of
# persistence in a variable. Note: in the lecture notes, we have not explicitly derived the 
# 95% CI for autocorrelations; this is left for 3rd year econometrics subjects

# Autocorrelations for Price
pdf("acf_price.pdf")
acf(mydata$cost,main="Autocorrelations for Price")
dev.off()

# Autocorrelations for Cost
pdf("acf_cost.pdf")
acf(mydata$cost,main="Autocorrelations for Cost")
dev.off()

# Autocorrelations for Website Search
pdf("acf_search.pdf")
acf(mydata$search,main="Autocorrelations for Website Hits")
dev.off()


# C. AUTOCORRELATIONS IN FIRST DIFFERENCES

# We can also compute the autocorrelations for the differenced variables to show how
# taking first differences results in less persistent time series relative to the
# ACFs computed above for these variables in levels and not differences
# Here, we simply use the diff() command within the acf() command to compute the 
# ACF for the differenced variables. Below, we use the differenced variables we just
# constructed for estimating AR(p) and ADL(p,q) models

# Autocorrelations for first difference of price
pdf("acf_price_diff.pdf")
acf(diff(mydata$price),main="Autocorrelations for Difference of Price")
dev.off()

# Autocorrelations for first difference of cost
pdf("acf_cost_diff.pdf")
acf(diff(mydata$cost),main="Autocorrelations for Difference of Cost")
dev.off()

# Autocorrelations for first difference of web search
pdf("acf_search_diff.pdf")
acf(diff(mydata$search),main="Autocorrelations for Difference of Website Hits")
dev.off()


#**********************************************************************************************
# 3. AUTOREGRESSIONS

# Number of observations in the time series (used below)
T=48

# B. PRELIMINARY SEASONALITY REGRESSIONS

# Search seasonality regression check
seas1=lm(search~sum+aut+win,data=mydata)
cov1=vcovHC(seas1, type = "HC1")    
se1=sqrt(diag(cov1))

# Prices seasonality regression check
seas2=lm(price~sum+aut+win,data=mydata)
cov2=vcovHC(seas2, type = "HC1")    
se2=sqrt(diag(cov2))

# Cost seasonality regression check
seas3=lm(cost~sum+aut+win,data=mydata)
cov3=vcovHC(seas3, type = "HC1")    
se3=sqrt(diag(cov3))

## Regression output table 
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(seas1, seas2, seas3, type="text",
          se=list(se1, se2, se3),
          digits=2, 
          covariate.labels=
            c("Summer",
              "Autumn",
              "Winter",
              "Constant"),
          out="reg_output1.txt")


# C. ESTIMATE AR(p) MODELS AND COMPUTE BIC/AIC

# AR(1) model with search
reg1=lm(search~search_lag1,data=mydata)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))
K1=2                             # Number of parameters in regression reg1
ssr1=sum(residuals(reg1)^2)      # SSR from the regression reg1
BIC1=log(ssr1/T)+K1*(log(T)/T)   # BIC from the regression reg1
AIC1=log(ssr1/T)+K1*(log(2)/T)   # BIC from the regression reg1

# AR(1) model with search with seasonal dummies
reg2=lm(search~search_lag1+sum+aut+win,data=mydata)
cov2=vcovHC(reg1, type = "HC1")    
se2=sqrt(diag(cov2))
coeftest(reg2, vcov = vcovHC(reg2, "HC1"))
K2=5                            
ssr2=sum(residuals(reg2)^2)     
BIC2=log(ssr2/T)+K2*(log(T)/T)
AIC2=log(ssr2/T)+K2*(log(2)/T)

# AR(2) model with search with seasonal dummies
reg3=lm(search~search_lag1+search_lag2+sum+aut+win,data=mydata)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))
K3=6                          
ssr3=sum(residuals(reg3)^2)     
BIC3=log(ssr3/T)+K3*(log(T)/T)
AIC3=log(ssr3/T)+K3*(log(2)/T)


# D. ESTIMATE ADL(p,q) MODELS

# ADL(0,2) model with search and price
reg4=lm(search~price_lag1+price_lag2+sum+aut+win,data=mydata)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))
K4=6                             
ssr4=sum(residuals(reg4)^2)     
BIC4=log(ssr4/T)+K4*(log(T)/T)
AIC4=log(ssr4/T)+K4*(log(2)/T)

# ADL(1,1) model with search and price
reg5=lm(search~search_lag1+price_lag1+price_lag2+sum+aut+win,data=mydata)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))
K5=7                            
ssr5=sum(residuals(reg5)^2)     
BIC5=log(ssr5/T)+K5*(log(T)/T)
AIC5=log(ssr5/T)+K5*(log(2)/T)


# E. ESTIMATE ADL(p,q1,q2) MODELS

# ADL(1,1,2) model with search, price, and cost
reg6=lm(search~search_lag1+price_lag1+price_lag2+cost_lag1+cost_lag2+sum+aut+win,data=mydata)
cov6=vcovHC(reg6, type = "HC1")    
se6=sqrt(diag(cov6))
K6=9                           
ssr6=sum(residuals(reg6)^2)     
BIC6=log(ssr6/T)+K6*(log(T)/T)  
AIC6=log(ssr6/T)+K6*(log(2)/T)  

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type="text",
          se=list(se1, se2, se3, se4, se5, se6),
          digits=2, 
          dep.var.labels=c("Search"),
          covariate.labels=
            c("Search, Lag 1",
              "Search, Lag 2",
              "Price, Lag 1",
              "Price, Lag 2",
              "Cost, Lag 1",
              "Cost, Lag 2",
              "Summer",
              "Autumn",
              "Winter",
              "Constant"),
          out="reg_output2.txt")   # Output results to your director in a text file




#**********************************************************************************************
# 3. FORECASTING

# A. RE-RUN ADL REGRESSION FOR FORECASTING
reg5=lm(search~search_lag1+price_lag1+price_lag2+sum+aut+win,data=mydata)
coeftest(reg5, vcov = vcovHC(reg5, "HC1"))


# B. WITHIN-SAMPLE FORECASTING

## Use dataset and predict() function to generate in-sample predictions based on reg5 regression model estimates
search_fcast_in=predict(reg5, data=mydata) 

# Add the within-sample forecasts and forecast errors t othe mydata dataset
# Need to make the first 2 observations in search_fcast_in "NA" because you cannot compute forecast error for first 2 observations
# due to the first and second lags of price in the regression model, which means the first and second observations
# do not have a within-sample prediction
mydata$search_fcast_in=c(NA,NA,search_fcast_in)   

# Add forecast errors to mydata
mydata$search_fcast_in_error=mydata$search-mydata$search_fcast_in

## Output some within-sample forecasting results
sprintf("Within-sample forecast for period T=48: %f", mydata$search_fcast_in[T])
sprintf("Within-sample forecast error for period T=48: %f", mydata$search_fcast_in_error[T])
sprintf("Average forecast error: %f", mean(mydata$search_fcast_in_error,na.rm=TRUE))         # Compute mean without NA data points
sprintf("Std. Dev. of forecast error: %f", sd(mydata$search_fcast_in_error,na.rm=TRUE))      # Compute std. dev. without NA data points

## Plot Search Data and Within-Sample Forecasts
pdf("line_search_forecast.pdf")
plot(mydata$week,mydata$search, type="l",col="blue",
     xlab="Week",ylab="Retail Price, Forecast (cpl)",main="Number of Website Searches (10000s)")
     lines(mydata$search_fcast_in, col = "orange",type="l")
     legend("topleft", legend=c("Website Hits", "Within-Sample Forecast"), 
     col=c("blue","orange"), lty=c(1,1),cex = 0.6)
dev.off()

## Plot Forecast Errors
pdf("line_search_forecast_error.pdf")
plot(mydata$week,mydata$search_fcast_in_error, type="l",col="green",
     xlab="Week",ylab="Retail Price, Forecast (cpl)",main="Number of Website Searches (10000s)")
dev.off()


# C. OUT-OF-SAMPLE FORECASTING

## Data frame for the last observation in the sample of T=48 (defined on line 70) for generating a forfecast
newdata1=data.frame(search_lag1=mydata$search_lag1[T],price_lag1=mydata$price_lag1[T],price_lag2=mydata$price_lag2[T],
                    sum=mydata$sum[T],aut=mydata$aut[T],win=mydata$win[T])

## Use the predict() command to forecast out the next observation in period T+1 out of the sample
search_fcast_out=predict(reg5, newdata=newdata1) 

## Standard error of search_fcast_in_error=RMSFE (see slides 27 and 28 in Lecture Note 9)
RMSFE=sqrt(var(residuals(reg5)))

## 95% CI for out-of-sample forecast
search_fcast_out_CIL=search_fcast_out-1.96*RMSFE
search_fcast_out_CIH=search_fcast_out+1.96*RMSFE

## Output results
sprintf("Out-of-sample forecast for period T=49: %f", search_fcast_out)
sprintf("95 CI lower bound for out-of-sample forecast: %f", search_fcast_out_CIL)
sprintf("95 CI upper bound for out-of-sample forecast: %f", search_fcast_out_CIH)


#**********************************************************************************************
# 4. GRANGER CAUSALITY

## Re-run the ADL(1,2,2) regression
reg6=lm(search~search_lag1+price_lag1+price_lag2+cost_lag1+cost_lag2+sum+aut+win,data=mydata)

## Conduct the Granger Causality Test
linearHypothesis(reg6,c("price_lag1=0",
                        "price_lag2=0"),vcov = vcovHC(reg6, "HC1"))


