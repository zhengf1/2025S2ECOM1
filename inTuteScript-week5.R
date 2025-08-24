#***************************************************************************** 
############### ECOM20001 Econometrics 1 Week 5
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.
#***************************************************************************** 

# this R.script is (will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025S2ECOM1

# You can also use the R code from Canvas.
# -> then you have to prepare before tutorial 
# -> run the code
# -> and label question number to code chunk

# please refer to Canvas R code for detailed explanations 
# (for your self learning)
# it may contains more than the tutorial questions with extra examples,
# so please be patient when go through the code

#***************************************************************************** 
#***************************************************************************** 
#****** Group registration for Assignment 2 closes on 1 September (Monday 2pm) – Week 6
#****** Assignment 2 is available, and is due in Week 8, Monday.
#****** Topics covered include up to and including Lecture Notes 6 and Tutorial 7.
#****** Last question on overall F test: the R code is available in Lecture Notes 6
#******                                  Also check ED discussion for this
#***************************************************************************** 
#*****************************************************************************

#**********************************************************************************************
# remove everything in the environment to start a new project
rm(list=ls()) 

# new: -- set the working directory based
#      -- on the location of your R code
#      -- This is very handy across device
# library("rstudioapi") # install the package if you want to use the following
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# OR as we introduced last few weeks, recall the path
setwd("~/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week5") # your path should be different 
getwd()

# --------- Load Stargazer package for summary statistics and regression tables
library(stargazer)

#**********************************************************************************************
# Earnings and Height
#**********************************************************************************************
# ------------------------- Q1 ------------------------- 
## Load dataset on income and height
mydata1 = read.csv("tute5_height.csv")

## Regression of earnings on height
earn_reg1=lm(earnings~height,data=mydata1)  # run the single linear regression
summary(earn_reg1)                          # summarize the regression results

## 95% CI for the OLS slope coefficient on height from earn_reg1 regression
confint(earn_reg1, 'height', level=0.95)

## 95% CI for the OLS intercept from earn_reg1 regression
confint(earn_reg1, '(Intercept)', level=0.95)


## Obtain regression coefficients from earn_reg1 regression
beta_hat = coef(summary(earn_reg1))[, "Estimate"] 
# or 
beta_hat = earn_reg1$coefficients
# beta[1] is OLS estimate of intercept
# beta[2] is OLS estimate of slope

## Obtain standard errors from coefficients earn_reg1 regression
se = coef(summary(earn_reg1))[, "Std. Error"] 
# se[1] is standard error of OLS estimate of intercept
# se[2] is standard error of OLS estimate of slope

## Compute 95% CI of the regression slope coefficient by hand
qnorm(0.975)
qnorm(0.025)
CI95_low = beta_hat[2] - 1.96*se[2]    # lower bound of 95% CI
CI95_upp = beta_hat[2] + 1.96*se[2]    # upper bound of 95% CI

#**********************************************************************************************
# ------------------------- Q2 ------------------------- 
# beta_hat +- critial_value * se(beta_hat)
# beta_hat*100 +- critial_value * se(beta_hat*100)

# var(100 beta_hat) = 100^2 var(beta_hat)
# se(100 beta_hat) = sqrt( 100^2 var(beta_hat)) = 100 se(beta_hat)

# beta_hat*100 +- critial_value * 100 se(beta_hat)
# 100* (beta_hat +- critial_value * se(beta_hat))

## 95% CI for increasing height by 100cm on earnings
CI95_low_100 = 100 * (beta_hat[2] - 1.96*se[2])    # lower bound of 95% CI
CI95_upp_100 = 100 * (beta_hat[2] + 1.96*se[2])    # upper bound of 95% CI
paste("95% CI lower bound for 100cm increase in earnings is: ", CI95_low_100)
paste("95% CI upper bound for 100cm increase in earnings is: ", CI95_upp_100)

#**********************************************************************************************
# ------------------------- Q3 ------------------------- 
# H0: 10cm -> 3000 income increase (measured in 10,000)
# H0: 10cm -> 0.3 income increase (in the data)
# H0: 1 cm -> 0.03 income increase

## t-statistic and p-value for null that slope = 0.03
# H0: beta1 = 0.03 H1: beta1 not= 0.03
# both t-stat and p-value from the regress table are not valid

(tstat2 = (beta_hat[2]-0.03)/se[2])
(critical_value = qnorm(0.975)) # 5%; two tail.
(pval2 = 2 * pnorm(-abs(tstat2)))
paste("pvalue for 2-sided test of null that slope=0.03 is:", pval2)
# Fail to reject null, p-value (pval2)=0.280

#**********************************************************************************************
# REGRESSION: HOMICIDES AND POLICE

#**********************************************************************************************
# ------------------------- Q1 ------------------------- 
rm(list=ls()) # remove everything in the environment to start a new project

## Load dataset on income and height
mydata2 = read.csv("tute5_crime.csv")

## Summary Statistics
stargazer(mydata2, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", title="Descriptive Statistics",
          out="sumstats2.txt")
# So a typical county had 3066 police offices and 13 homicides in 2012
# The range is considerable: min police and homicides is 809 and 1
# The max police and homicides is 31435 and 111 (!)

## Scatter plot of homicides and police numbers
plot(mydata2$police,mydata2$homicides,
     main="Homicides and Police Force Across England and Wales Counties",
     xlab="Number of Police Officers in 2012",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
# is the pattern counter-intuitive?

#**********************************************************************************************
# ------------------------- Q2 ------------------------- 

## identify any potential outlier
mydata2$county[which(mydata2$homicides>100)]

plot(mydata2$police[-25],mydata2$homicides[-25])

## Scatter plot removing potential outlier
 plot(mydata2$police[mydata2$homicides<100],mydata2$homicides[mydata2$homicides<100],
     main="Homicides and Police Force Across England and Wales Counties",
     sub="(Outlier Removed)",
     xlab="Number of Police Officers in 2012",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
 
#**********************************************************************************************
# ------------------------- Q3 ------------------------- 
crime_reg1 = lm(homicides~police, data=mydata2)
summary(crime_reg1)
confint(crime_reg1, 'police', level=0.95)

#**********************************************************************************************
# ------------------------- Q4 ------------------------- 
## Construct re-scalled police independent variable in terms of 1000's of police
mydata2$police_1000 = mydata2$police/1000     # save the re-scaled police variable in mydata2
summary(mydata2)
# Notice how with the summary statistics police_1000 now shows up with a mean of 
# 3.066 (which means 3066 police on average) which has a similar scale as the mean 
# of homicides in the sample of 12.93

## Re-run our homicides and police regression with our re-scaled police_1000 regressor
## and compute the 95% confidence interval
crime_reg2=lm(homicides~police_1000, data=mydata2)
summary(crime_reg2)
confint(crime_reg2, 'police_1000', level=0.95)


#**********************************************************************************************
# ------------------------- Q5 ------------------------- 
# Outliers

# Recall from our scatter plot above "q2_scat_homicides_police1.pdf" that 'Metropolitan Police'
# is a potential outlier. Let's look at the influence of the outlier on our results using 
# re-scaled police numbers throughout

## Regression results without potential outlier
crime_reg3=lm(homicides[-25]~police_1000[-25], data=mydata2)
summary(crime_reg3)
confint(crime_reg3, 'police_1000[homicides < 100]', level=0.95)


## Plotting the impact of the outlier on regression results, highlighting the outlier's impact
 plot(mydata2$police_1000,mydata2$homicides,
     main="Homicides and Police Force Across England and Wales Counties",
     xlab="Number of Police Officers in 2012 (1000s)",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
abline(crime_reg2, col="blue", lwd=2)
abline(crime_reg3, col="red", lwd=2)
legend("bottomright", c("regression 2 (include outlier)","regression 3 (omit outlier)"), col = c("blue","red"),pch=16)

############### Notes ############### 
# this R.script is (will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025S2ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required



