#***************************************************************************** 
############### ECOM20001 Econometrics 1 Week 8
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.
#***************************************************************************** 

# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching section
# or my github: https://github.com/zhengf1/2025S2ECOM1

# please refer to Dave's R code for detailed explanations (for your self learning)
# it contains much more than the tutorial questions with extra examples,
# so please be patient when go through the code

#***************************************************************************** 
#****** Admin:
#***************************************************************************** 
#****** Welcome back
#**********************************************************************************************
#**********************************************************************************************

# remove everything in the environment to start a new project
rm(list=ls()) 

# new: -- set the working directory based
#      -- on the location of your R code
#      -- This is very handy across device
# library("rstudioapi") # install the package if you want to use the following
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# OR as we introduced last few weeks, recall the path
setwd("~/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week8") # your path should be different 
getwd()

## Load Stargazer package for summary statistics and regression tables
# install.packages(“stargazer”)
library(stargazer)

## Load Applied Econometrics package (see above for installing AER package)
# install.packages(“AER”)
library(AER)

#**********************************************************************************************
# ------------------ Q0 ------------------
## Load dataset
#**********************************************************************************************

donotsmoke=read.csv(file="tute8_smoke.csv")

#***************************************************************************** 
# ------------------ Q1 ------------------
# JOINT MODEL TESTING: waldtest(), linearHypothesis()
#***************************************************************************** 

# The overall model F-statistics for the test of the joint null that are regression coefficients 
# are jointly equal to 0.
# Alternative: at least one for them not = zero

## Run main regression specification interest and print output
reg=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
coeftest(reg, vcov = vcovHC(reg, "HC1"))

# The waldtest() command, which is another name for the F-test, can be used to 
# compute the heteroskedasticity-robust overall F-statistic for a given regression
# Notice that with n=3000 observations and q=k=10 regressors, the degrees of freedom
# for this F-statistic are df1=10 and df2=3000-10-1=2989
## Compute the overall regression F-statistic for our regression
waldtest(reg, vcov = vcovHC(reg, "HC1"))

# Alternatively, you can use the linearHypothesis() command to compute the 
# heteroskedasticity-robust overall regression F-statistic by jointly testing
# that all the regression coefficients are 0. Here, we input all of the constraints into 
# the linearHypothesis() command in an intuitive way
## Compute the overall regression F-statistic for our regression (alternative method)
linearHypothesis(reg,c("smoker=0",
                       "alcohol=0",
                       "drinks=0",
                       "nprevisit=0",
                       "tripre1=0",
                       "tripre2=0",
                       "tripre3=0",
                       "unmarried=0",
                       "educ=0",
                       "age=0"),vcov = vcovHC(reg, "HC1"))

# Notice how both waldtest() and linearHypothesis yield identifcal F-statistics
# of F=23.88 and p-values<0.000001

# We can also compute the overall regression F-statistic for the regression
# using the summary() command, but this has the problem that it assumes homoskedasticity
## Compute the overall regression F-statistic for our regression (under homoskedasticity) 
summary(reg)

# Notice here how the F-statistic is larger at F=30.94. That is, once we correct for
# heteroskedasticity, we obtain a very differnt F-statistic, even though the outcome of the
# joint test is the same. This highlights the importance of accounting for 
# heteroskedasticity when computing F-statistics and conducting joint hypothesis tests

#***************************************************************************** 
# ------------------ Q2 ------------------
# TESTING JOINT RESTRICTIONS
#***************************************************************************** 

## Jointly test coefficients on smoker=0 and alcohol=0
linearHypothesis(reg,c("smoker=0","alcohol=0"),vcov = vcovHC(reg, "HC1"))

## Jointly test coefficients on smoker=-200 and alcohol=-50
linearHypothesis(reg,c("smoker=-200","alcohol=-50"),vcov = vcovHC(reg, "HC1"))

## Jointly test coefficients on tripre1=0 and tripre2=0 and tripre3=0
linearHypothesis(reg,c("tripre1=0","tripre2=0","tripre3=0"),vcov = vcovHC(reg, "HC1"))

## Jointly test coefficients on tripre1=0 and tripre2=0
linearHypothesis(reg,c("tripre1=0","tripre2=0"),vcov = vcovHC(reg, "HC1"))

## Jointly test coefficients on tripre1=200 and tripre2=300 and tripre3=400
linearHypothesis(reg,c("tripre1=200","tripre2=300","tripre3=400"),vcov = vcovHC(reg, "HC1"))

#***************************************************************************** 
# ------------------ Q3 ------------------
# HOMOSKEDASTICITY-ONLY F-STATISTIC
#***************************************************************************** 
# Conduct homoskedasticity-only F-Statistic
# for the test tripre1=0, tripre2=0, tripre3=0

## Run the unrestricted regression and save the unrestricted R-Squared, assuming homoskedasticity
reg1=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
R2u=summary(reg1)$r.squared

## Run the restricted regression where the coefficients tripre1=0, tripre2=0, tripre3=0
## and save the restricted R-Squared, assuming homoskedasticity
reg2=lm(birthweight~smoker+alcohol+drinks+nprevisit+unmarried+educ+age,data=donotsmoke)
R2r=summary(reg2)$r.squared

## Compute the homoskedasticity-only F-statistic (df1=3, df2=3000-10-1=2989)
num1=(R2u-R2r)/3
denom1=(1-R2u)/2989 
Fstat1=num1/denom1

## Compute p-value for Fstat using cumulative density function of F-distribution
## with df1=3, df2=2989 degrees of freedom; see tute3.R for CDF commands if needed
(pval1 = 1-pf(Fstat1,df1=3,df2=2989))

# alternative formula
SSRu <- sum(residuals(reg1)^2)
SSRr <- sum(residuals(reg2)^2)
Fstat1_alt = ((SSRr-SSRu)/3) / (SSRu/2989)

#***************************************************************************** 
# ------------------ Q4 ------------------
# TESTING RESTRICTIONS WITH MULTIPLE PARAMETERS
#***************************************************************************** 
# We can also use linearHypothesis() to test a single restriction involving
# multiple regression coefficients. For this, we run the command as
# linearHypothesis(reg,c("xvar1=xvar2")), which tests the null that the 
# regression coefficients on xvar1 and xvar2 in the regression are equal

## Re-run the main regression of interest for joint testing
reg=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
coeftest(reg, vcov = vcovHC(reg, "HC1"))

## Test regression coefficients on smoker and alcohol are equal
linearHypothesis(reg,c("smoker=alcohol"),vcov = vcovHC(reg, "HC1"))

## Test regression coefficient on smoker is twice as large as the regression coefficient on alcohol
linearHypothesis(reg,c("smoker=2*alcohol"),vcov = vcovHC(reg, "HC1"))

## Test sum of regression coefficients on smoker and alcohol equals -200
linearHypothesis(reg,c("smoker+alcohol=-200"),vcov = vcovHC(reg, "HC1"))

## Test sum of regression coefficients on alcohol and unmarried equals the coefficient on alcohol
linearHypothesis(reg,c("alcohol+unmarried=smoker"),vcov = vcovHC(reg, "HC1"))

## Test regression coefficient on tripre1, tripre2 and tripre3 are jointly equal to each other
linearHypothesis(reg,c("tripre1=tripre2","tripre2=tripre3"),vcov = vcovHC(reg, "HC1"))

## Test coefficient on tripre2 is twice the coefficient on tripe1 and that 
## the coefficient on tripe3 is twice the coefficient on tripe2
linearHypothesis(reg,c("tripre2=2*tripre1","tripre3=2*tripre2"),vcov = vcovHC(reg, "HC1"))

#***************************************************************************** 
# ------------------ Q5 ------------------
# D. TRANSFORMING THE REGRESSION TO CONDUCT A TEST INVOLVING MULTIPLE PARAMETERS
#***************************************************************************** 
# Following a similar strategy to what is in the lecture notes, you can transform the regression
# to test the null that alcohol+unmarried=smoker. You can re-write the regression equation such that 
# you obtain a regression of the form: birthweight=b0 + g1 x smoker + b2 x W + b3 x Z + u 
# where W=alcohol+smoker and Z=unmarried+smoker, and where the a t-test for the null that the 
# regression coefficient on tripre1, g1, equals 0 is equivalent to testing the null that the 
# coefficients alcohol+unmarried=smoker the solutions provides the explicit calculations, 
# and the tutors will work through these in the tute.

# bw = b0 + b1 smoker + b2 alcohol + b3 unmarried
# alcohol + unmarried = smoker
linearHypothesis(reg,c("alcohol+unmarried=smoker"),vcov = vcovHC(reg, "HC1"))

## construct W
donotsmoke$W=donotsmoke$alcohol+donotsmoke$smoker

## construct Z
donotsmoke$Z=donotsmoke$unmarried+donotsmoke$smoker

## Re-run the transformed regression and conduct t-test
reg=lm(birthweight~smoker+W+Z+drinks+nprevisit+tripre1+tripre2+tripre3+educ+age,data=donotsmoke)
coeftest(reg, vcov = vcovHC(reg, "HC1"))

# Notice how the coefficient on smoker is 24.708, where you fail to reject the null with 
# a p-value of 0.8094. Notice this single-regressor hypothesis for the test of the null of the 
# coefficients alcohol+unmarried=smoker is identical to the p-value=0.8094 
# from the joint-test of this null:
reg=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
linearHypothesis(reg,c("alcohol+unmarried=smoker"),vcov = vcovHC(reg, "HC1"))

############### Notes ############### 
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching section
# or my github: https://github.com/zhengf1/2025S2ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required



