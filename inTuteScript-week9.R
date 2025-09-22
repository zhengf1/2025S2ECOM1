#***************************************************************************** 
############### ECOM20001 Econometrics 1 Week 9
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
#****** No tutorials next week, see you in the week after
#***************************************************************************** 
#***************************************************************************** 

# remove everything in the environment to start a new project
rm(list=ls()) 

# new: -- set the working directory based
#      -- on the location of your R code
#      -- This is very handy across device
# library("rstudioapi") # install the package if you want to use the following
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# OR as we introduced last few weeks, recall the path
setwd("~/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week9") # your path should be different 
getwd()

## Load Stargazer package for summary statistics and regression tables
# install.packages(“stargazer”)
library(stargazer)

## Load Applied Econometrics package (see above for installing AER package)
# install.packages(“AER”)
library(AER)

## Load ggplot2 Package for Nice Graphs and Easily Fitting Curves 
# install.packages(“ggplot2”)
library(ggplot2)
# NOTE: run "install.packages(ggplot2)" in the Console to install the ggplot2() package
# just like was done previously with the "AER" and "stargazer" packages

#**********************************************************************************************
# ------------------ Q0 ------------------
## Load dataset
#**********************************************************************************************
mydata1=read.csv(file="tute9_cps.csv")

## Create nonlinear terms for polynomial regressions
mydata1$age2 = mydata1$age*mydata1$age
mydata1$age3 = mydata1$age*mydata1$age*mydata1$age
mydata1$age4 = mydata1$age*mydata1$age*mydata1$age*mydata1$age

#**********************************************************************************************
# ------------------ Q1 ------------------
# VISUALIZE DATA
#**********************************************************************************************

## Scatter plots for earnings and age
plot(ahe~age,data=subset(mydata1),
     main="Earnings and Age",
     xlab="Age",
     ylab="Average Household Earnings (1000s)",
     col="darkgrey",
     xlim=c(25,34),
     ylim=c(0,80),
     pch=1, cex=0.75)

# - ggplot - 
## Nicer scatter plot for fitting quadratic function with the ggplot package
## Quadratic function of degree 2 specified on the line where it states "formula = y ~ poly(x,2)"
ggplot(mydata1, aes(y=ahe, x=age)) +                                    # Define the dataset, x and y variables for scatter plot
  geom_point(alpha = .3) +                                              # Allow for shading of the points in the scatter plot to help visualisation
  stat_smooth(method = "lm", formula = y ~ poly(x,2), col="blue") +     # Fit a polynomial of DEGREE 2 (QUADRATIC)
  ggtitle("Relationship Between Earnings and Age") +                    # Scatter plot title
  theme(plot.title = element_text(hjust = 0.5)) +                       # Center the scatter plot title
  scale_x_continuous(name="Age", limits=c(25, 34),breaks=seq(25,34,1)) +                              # x-axis title, limits, lines
  scale_y_continuous(name="Average Household Earnings (1000s)", limits=c(0, 80),breaks=seq(0,80,10))  # y-axis title, limits, lines

## Nicer scatter plot for fitting quadratic function with the ggplot package
## Cubic function of degree 2 specified on the line where it states "formula = y ~ poly(x,3)"
ggplot(mydata1, aes(y=ahe, x=age)) +
  geom_point(alpha = .3) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3), col="blue") +     # Fit a polynomial of DEGREE 3 (CUBIC)
  ggtitle("Relationship Between Earnings and Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name="Age", limits=c(25, 34),breaks=seq(25,34,1)) +
  scale_y_continuous(name="Average Household Earnings (1000s)", limits=c(0, 80),breaks=seq(0,80,10))

#**********************************************************************************************
# ------------------ Q2, Q3 ------------------
# SEQUENTIAL ESTIMATION OF NONLINEAR MODEL
# Sequential hypothesis testing
#**********************************************************************************************

## Quartic earnings-age relationship, holding education and gender fixed
reg1=lm(ahe~age+age2+age3+age4+bachelor+female,data=mydata1)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))

## Cubic earnings-age relationship, holding education and gender fixed
reg2=lm(ahe~age+age2+age3+bachelor+female,data=mydata1)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

## Quadratic earnings-age relationship, holding education and gender fixed
reg3=lm(ahe~age+age2+bachelor+female,data=mydata1)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))

## Linear earnings-age relationship, holding education and gender fixed
reg4=lm(ahe~age+bachelor+female,data=mydata1)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))

## Quadratic earnings-age relationship, without controling for education or gender
reg5=lm(ahe~age+age2,data=mydata1)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))

## Regression output table for the 4 regressions reg1 reg2 reg3 reg4 reg5
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, reg3, reg4, reg5, type="text",
          se=list(se1, se2, se3, se4, se5),
          digits=2, 
          dep.var.labels=c("Annual Household Earnings (AHE)"),
          covariate.labels=
            c("Age",
              "Age Squared",
              "Age Cubed",
              "Age Quartic",
              "Bachelor Degree",
              "Female",
              "Constant"),
          out="reg_output.txt")   # Output results to your director in a text file

## Compute standard errors for reg3 based on the reg_output.txt file
## to obtain the t-statistic on Age Squared for the test of nonlinearity in the relationship
## between AHE and Age (relevant t-stat in the results is -2.3960)
coeftest(reg3, vcov = vcovHC(reg3, "HC1")) 

## Overall regression F-statistic for the quadratic regression, accounting for heteroskedasticity
linearHypothesis(reg3,c("age=0",
                        "age2=0",
                        "bachelor=0",
                        "female=0"),vcov = vcovHC(reg3, "HC1"))

# y = b1*age
# d y/d age = b1                (before)
# y = b1*age + b2*age^2
# d y/d age = b1 + 2*b2*age      (polynomial)

#**********************************************************************************************
# ------------------ Q4 ------------------
# COMPUTE NONLINEAR PARTIAL EFFECTS
#**********************************************************************************************

## Re-run quadratic regression with controls for computing partial effects
reg = lm(ahe~age+age2+bachelor+female, data=mydata1)

#**********************************************************************************************
# ------------------ Q5 ------------------
#**********************************************************************************************
# Create two new data frames for age=25 and age=28 for computing the partial 
# effect of going from age=25 and age=28 years hold. Notice that to compute 
# the partial effect based on the regression model 'reg' we just estimated, 
# we need to choose values for 'age', 'age2', 'bachelor' and 'female' in the new 
# data frames. In the 'reg' model we estimated, the values of 'bachelor' and 'female'
# do not affect the partial effect of age on ahe, so we can pick any values
# for 'bachelor' and 'female' in constructing the new data frames. In the lecture note
# solutions, I show this mathematical. Given this, I just set them both to 0, but you can 
# set 'bachelor' and 'female' to any values you like and it won't affect the computed partial
# effect of ahe and age

# a. age changes from 25 (newdata1) to 28 (newdata2)  

# We construct dataframes (datasets) with the data.frame() command
# Here, we are constructing a very simple dataset with 1 observation, and values for
# age, age2, bachelor, and female that you, the user, specify. With this one observation
# we can use the 'reg' model we estimated above to predict the value of ahe evaluated
# at the observation that you specified with the data.frame() command

## Construct dataframe for predicting ahe based on 'reg' model for age=25 
newdata1 = data.frame(age=25, age2=25*25, bachelor=0,female=1)
# y1 = b0 + b1 25 + b2 25^2 + b3 bachelor + b4 female

## Construct dataframe for predicting ahe based on 'reg' model for age=28 
newdata2 = data.frame(age=28, age2=28*28, bachelor=0,female=1)
# y2 = b0 + b1 28 + b2 28^2 + b3 bachelor + b4 female

# y2 - y1 = b1 28 + b2 28^2 - (b1 25 + b2 25^2)

# Combining the one-observation datasets we just created, newdata1 and newdata2, 
# we now use the predict() command to computed the predicted values of ahe using our estimated
# nonlinear regression model, evaluated at the values for age, age2, bachelor, female
# that we specified in our dataframes, newdata1 and newdata2

## Compute the predicted value of ahe for age=25 based on 'reg' model and values
## of age, age2, bachelor, and female in newdata1
ahe1=predict(reg, newdata=newdata1)

## Compute the predicted value of ahe for age=28 based on 'reg' model and values
## of age, age2, bachelor, and female in newdata2
ahe2=predict(reg, newdata=newdata2)

## Compute partial effect on ahe from changing from age=25 to age=28
(dahe=ahe2-ahe1)

# b. age changes from 28 (newdata1) to 31 (newdata2)
newdata1 = data.frame(age=28,age2=28*28,bachelor=1,female=0)
newdata2 = data.frame(age=31,age2=31*31,bachelor=1,female=0)
ahe1 = predict(reg, newdata=newdata1)
ahe2 = predict(reg, newdata=newdata2)
(dahe=ahe2-ahe1)

# c. age changes from 31 (newdata1) to 35 (newdata2)
newdata1=data.frame(age=31,age2=31*31,bachelor=1,female=1)
newdata2=data.frame(age=35,age2=35*35,bachelor=1,female=1)
ahe1=predict(reg, newdata=newdata1)
ahe2=predict(reg, newdata=newdata2)
(dahe=ahe2-ahe1)

#**********************************************************************************************
# ------------------ Q6 ------------------
# Standard errors of nonlinear partial effects
# and Confidence interval
#**********************************************************************************************

# from age=25 to age=28 years old, holding education and gender fixed

# Need to compute the Fstatistic for the joint test of the null that the predicted
# partial effect of the change in age from 25 to 28 on ahe 

# se(D y) = se(?)
# y1 = b0 + b1 25 + b2 25^2 + b3 bachelor + b4 female
# y2 = b0 + b1 28 + b2 28^2 + b3 bachelor + b4 female
# D y = y2 - y1 = b1 28 + b2 28^2 - (b1 25 + b2 25^2)


## Fstatistic for the joint test of the null that dahe=0
#  dahe = (28 b1 + 28^2 b2) - (25 b1 + 25^2 b2)
#  dahe = 3 b1 + (28^2-25^2) b2
#  se(dahe) = se(3 b1 + (28^2-25^2) b2)

Ftest=linearHypothesis(reg,c("3*age+159*age2=0"),vcov = vcovHC(reg, "HC1"))

## Recover the Fstat from the joint test results in Ftest
Fstat = Ftest[2,3]

## Compute the standard error for the partial effect we computed, dahe 
# (see slide 22 of Lecture note 8) 
se_dahe = abs(dahe)/sqrt(Fstat)

## 95% CI for the partial effect we computed, dahe
dahe_ci95L=dahe-se_dahe*1.96
dahe_ci95H=dahe+se_dahe*1.96
dahe_ciwidth=dahe_ci95H-dahe_ci95L

## Outputting results
sprintf("partial effect: %f", dahe)
sprintf("SE of partial effect: %f", se_dahe)
sprintf("95 CI lower bound for partial effect: %f", dahe_ci95L)
sprintf("95 CI upper bound for partial effect: %f", dahe_ci95H)
sprintf("Width of 95 CI for partial effect: %f", dahe_ciwidth)

# try the following:
# b. age changes from 28 (newdata1) to 31 (newdata2): 

# why null is 3*age+177*age2=0 ? 

# c. age changes from 31 (newdata1) to 35 (newdata2):

# why null is 4*age+264*age2=0 ? 


############### Notes ############### 
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching section
# or my github: https://github.com/zhengf1/2025S2ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required

