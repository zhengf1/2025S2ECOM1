#***************************************************************************** 
############### ECOM20001 Econometrics 1 Week 10
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
#****** Assignment 3 should be available.
#****** !! Please REGISTER your group !!
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
# setwd("~/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week10") # your path should be different 
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
mydata1=read.csv(file="tute10_cps.csv")

#**********************************************************************************************
# ------------------ Q1 ------------------
# LOGARITHMIC REGRESSIONS: as.numeric()
#**********************************************************************************************

## Create logarthimic variables for ahe and age
mydata1$log_ahe = log(mydata1$ahe)
mydata1$log_age = log(mydata1$age)

# The as.numeric() command can be used to create dummy variables in R
## Create year of sample dummy variables for the regression
mydata1$d1992 = as.numeric(mydata1$year==1992)  #d1992 is a dummy variable that equals one if year=1992, and 0 otherwise
mydata1$d2012 = as.numeric(mydata1$year==2012)  #d2012 is a dummy variable that equals one if year=2012, and 0 otherwise

# alternatively, instead of as.numeric, you can simply multiply by 1.
mydata1$d1992 = (mydata1$year==1992)*1

# ------------------ Q2 ------------------
## Regression of ahe on age + controls
reg1=lm(ahe~age+bachelor+female+d1992,data=mydata1)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))

## Regression of ahe on log(age) + controls
reg2=lm(ahe~log_age+bachelor+female+d1992,data=mydata1)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

## Regression of log(ahe) on age + controls
reg3=lm(log_ahe~age+bachelor+female+d1992,data=mydata1)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))

## Regression of log(ahe) on log(age) + controls
reg4=lm(log_ahe~log_age+bachelor+female+d1992,data=mydata1)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, reg3, reg4, type="text",
          se=list(se1, se2, se3, se4),
          digits=3, 
          dep.var.labels=c("AHE","Log(AHE)"),
          covariate.labels=
            c("Age",
              "Log(Age)",
              "Bachelor Degree",
              "Female",
              "1992 Dummy",
              "Constant"),
          out="reg_output1.txt")   # Output results to your director in a text file

#**********************************************************************************************
# ------------------ Q3 ------------------
# INTERACTIONS REGRESSIONS
#**********************************************************************************************

# CREATE INTERACTIONS

## Create interactive variables involving female, age, and bachelor
mydata1$female_age = mydata1$female * mydata1$age
mydata1$female_bachelor = mydata1$female * mydata1$bachelor

#**********************************************************************************************
# ------------------ Q4 ------------------
# RUN INTERACTIVE REGRESSIONS
#**********************************************************************************************

## Regression of ahe on age, allowing for differential gender effects with age, + controls
reg5=lm(ahe~age+bachelor+female+female_age+d1992,data=mydata1)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))

## Regression of ahe on age, allowing for differential gender effects with bachelor, + controls
reg6=lm(ahe~age+bachelor+female+female_bachelor+d1992,data=mydata1)
cov6=vcovHC(reg6, type = "HC1")    
se6=sqrt(diag(cov6))

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg5, reg6, type="text",
          se=list(se5, se6),
          digits=3, 
          dep.var.labels=c("AHE"),
          covariate.labels=
            c("Age",
              "Bachelor Degree",
              "Female",
              "Female x Age",
              "Female x Bachelor",
              "1992 Dummy",
              "Constant"),
          out="reg_output2.txt")   # Output results to your director in a text file

#**********************************************************************************************
# ------------------ Q5 ------------------
# ------------------ COMPUTE PARTIAL EFFECTS 
#**********************************************************************************************

# Note: the default here is to computing the partial effect of being female at age=28 and its standard error
# To compute the partial effect of being female at other age=XX, change age=XX and female_age=XX on line 96 and re-run 
# the code between lines 96 and 128

## Data frame for female aged 28, predicted value for ahe for female aged 28 based on reg7, other variables (bachelor, d1992) irrelevant
newdata1=data.frame(age=28,bachelor=1,female=1,female_age=28,d1992=0)
ahe1=predict(reg5, newdata=newdata1)

## Data frame for male aged 28, predicted value for ahe for male aged 28 bsaed on reg7, other variables (bachelor, d1992)  irrelevant
## (all female variables and interactions are 0 in this dataframe)
newdata2=data.frame(age=28,bachelor=1,female=0,female_age=0,d1992=0)
ahe2=predict(reg5, newdata=newdata2)

## Partial effect of being female at age 28
dahe= ahe1 - ahe2
print(dahe)

# ------------------ COMPUTE THEIR CONFIDENCE INTERVALS

## Fstatistic for Test of Difference in ahe if female=1 or female=0
Ftest = linearHypothesis(reg5,c("female+28*female_age=0"),vcov = vcovHC(reg5, "HC1"))

## Recover the Fstat from the joint test results in Ftest
(Fstat = Ftest[2,3])
 
## Compute the standard error for the partial effect we computed, dahe (see slide 21 of Lecture note 8) 
se_dahe=abs(dahe)/sqrt(Fstat)

## 95% CI for the partial effect we computed, dahe
dahe_ci95L=dahe-se_dahe*1.96
dahe_ci95H=dahe+se_dahe*1.96

## Outputting results
sprintf("partial effect of age for females: %f", dahe)
sprintf("SE of partial effect of age for females: %f", se_dahe)
sprintf("95 CI lower bound for partial effect of age for females: %f", dahe_ci95L)
sprintf("95 CI upper bound for partial effect of age for females: %f", dahe_ci95H)

############### Notes ############### 
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching section
# or my github: https://github.com/zhengf1/2025S2ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required

