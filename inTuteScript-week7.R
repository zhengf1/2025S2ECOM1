#***************************************************************************** 
############### ECOM20001 Econometrics 1 Week 7
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.
#***************************************************************************** 

# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching section
# or my github: https://github.com/zhengf1/2025ECOM1

# please refer to Dave's R code for detailed explanations (for your self learning)
# it contains much more than the tutorial questions with extra examples,
# so please be patient when go through the code

#***************************************************************************** 
#****** Admin:
#***************************************************************************** 
#****** No tutorial next week
#****** Mid semester break
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
setwd("~/Dropbox/01 UoM-Teaching/2025-S1-Ecom1/ECOM1_Week7") # your path should be different 
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
#*
donotsmoke=read.csv("tute7_smoke.csv")

#**********************************************************************************************
# ------------------ Q1 ------------------
## CONSTANT REGRESSOR AND THE DUMMY VARIABLE TRAP
#**********************************************************************************************

## Create a new regressor that is the sum of tripre0, tripre1, tripre2, triepre2
## This is also known as the "Constant Regressor" denoted X0 from Lecture Note 6
donotsmoke$constant = donotsmoke$tripre0 + donotsmoke$tripre1 +donotsmoke$tripre2 + donotsmoke$tripre3

## Summarize the constant variable - it's always 1!
summary(donotsmoke$constant)

#**********************************************************************************************
# ------------------ Q2 ------------------
## Simple linear regression of birthweight on alcohol
#**********************************************************************************************

reg1=lm(birthweight ~ alcohol, data=donotsmoke)
coeftest(reg1, vcov = vcovHC(reg1, "HC1")) 

# We can estimate regressions in R WITHOUT the constant by adding a "+0"

## Simple linear regression of birthweight on alcohol without constant
reg2=lm(birthweight ~ alcohol+0, data=donotsmoke)
coeftest(reg2, vcov = vcovHC(reg2, "HC1")) 

## Simple linear regression of birthweight on alcohol without constant, and with
## "constant=tripre0+tripre1+tripre2+tripre3" variable that we created
reg3=lm(birthweight~constant+alcohol+0,data=donotsmoke)
coeftest(reg3, vcov = vcovHC(reg3, "HC1")) 

# Notice how reg1 and reg3 produce identical results, thereby highlighting the role of
# the constant regressor in multiple linear regression
# The second regression differs because it does not contain a constant at all.

#**********************************************************************************************
# ------------------ Q3 ------------------
## FALLING INTO THE DUMMY VARIABLE TRAP
#**********************************************************************************************

# Try running the regression including our "constant" variable but not dropping
# the constant from the regression (e.g., do not have the "+0" part). This represents 
# a dummy variable trap because our "constant" variable is perfectly collinear 
# with the constant regressor on the model's intercept

## Trying to fall into dummy variable trap
reg4=lm(birthweight~constant+alcohol,data=donotsmoke)
coeftest(reg4, vcov = vcovHC(reg4, "HC1"))

## Re-run simple original regression without the constant variable
reg1=lm(birthweight~alcohol,data=donotsmoke)
coeftest(reg1, vcov = vcovHC(reg1, "HC1"))

# What happens? reg4 and reg1 yields the EXACT SAME results
# Why? Because R automatically drops the "constant" variable in reg4 to
# avoid the dummy variable trap. Notice how the program does not tell you that it does this
# So when running regressions with lots of dummy variables in R, it is important 
# to check with your inputted variable list and the regression output in coeftest() 
# that the inputted and outputted variable list is the same. If it is not, then R
# may have automatically dropped some of your dummy variables to avoid the dummy variable trap

#**********************************************************************************************
# ------------------ Q4 ------------------
#**********************************************************************************************

# Let's try to fall into the dummy variable trap again to cement ideas. This time we will
# include tripre0, tripre1, tripre2, tripre3 as regressors in our regression. Collectively
# these regressors are perfectly collinear with the constant regressor

## Regression trying to fall into the dummy variable trap again
reg6=lm(birthweight ~ alcohol+ tripre0+tripre1+tripre2+tripre3,data=donotsmoke)
coeftest(reg6, vcov = vcovHC(reg6, "HC1"))





dt1 = donotsmoke
reg6_alt = lm(birthweight ~ alcohol + tripre0 + tripre1 + tripre2, data = dt1)
coeftest(reg6_alt, vcov = vcovHC(reg6_alt, "HC1"))
mean(dt1$birthweight[dt1$alcohol == 0 & dt1$tripre3 == 1])

X_pred <- data.frame(
  alcohol = 0,
  tripre0 = c(1,0,0,0),
  tripre1 = c(0,1,0,0),
  tripre2 = c(0,0,1,0)
)
(X_pred$predicted = predict(reg6_alt, newdata = X_pred))

reg6_alt2 = lm(birthweight ~ alcohol, data = donotsmoke)
coeftest(reg6_alt2, vcov = vcovHC(reg6_alt, "HC1"))
mean(donotsmoke$birthweight[donotsmoke$alcohol == 0])

X_pred2 <- data.frame(
  alcohol = 0
)
(X_pred2$predicted = predict(reg6_alt2, newdata = X_pred2))

# Notice what happens: R automatically drops tripre3 to avoid the dummy variable trap, the last
# variable in the regression.

# This is important because it means that tripre3 becomes our omitted category (or "base group")
# meaning that the interpretation of tripre0, tripre1, tripre2 equaling one on birthweight is 
# RELATIVE to observations where tripre3==1. This is important for interpretation of our results:

# tripre0=-569.321 means RELATIVE to tripre3==1, babies with no prenatal care weigh 569 grams LESS
# than babies that had their first prenatal care in the 3rd trimester, and this difference is 
# statistically significant at the 1% level

# tripre1=180.603 means RELATIVE to tripre3==1, babies with their first prenatal care in the 1st 
# trimester weigh 180 grams MORE than babies that had their first prenatal care in the 3rd trimester,
# and this difference is statistically significant at the 1% level

# tripre2=55.707 means RELATIVE to tripre3==1, babies with their first prenatal care in the 2nd 
# trimester weigh 180 grams MORE than babies that had their first prenatal care in the 3rd trimester,
# and this difference is NOT statistically significant at the 1% level

# For interpretation, it might be easier to make our base group babies for whom tripre0==1, 
# that is, those who do not have any prenatal care. Here, as model builders, we explicitly omit
# tripre0 from the list of regressors to avoid the dummy variable trap and make it our base group 
# as opposed to having R simply choose the base group for us

#**********************************************************************************************
# ------------------ Q5 ------------------
## Regression where we explicitly make tripre0==1 the base group
#**********************************************************************************************

reg7=lm(birthweight ~ alcohol+tripre1+tripre2+tripre3,data=donotsmoke)
coeftest(reg7, vcov = vcovHC(reg7, "HC1"))

bw|tripre1 - bw|tripre0 (aclohlo = 1)
bw|tripre1 - bw|tripre0 (aclohlo = 0)

# Notice how the regression coefficient on alcohol is identical in reg6 and reg7; this is because
# we are still holding all other factors fixed in equivalent ways in estimating the alcohol
# regression coefficient, irrespective of what the chose base group among the tripre0, tripre1,
# tripre2, and tripre3 is.

# Also notice how the intercept and coefficients on tripre1 and tripre2 are different in reg6 and reg7,
# in particular they are much larger. Why? Because our base group is now the babies where tripre0==1
# meaning the interpretation of the tripre1, tripre2, and tripre3 coefficients are all relative to 
# babies without prenatal visits. 

# Explicitly provide the interpretations on the tripre1, tripre2, tripre3 coefficients
# in answering the tutorial questions similar to the interpretations provided above.

#**********************************************************************************************
# ------------------ Q6 ------------------
#**********************************************************************************************

# There is a clearer/easier interpretation of results in question 5 with a base 
# group of babies where tripre0==1 relative to question 4 than when our base group
# was babies where tripre3==1. It makes clear that having prenatal care in any 
# trimester has a large positive impact on birthweight relative to having no 
# prenatal care in any trimester at all.

#**********************************************************************************************
# ------------------ Q7 ------------------
#**********************************************************************************************

# The regression coefficients and standard errors on alcohol in the regressions 
# from questions 4. and 5. are identical.

#**********************************************************************************************
# ------------------ Q8 ------------------
#****** MULTICOLLINEARITY
#**********************************************************************************************

# 1. How many observations out of 3000 does tripre0 equal one for?

## Cross-tabulation of tripre0 and gambles
xtabs(~tripre0+gambles, data=donotsmoke)
cor(donotsmoke$tripre0,donotsmoke$gambles)

## 30 of 3000 observations have tripre0 equal one.

# 2. Among the observations where tripre0 equals one, how many have
#    gambles equal to one as well?

# Among these variables, 26 of 30 observations have gambles equal one as well. 

# 3. Comment on how your results raise concerns of possible imperfect 
#    multicollinearity between tripre0 and gambles in a regression where both 
#    are included as independent variables.

# The high degree of correlation between the two variables raises a concern of 
# imperfect multicollinearity between tripre0 and gambles in a regression where 
# both are included as independent variables.

# 4. Further comment on the imperfect multicollinearity concerns between 
#    gambles and tripre1, tripre2, tripre3 together in a regression where all 
#    are included as independent variables.

# Given that tripre0=1-tripre1-tripre2-tripre3, this directly implies an imperfect 
# multicollinearity concerns between gambles and tripre1, tripre2, tripre3 
# together in a regression where all are included as independent variables.

#**********************************************************************************************
# ------------------ Q9 ------------------
#**********************************************************************************************
reg1=lm(birthweight~smoker+alcohol+drinks+gambles+unmarried+educ+age,data=donotsmoke)  # Regression estimates for reg1
cov1=vcovHC(reg1, type = "HC1")     # The next 2 lines produce heteroskedasticity-robust standard errors for reg1
se1=sqrt(diag(cov1))

reg2=lm(birthweight~smoker+alcohol+drinks+gambles+nprevisit+unmarried+educ+age,data=donotsmoke)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

reg3=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))

reg4=lm(birthweight~smoker+alcohol+drinks+gambles+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age,data=donotsmoke)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))

## Regression output table for the 4 regressions reg1 reg2 reg3 reg4
# Discuss the results from stargazer() as asked in the question on the assignment sheet.

stargazer(reg1, reg2, reg3, reg4, type="text",
          se=list(se1, se2, se3, se4),
          digits=2, 
          dep.var.labels=c("Baby Birthweight in Grams"),
          covariate.labels=
            c("Smoker",
              "Drinks Alcohol During Pregnancy",
              "Drinks per Week During Pregnancy",
              "Gambles",
              "Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age",
              "Constant"),
          out="reg_output.txt")   # Output results to your director in a text file

############### Notes ############### 
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required



