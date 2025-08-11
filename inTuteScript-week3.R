#***************************************************************************** 
#****** ECOM20001 Econometrics 1 Week 3
#****** by Zheng Fan; fan.z@unimelb.edu.au
#****** Contact me if you have any questions.
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
#****** Admin:
#***************************************************************************** 
#****** Keep in mind you group assignment 1. 
#****** due in Monday Week 4 (next week) 
#****** If failed to form a group on time, 
#****** but want to form a group,
#****** email ecom-1@unimelb.edu.au
#***************************************************************************** 
#***************************************************************************** 


#**********************************************************************************************
# --------- Question C.0. --------- 
## Load dataset
#**********************************************************************************************
# remove everything in the environment to start a new project
rm(list=ls()) 

# to read data, we MUST set work directory to tell R the location
# of our data file.
# setwd("C:/Users/zhefan/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week3")
setwd("/Users/zhengfan/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week3")
# yours should be different

## Load the dataset from a comma separate value
data = read.csv("tute3_cps.csv")
 
## Dimension of the dataset
# 15052 observations (individuals)
# 5 variables: year, ahe, bachelor, female, age
dim(data)

# --------- INDEXING 
## Index observation 1 for ahe
data$bachelor
data$ahe
data$ahe[1]    # observation 1 for ahe

## Index observations 1 to 5 for ahe
data$ahe[1:5]

# --------- SUBSETS OF DATA
# Suppose we want to just look at ahe for females
# We can get all the observations that corresponds to females with female==1
# data$ahe[data$female==1] is ahe for females
# data$ahe[data$female==0] is ahe for males

data$female == 1
# convert into a dummy variable?
(data$female == 1) *1
# eg.
(data$year == 2012) *1


data$ahe[data$female == 1]
data$ahe[data$female == 1 & data$bachelor == 1]

female_ahe = data$ahe[data$female == 1]
notfemale_ahe = data$ahe[data$female == 0]

# idea: P(ahe) = p(ahe|female)p(female) + p(ahe|notfemale)p(female)

#**********************************************************************************************
# --------- Question C.1. --------- 
#**********************************************************************************************

## Mean and standard deviation of earnings for females
mean(data$ahe[data$female==1])
sd(data$ahe[data$female==1])

## Mean and standard deviation of earnings for males
mean(data$ahe[data$female==0])
sd(data$ahe[data$female==0])

# compare the distributions on earnings from each groups (gender)
plot(density(female_ahe), 
     col="red",
     lty=1,                         # lty: line style, ie solid, dashed, dotted ,etc   
     xlab="Average hourly earning",
     main="Gender and Earnings")
lines(density(notfemale_ahe), col="blue",lty=2)
legend("topright",                 # position of legend   
       legend=c("Female", "Male"), # match the order 
       col=c("red", "blue"),       # match the order 
       lty=c(1,2))                 # match the order 
# IMPORTANT NOTICES:
# Do NOT crop unless you have valid and strong reason
# Do NOT cover lines
# ...

#**********************************************************************************************
# --------- Question C.2. --------- 
#**********************************************************************************************

## Mean and standard deviation of earnings for bachelor degree
mean(data$ahe[data$bachelor==1])
sd(data$ahe[data$bachelor==1])

## Mean and standard deviation of earnings for no bachelor degree
mean(data$ahe[data$bachelor==0])
sd(data$ahe[data$bachelor==0])

# compare the distributions on earnings from each groups (bachelor)
plot(density(data$ahe[data$bachelor==0]), col="red",lty=1,xlab="Average hour income",main="Education and Earnings")
lines(density(data$ahe[data$bachelor==1]), col="blue",lty=1)
legend("topright", legend=c("No Bachelor Degree", "Bachelor Degree"), col=c("red", "blue"), lty=c(1,1))

#**********************************************************************************************
# --------- Question C.3. --------- 
#**********************************************************************************************
# Confidence interval and Hypothesis testing
# Xbar +- critical_value* se(Xbar)
# var(Xbar) = var(sum X1+..Xn / n) 
# = var(sum X1+..Xn) ./ n^2
# = sum var( X1+..Xn) ./ n^2
# = n * var(X) / n^2
# = var(X)/n
# CI: Xbar +- critical_value* sd(X)/sqrt(n)
ahe_mu = mean(data$ahe)               # Sample mean of ahe
n = dim(data)[1]
ahe_nobs = length(data$ahe)             # Number of observations; length() returns the number of obs in ahe
ahe_sd = sd(data$ahe)                   # Sample standard deviation of ahe
ahe_se = ahe_sd/sqrt(ahe_nobs)          # Standard error of the sample mean
test_sta = (ahe_mu - 19.5) / ahe_se
(critial_value = qnorm(0.975))
ahe_CI95_low = ahe_mu - critial_value * ahe_se       # Lower bound of the 95% CI
ahe_CI95_high = ahe_mu + critial_value * ahe_se      # Upper bound of the 95% CI
c(ahe_CI95_low, ahe_CI95_high)

## Test the null that the true value of the mean of ahe is 19.5 (example)
t.test(data$ahe, mu=19.5) # default is a two sided test
t.test(data$ahe, mu=19.5, alternative = "two.sided" )
t.test(data$ahe, mu=19.5, alternative = "greater" )
t.test(data$ahe, mu=19.5, alternative = "less" )

# manual calculate p-value for right-tail test
t_act = (ahe_mu-19.5)/ahe_se  # t-statistic
pvalue1 = 1 - pnorm(t_act)      # compute p-value
paste("One-sided p-value for greater than (>) alternative:",pvalue1)

#**********************************************************************************************
# --------- Question C.4. --------- 
## Test difference of means in ahe for male and female without bachelor in 2012
#**********************************************************************************************
# Gender wage gap in 2012 among people without bachelor degree
mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0])
mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])
diff1=mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0])-mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])

t.test(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0],
       data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])

sub1 = data$ahe[data$female==1 & data$year==2012 & data$bachelor==0]
sub2 = data$ahe[data$female==0 & data$year==2012 & data$bachelor==0]
mu_sub1 = mean(sub1)
sd_sub1 = sd(sub1)
se_sub1 = sd_sub1 / sqrt(length(sub1))
mu_sub2 = mean(sub2)
sd_sub2 = sd(sub2)
se_sub2 = sd_sub2 / sqrt(length(sub2))
test_statistics = (mu_sub1 - mu_sub2 - 0) / sqrt(se_sub1^2+se_sub2^2)

crt_val =  qnorm(0.975)
p_value2 = pnorm( test_statistics )

#**********************************************************************************************
# --------- Question C.5. --------- 
## Test difference of means in ahe for male and female without bachelor in 2012
#**********************************************************************************************
# Gender wage gap in 2012 among people with bachelor degree
mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1])
mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])
diff2=mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1])-mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])

t.test(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1],
       data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])

# Graphically: difference in gender wage gap depending on education in 2012
 # pdf("ahe_female_bachelor_2012.pdf")
plot(density(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0]), col="red", lty=1,
     main="Gender, Education, and Earnings in 2012", xlab="Hourly Earnings")
lines(density(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0]), col="blue", lty=1)
lines(density(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1]), col="red", lty=2)
lines(density(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1]), col="blue", lty=2)
legend("topright", legend=c("Female, No Degree", "Male, No Degree", "Female Degree", "Male Degree"),
       col=c("red", "blue", "red", "blue"), lty=c(1,1,2,2))
 # dev.off()


#**********************************************************************************************
# --------- PART 2. --------- 
# --------- Question 1 --------- 
#**********************************************************************************************

(n = dim(data)[1]) # sample size
idx = sample(n,5000) # draw 5000 random sample from 1 to n
sample_data = data[idx,]

# the rest are exercise, which are similar to what we
# have discussed before.

#**********************************************************************************************
############### Notes ############### 
#**********************************************************************************************
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025S2ECOM1

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required




