# do some simple add
1 + 1
# assign a value to a variable
a = 20
# comments

print(a)
a

# print results and execute
(b = 5)

# 
a = 10
"c = 10"

# load data
# 1. set work directory (tell R location of data file)
setwd("/Users/zhengfan/Dropbox/01 UoM-Teaching/2025-S2-Ecom1/ECOM1_Week1")
# 2. load the data
getwd()
dt1 = read.csv("tute1_tutors.csv") 

View(dt1)
dim(dt1)
names(dt1)

mean(dt1$fav_number)
median(dt1$fav_number)

# index []
dt1[2,2]
dt1[,3]
dt1[1,]

