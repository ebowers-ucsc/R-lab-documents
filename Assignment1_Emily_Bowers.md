#Assignment 1, 294A
#Question 0
print('Emily Bowers, 1453007')
#Question 1: Load Data
library(foreign) 
df.dta <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta"
)
df.csv <- read.csv(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv"
)
df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"
)
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
print("The .rdata is called NHIS_2007_RData")

#Question 2, How big is each file
format(object.size(df.dta),units="KB")
print("df.dta is 452.3 KB")
format(object.size(df.csv),units="KB")
print("df.csv is 188.5 KB")
format(object.size(df.td),units="KB")
print("df.td is 506.4 KB")
format(object.size(NHIS_2007_RData),units="KB")
print("NHIS_2007_RData is 188.5 KB")
print("The csv and the .RData files are the smallest")

#Question 3, typeof and class of the .rdata
typeof(NHIS_2007_RData)
print("The .rdata is list type")
class(NHIS_2007_RData)
print("The .rdata is data.frame class type")
length(NHIS_2007_RData)
print("The length of the .rdata is 9")
dim(NHIS_2007_RData)
print("The dimensions of the .rdata is 4785 by 9")
nrow(NHIS_2007_RData)
print("The number of rows is 4785")
ncol(NHIS_2007_RData)
print("The number of columns is 9")
summary(NHIS_2007_RData)
print(summary(NHIS_2007_RData))

#Question 4
df<- read.dta(
  file= "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
print(str(df))
print("There are 1,119,754 observations and 30 variabels")
min(df$rw, na.rm=TRUE)
mean(df$rw, na.rm=TRUE)
median(df$rw, na.rm=TRUE)
max(df$rw, na.rm=TRUE)
quantile(df$rw, na.rm=TRUE)
print("The min is 1.81, the mean is 19.81, the median is 15.88, the max is 354.8, the 1st quartile is 10.70, and the 3rd quartile is 24.36")
sum(is.na(df$rw))
print("There are 521,179 NA's")

#Question 5
v <- c(1,2,3,4,5,6,7,4,NULL,NA)
length(v)
print("The reported length of v is 9")
print("The number of values in the vector and the number reported in length don't match because Null gets dropped while NA remains in the vector.")
mean(v, na.rm=T)
print("The mean of v is 4")

#Question 6
x<- matrix(c(1,4,7,2,5,8,3,6,9),nrow=3, ncol=3)
t(x)
eigen(x)
y<- matrix(c(1,2,3,2,2,3,3,1,0), nrow=3)
y_inv<- solve(y)
y%*%y_inv
print("This is the indentity matrix")

#Question 7
carat = c(5,2,0.5,1.5,5,NA,3)
cut = c("fair","good","very good","good","fair","Ideal","fair")
clarity = c("SI1","I1","VI1","VS1","IF","VVS2","NA")
price = c(850,450,450,"NULL",750,980,420)
diamonds = data.frame(carat,cut,clarity,price)
print(diamonds)
priceNum <- as.numeric(as.character(diamonds$price))
mean(priceNum, na.rm =T) 
print("The mean price is 650")
diamond_Fair <- subset(diamonds, cut=="fair")
diamond_new <- as.numeric(as.character(diamond_Fair$price))
mean(diamond_new)
print("The mean price of cut fair is 673.33")
diamond_notfair <- subset(diamonds, cut !="fair")
diamond_nnf <- as.numeric(as.character(diamond_notfair$price))
mean(diamond_nnf, na.rm=T)
print("The mean price of cut good, very good, and ideal is 626.67")
diamond_1 <- subset(diamonds, carat>= 2 & cut =="very good" | cut=="Ideal")
diamond_2 <- as.numeric(as.character(diamond_1$price))
median(diamond_2, na.rm=T)
print("The median price is 980, but the carat value is NA")



