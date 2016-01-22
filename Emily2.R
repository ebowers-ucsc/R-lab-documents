#Assignment 2: ECON 294A
#Emily Bowers

#Question 0
EmilyBowersAssignment2 <- list(
  firstName = "Emily",
  lastName = "Bowers",
  email = "ebowers@ucsc.edu",
  studentID = 1453007
) 

#Question 1
diamonds <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)

EmilyBowersAssignment2$s1a<-nrow(diamonds)
EmilyBowersAssignment2$s1b<-ncol(diamonds)
EmilyBowersAssignment2$s1c<-names(diamonds)
EmilyBowersAssignment2$s1d<-summary(diamonds$price)

#Question 2
df <- read.table(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt", header=TRUE)
EmilyBowersAssignment2$s2a<-nrow(df)
EmilyBowersAssignment2$s2b<-ncol(df)
EmilyBowersAssignment2$s2c<-names(df)
EmilyBowersAssignment2$s2d<-mean(df$weight)
EmilyBowersAssignment2$s2e<-median(df$weight)
column<-ifelse (test = df$weight >=996 & df$weight<=999, yes=NA, no=df$weight)
EmilyBowersAssignment2<-hist(column)
EmilyBowersAssignment2$s2f<-mean(column, na.rm=TRUE)
EmilyBowersAssignment2$s2g<-median(column, na.rm=TRUE)
cool <- subset(df, SEX==1 & weight<990)
EmilyBowersAssignment2$s2h<-summary(cool$weight)
beans <- subset(df, SEX==2 & weight<990)
EmilyBowersAssignment2$s2i<-summary(beans$weight)

#Question 3
vec <- c(letters,LETTERS)
as.factor(vec)
newvec <- vec[seq(2, length(vec), 2)]
EmilyBowersAssignment2$s3a <- print(newvec)
EmilyBowersAssignment2$s3b<-paste(vec[c(31,13,9)], collapse="")
arr<- array( c(letters,LETTERS), dim = c(3,3,3))
EmilyBowersAssignment2$s3c<-arr[,1,2]
EmilyBowersAssignment2$s3d<-arr[2,2,]
EmilyBowersAssignment2$s3e<-paste(arr[2,2,1],arr[1,2,2],arr[3,3,1],sep = "")

#Question 4
library(foreign)
df.dta <- read.dta(
  file = "http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta"
)
EmilyBowersAssignment2$s4<-
  
  #To get all answers
  print(EmilyBowersAssignment2)  
save(EmilyBowersAssignment2, file = "EmilyBowersAssignment2.Rdata")
#End of file again
