#### Assignment 3   #####
print ("Emily Bowers")
print ("1453007")
print ("ebowers@ucsc.edu")

#Question 1
library(foreign)
df.ex <- read.dta (file='https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta')

#Question 2
install.packages("dplyr")
library(dplyr)
dec13 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(dec13))

summer13 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(summer13))

#Question 3
df.ex.3a <- df.ex %>%
  dplyr::arrange(
    year, month
  )

#Question 4 
df.ex.4a <- select(df.ex, year:age)
df.ex.4b <- select(df.ex, year, month, starts_with("i"))
distinct (select(df.ex, state))       

#Question 5 

stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}
df.ex.5a <-df.ex %>%
  dplyr::mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count =n()
    )

df.ex.5b <- df.ex %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count = n()
  )

#Question 6 
df.ex.6 <- df.ex %>%
  dplyr::group_by(year, month, state) %>%
  dplyr::summarise(
    rw.min = min(rw, na.rm = T),
    rw.q1 = quantile(rw, 0.25, na.rm = T),
    rw.mean = mean(rw, na.rm = T),
    rw.median = median(rw, na.rm = T),
    rw.q3 = quantile(rw, 0.75, na.rm = T),
    rw.max = max(rw, na.rm = T),
    count = n()
  )
print(nrow(df.ex.6))

max_mean <- (max(df.ex.6$rw.mean))
row <- filter(df.ex.6, rw.mean== max_mean)
row_max <- select (row, year, month, state)
print(row_max)

#Question 7 -Extra credit
 
df.ex.7a <- df.ex %>%
  dplyr::arrange(
    year, month, desc(as.character(df.ex$state))
  )

save(EmilyBowersAssignment3, file = "EmilyBowersAssignment3.Rdata")