#Assignment 4: ECON 294 A
#Emily Bowers

#Question 0
print ("Emily Bowers")
print ("1453007") 
print ("ebowers@ucsc.edu")

#Question 1
flights <- read.csv (file='C:/Users/emem/Documents/MS_UCSC/R-Lab/flights.csv', stringsAsFactors=FALSE)
planes<- read.csv (file='C:/Users/emem/Documents/MS_UCSC/R-Lab/planes.csv', stringsAsFactors=FALSE)
weather <- read.csv (file='C:/Users/emem/Documents/MS_UCSC/R-Lab/weather.csv', stringsAsFactors=FALSE)
airports <- read.csv (file='C:/Users/emem/Documents/MS_UCSC/R-Lab/airports.csv', stringsAsFactors=FALSE)


#Question 2
as.Date(flights$date)
as.Date(airports$date)
as.Date(planes$date)
as.Date(weather$date)

#Question 3
install.packages("dplyr")
library(dplyr)
unique(flights$dest)
#went to San Francisco or oakland, CA
flights.2a<- dplyr::filter(flights, dest=="SFO" | dest=="OAk")
nrow(flights.2a)
print("There are 2818 observations in flights.2a")

#fligths delayed by an hour or more
flights.2b <- dplyr::filter(flights, arr_delay>60)
print(nrow(flights.2b))

#arrival delay was more than twice as much as the departure delay
flights.2c<-dplyr::filter(flights, arr_delay>=2*dep_delay)
print(nrow(flights.2c))

#Question 4

#using select ()'s helper function, 3 different ways to select the delay variables from flights 
install.packages("plyr")
library(plyr)
#select columns by name 
select(flights, dep_delay, arr_delay)
#select columns between two variable (inclusive)
select(flights,dep_delay:arr_delay)
select(flights, contains("delay"))    
select(flights, matches("delay"))


#Question 5
arrange(flights, -dep_delay)
head(flights,n=5)
flights.5b<-flights
flights$delaydiff<-(flights.5b$dep_delay-flights.5b$arr_delay)
flights.5b<-arrange(flights.5b,-delaydiff)
head(flights.5b, n=5)

#top 5 most Departure delayed flights
arrange(flights, desc(dep_delay))
head(flights$dep_delay, 5)
#top 5 flights that caught up the most during the flight
Catchup<-(flights$dep_delay - flights$arr_delay)
arrange(flights, desc(Catchup))
print(Catchup[1:5])


#Question 6 - mutate()
flights <- flights%>% 
  mutate(newthing= time/60,
         speed=dist/newthing,
         delta= dep_delay - arr_delay)

#6a- top 5 flighs by speed
arrange(flights, speed)
head(flights$speed, 5)
#6b - top  flights that made up the most time in flight(should match 5b)
arrange(flights, newthing)
head(flights$newthing, 5)
#6c- top flights that lost the most time in flight
arrange(flights, desc(newthing))
head(flights$newthing, 5)

#Question 7
flights.7a <- flights %>%
  dplyr::group_by(carrier) %>%
  dplyr::summarize(
    flight.cancled = sum(cancelled, na.rm= TRUE),
    flights.total= n(),
    flights.percent= 100*(sum(cancelled, na.rm= TRUE)/n()),
    delta.min = min(delta, na.rm = TRUE),
    delta.1stq = quantile(delta, 0.25, na.rm = TRUE),
    delta.mean = mean(delta, na.rm = TRUE),
    delta.median = median(delta, na.rm = TRUE),
    delta.3rdq = quantile(delta, 0.75, na.rm = TRUE),
    delta.90 = quantile(delta, 0.90, na.rm=TRUE),
    delta.max = max(delta, na.rm = TRUE)
  )

arrange(flights.7a, desc(flights.percent))
summary(flights.7a)


day_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%group_by(date)%>%
  summarize(
    delay = mean(dep_delay),
    n=n()
  ) 
    

#Question 8
arrange(day_delay, date)
delay.8 <- day_delay %>% 
  mutate(today.delay = delay,
         yest.delay =  lag(delay,1),
         increase_delay = (today.delay - yest.delay))

arrange(delay.8, increase_delay)
head(delay.8$increase_delay,5)

#Question 9
dest_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%group_by(dest)%>%
  summarize(
    arr = mean(arr_delay),
    n = n())

airports<- airports%>%
  rename(dest = iata, name = airport)

#9a- top 5 city and states with the highest average arrival delays 
df.9a <- left_join(dest_delay, airports, by=c("dest"="dest"))
df.9a <- arrange(df.9a, desc(arr))
head(df.9a,5)
nrow(df.9a)

#9b
df.9b<-inner_join(dest_delay, airports, by=c("dest"="dest"))
df.9b<-arrange(df.9b, desc(arr))
head(df.9b,5)
nrow(df.9b)
print("Left join gives 116 observations and inner join gives 114 these are not the same")
#9c
df.9c<-right_join(dest_delay, airports, by=c("dest"="dest"))
df.9c<-arrange(df.9c, desc(arr))
head(df.9c,5)
nrow(df.9c)
Print("When we use right join there's 3376 observations. There are NA's in the arr_delay becausethere are a different number of observations in the two that we are joining.")
#9d
df.9d<-full_join(dest_delay, airports, by=c("dest"="dest"))
df.9d<-arrange(df.9d, desc(arr))
head(df.9d,5)
nrow(df.9d)
print("There are 3378 observations using full join. There are NA's because airports and dest_delay have a different number of observations.")

#Question 10
hourly_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%group_by(date, hour)%>%
  summarize(
    mean = mean(dep_delay,na.rm=TRUE)
  )    

hourly_delay$date<- as.Date(hourly_delay$date)
weather$date<- as.Date(weather$date)

Merged <- left_join(hourly_delay, weather, by=c("date"="date"))

#Question 11
#11a
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)

#11b
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))

#11c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m","m"),
  age = c(15,50,45,18),
  state = c("CA","NY","HI","DC"),
  value = c(3,4,5,6)
)
#11d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
data.frame(
  subject = c(1,2,3,4),
  demo = c("f.11.DC", "f.55.NY", "m.65.WA", NA),
  value = c(3,4,5,6)
)
