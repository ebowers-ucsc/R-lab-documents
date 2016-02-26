#ECON 294A: Assignment 5
#Emily Bowers

#Question 1
install.packages("ggplot2")
library(ggplot2)

head(diamonds)
#1a
graph1a<-ggplot(diamonds,
            aes(x = log(x*y*z), y = log(price)))
graph1a + geom_point()
graph1a + geom_point(aes(colour = clarity, size = carat, alpha = 0.5))
#1b
install.packages("reshape2")
library(reshape2)
hist1b<-ggplot(diamonds, aes(x = carat, y = ..density..)) + facet_wrap(~cut)
hist1b + geom_histogram(aes(fill = clarity)) + facet_grid(cut ~ .)
#1c
graph1c<-ggplot(diamonds, aes(x = cut, y = price))
graph1c + geom_violin() + geom_jitter(alpha = .0123)

#Question 2
library(foreign)
org<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

library(dplyr)
#2a
org2a <-org %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarize(
    rw_median = median(rw, na.rm=TRUE),
    rw.q3 = quantile(rw, 0.75, na.rm = TRUE),
    rw.q1 = quantile(rw, 0.25, na.rm = TRUE),
    rw.d1 = quantile(rw, prob=1/10, na.rm = TRUE, type = 5),
    rw.d9 = quantile(rw, prob=9/10, na.rm = TRUE, type = 5)
    )
org2a$date<- as.Date(paste(org2a$month, org2a$year, "01", sep="."), format = "%m.%Y.%d")

graph2a <- ggplot(org2a, aes(x=date, y=rw_median))
graph2a + coord_cartesian(ylim=c(0,50)) + geom_line(size=1) + geom_ribbon(aes(ymin= org2a$rw.q1, ymax= org2a$rw.q3 , alpha=0.1)) + geom_ribbon(aes(ymin= org2a$rw.d1, ymax= org2a$rw.d9, alpha=0.1))

#2b
org2b <-org %>%
  dplyr::group_by(year, month, educ) %>%
  dplyr::summarize(
    rw_median= median(rw, na.rm=TRUE))
org2b$date<- as.Date(paste(org2b$month, org2b$year, "01", sep="."), format = "%m.%Y.%d")

ggplot(data=org2b, aes(x=date, y=rw_median, group = educ, colour = educ)) + geom_line()
