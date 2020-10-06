#week5 hw risk analysis

library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library(measurements)
library(XML)
library(plyr)
library(RCurl)
library(lubridate)
library(ggpubr)


setwd("/Users/munshirasel/Google Drive/GMU/Fall2020/CEIE550/assignment-code/CEIE550-Rasel")



dulles <- read.csv(file = "data/dulles_airport_hw.csv")


head(dulles,100)
summary(dulles)

length(is.na(dulles$MonthlyMeanTemperature))


#plot frequencies as histogram

plot1 <- dulles %>% ggplot(aes(MonthlyMeanTemperature)) + geom_histogram (binwidth=2, color = "black") +
  labs(x= "Monthly Mean Temperature (F)",  y = "Count") + scale_x_continuous(breaks = seq(0, 90, by = 10))

ggsave("plot/plot1.png")

#log plot

dulles %>% ggplot(aes(log(MonthlyMeanTemperature))) + geom_histogram (binwidth=1, color = "black") +
  labs(x= "Monthly log Mean Temperature (F)",  y = "Count") + scale_x_continuous(breaks = seq(0, 90, by = 10))

dulles %>% ggplot(aes(log2(MonthlyMeanTemperature))) + geom_histogram (binwidth=1, color = "black") +
  labs(x= "Monthly log Mean Temperature (F)",  y = "Count")

dulles %>% ggplot(aes(log10(MonthlyMeanTemperature))) + geom_histogram (binwidth=1, color = "black") +
  labs(x= "Monthly log Mean Temperature (F)",  y = "Count")


#plot pdf as histogram


hist(dulles$MonthlyMeanTemperature, freq =F, breaks= 10,
     xlab = "Monthly Mean Temperature (F)", main = "Histogram of Temperature at Dulles Airport")

hist(log(dulles$MonthlyMeanTemperature), freq =F, breaks=10,
     xlab = "Monthly log Mean Temperature (F)", main = "Histogram of log Temperature at Dulles Airport")


#mean & SD

mean (dulles$MonthlyMeanTemperature)

sd(dulles$MonthlyMeanTemperature)


# c)Pr[ Temperature > 70 ] = ?)

dulles_70  <- dulles %>% filter (MonthlyMeanTemperature>70)


# dulles_70 <- dulles [MonthlyMeanTemperature>70]

N <-  length(dulles$MonthlyMeanTemperature)


length(dulles_70$MonthlyMeanTemperature)/N *100


# (Pr[ 30 < Temperature < 40 ])

dulles_30_40  <- dulles %>% filter (MonthlyMeanTemperature>30 & MonthlyMeanTemperature<40)

length(dulles_30_40$MonthlyMeanTemperature)/N *100


# (Pr[ Temperature = 50 ])
dulles_50  <- dulles %>% filter (MonthlyMeanTemperature==50)

length(dulles_50$MonthlyMeanTemperature)/N *100

#class problem

# flow <-  read.csv("/Users/munshirasel/Google Drive/GMU/Fall2020/CEIE550/week5/difficult_run_streamflow.csv")
#
#
# N <-  length(flow$X144762_00060_00003)
#
# flow_400 <-  flow %>% filter(X144762_00060_00003>400)
#
#
# summary(flow_400)
#
# length(flow_400$X144762_00060_00003)
#
# length(flow_400$X144762_00060_00003)/N *100
#
#
#
# length(flow_400)
