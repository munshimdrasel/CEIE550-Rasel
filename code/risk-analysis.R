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


hist(dulles$MonthlyMeanTemperature, freq =F, breaks= 30,
     xlab = "Monthly Mean Temperature (F)", main = "Histogram of Temperature at Dulles")

hist(log(dulles$MonthlyMeanTemperature), freq =F,
     xlab = "Monthly log Mean Temperature (F)", main = "Histogram of log Temperature at Dulles")


#mean & SD

mean (dulles$MonthlyMeanTemperature)

sd(dulles$MonthlyMeanTemperature)



