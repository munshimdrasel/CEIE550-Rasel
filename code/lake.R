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



setwd("/Users/munshirasel/Google Drive/GMU/Fall2020/CEIE550/CEIE_450_550-master")
V0 <- 8*10^14

# time step, number of steps, and time vector
dt <- 1 #day
n.steps <- 100
time <- seq( from = 0, by = dt, length.out = n.steps + 1)



#Lake without any incident of rainstorm

Qi <-  c (9.504* 10^13, rep ( 9.504*10^13, 100))
Qo <-  c (9.504* 10^13, rep ( 9.504*10^13, 100))
Vi <-  c (8*10^14, rep (8*10^14,100))

# Plotting figure when there is no incident of rainstorm
par( mfrow = c( 1, 2))

plot( time, Qi,
      # plot aesthetics
      ylim = c( 0, 1*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake inflow (no rainstorm)',
      ylab = 'Inflow, ft^3/day',
      xlab = 'time, day')
plot( time, Qo,
      # plot aesthetics
      ylim = c( 0, 1*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake outflow(no rainstorm) ',
      ylab = 'Outflow, ft^3/day',
      xlab = 'time, day')
plot( time, Vi,
      # plot aesthetics
      ylim = c( 0, 8.5*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake Volume (no rainstorm)',
      ylab = 'Volume, cft',
      xlab = 'time, day')

par( mfrow = c( 1, 1))


#Lake without any incident of rainstorm (outflow increases)

Qi <-  c (9.504* 10^13, rep ( 9.504*10^13, 100))
Qo <-  c (9.504*10^13, rep ( 9.504*10^13, 20), rep ( 10*10^13, 80) )
# Vi <-  c (8*10^14, rep (8*10^14,100))


V <- c()
V[1] <- V0

# calculate volumes
for( t in 1:n.steps){
  V[t + 1] <- V[t] + (Qi[t + 1] - Qo[t + 1]) * dt
}




# Plotting figure when there is no incident of rainstorm
par( mfrow = c( 1, 2))

plot( time, Qi,
      # plot aesthetics
     ylim = c( 0, 1*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake inflow (no rainstorm)',
      ylab = 'Inflow, ft^3/day',
      xlab = 'time, day')
plot( time, Qo,
      # plot aesthetics
      ylim = c( 0, 1.2*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake outflow(no rainstorm) ',
      ylab = 'Outflow, ft^3/day',
      xlab = 'time, day')
plot( time, V,
      # plot aesthetics
      ylim = c( 0, 8.5*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake Volume (no rainstorm)',
      ylab = 'Volume, cft',
      xlab = 'time, day')

par( mfrow = c( 1, 1))


#once a week there is a rainstorm that increases flow by 40% in both inflow rivers for 24 hours. 
#assuming on very week on day #7 rainstorm happens

Qi <- c( 9.504*10^13, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14,
         rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14,
         rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14,
         rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14,
         rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 6), 1.33056*10^14, rep( 9.504*10^13, 2))

Qo <- c( 9.504*10^13, rep( 9.504*10^13, 100))

## ========================================= ##
#      calculate the volume in the lake across time
## ========================================= ##
# initialize volume vector

# V <- c()
# V[1] <- V0
# 
# # calculate volumes 
# for( t in 1:n.steps){
#   V[t + 1] <- V[t] + (Qi[t + 1] - Qo[t + 1]) * dt
# }
# 
#If there was no control on outflow
plot( time, Qi,
      # plot aesthetics
      ylim = c( 0, 1.3*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake inflow over time (rainstorm happened on every 7th day)',
      ylab = 'Inflow, ft^3/day',
      xlab = 'time, day')
# x <-  as.data.table(data.frame(time=time, volume=V))
# 
# # •	What is the first day that the plant operator will need to open the spillway?
# x %>% filter (volume >9*10^14)

# on day 21 the bathtub will reach it's 90% capcity when operator will open the spillway


# •	What is the maximum water volume in the lake over 100 days? What percent of the capacity is this?

## ========================================= ##
#   Feedback #2: balancing + reinforcing
#      calculate the volume in the tub across time
#      with feedback loop on inflow:
#        Qin ==  2 if V[t-1] >  45
#        Qin ==  6 if V[t-1] <= 45
#        Qout == 10 if V[t-1] >= 45
#        Qout == 8  if V[t-1] <  45
## ========================================= ##
# initialize volume vector (fb2 stands for feedback 2)
V_fb2 <- c()
V_fb2[1] <- V0

# we'll start by assuming Qi_fb2 and Qo_fb2 are the same as 
# Qi and Qo in the base case

Qo_fb2 <- Qo

# calculate volumes 
for( t in 1:n.steps){
# Qi_fb2[t + 1] <- ifelse( V_fb2[t] >  9*10^14,   9.504*10^13)
  Qo_fb2[t + 1] <- ifelse( V_fb2[t] > 9*10^14,  1.296*10^14,  9.504*10^13)
  V_fb2[t + 1] <- V_fb2[t] + (Qi[t + 1] - Qo_fb2[t + 1]) * dt
}

## ========================================= ##
#      plot volume across time in feedback model #1
## ========================================= ##
# plot volume
plot( time, V_fb2, 
      # plot aesthetics
      ylim = c( 7.5*10^14, 9.5*10^14), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Lake Volume over time',
      ylab = 'Volume, cft',
      xlab = 'time, day')

# # plot the flows
# par( mfrow = c( 1, 2))
# plot( time, Qi_fb2, type = 'l', 
#       main = 'Bathtub, feeback #2', ylab = 'Inflow, gal/min', xlab = 'time, min')

#Outflow figure
plot( time, Qo_fb2, type = 'l', 
      main = 'Bathtub, feeback #2', ylab = 'Outflow, gal/min', xlab = 'time, min')
par( mfrow = c( 1, 1))

y <-  as.data.table(data.frame(time=time, volume=V_fb2))

y%>% filter (volume > 9*10^14)

# •	How many days in the first 100 days does the dam operator need to open the spillway?
#13 days

z<- y%>% filter (volume < 9*10^14)
max(z$volume)

