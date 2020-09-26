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


Mo <-  800 #Gtc (1.7215 is converting c to co2 equivalent in ppm)

# time step, number of steps, and time vector
dt <- 1 #year
n.steps50 <- 50
n.steps100 <-  100
time50 <- seq( from = 0, by = dt, length.out = n.steps50 + 1)
time100 <- seq( from = 0, by = dt, length.out = n.steps100 + 1)


Qi <- c(219, rep (219, 100))
Qo <- c(215, rep (215,100)) 

V <- c()
V[1] <- Mo

# calculate volumes
for( t in 1:n.steps100){
  V[t + 1] <- V[t] + (Qi[t + 1] - Qo[t + 1]) * dt
}



# Plotting figure when there is no feedback loop
par( mfrow = c( 1, 2))

plot( time100, Qi,
      # plot aesthetics
      ylim = c( 0, 300), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 inflow (no control)',
      ylab = 'CO2 Inflow, Gtc/year',
      xlab = 'time, year')
plot( time100, Qo,
      # plot aesthetics
      ylim = c( 0, 300), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 outflow (no control) ',
      ylab = 'CO2 outflow, Gtc/year',
      xlab = 'time, year')
plot( time100, V,
      # plot aesthetics
      ylim = c( 0, 1200), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 Mass over time (no emission control)',
      ylab = 'CO2 Mass (Gtc)',
      xlab = 'time, year')

par( mfrow = c( 1, 1))




M <- c()
M[1] <- Mo

# 1 Gt carbon = 0.4704 ppm carbon
# concentration of C equivalent = 44/12 * 0.4704 = 1.7215 ppm CO2
# calculate Mass 


for( t in 1:n.steps100){
  M[t + 1] <- M[t] + (Qi[t+1]-Qo[t+1]) * dt
}

plot( time100, M, 
      # plot aesthetics
      ylim = c( 0, 2000), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 over time',
      ylab = 'CO2 concentration, Gtc',
      xlab = 'time, year')



x <-  as.data.table(data.frame(time=time100, Mass=M))

x [, Mass2 := Mass*1.725]

x %>% filter (time==100)

 plot( time100, x$Mass2, 
      # plot aesthetics
      ylim = c( 0, 2100), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 over time',
      ylab = 'CO2 concentration, ppm',
      xlab = 'time, year')

 x %>% filter (time==100)
 x %>% filter (time==50)

 
 #sensitivity analysis
 
 Qin <- c(210, rep(210,100))
 Qho <-  9
 
#5% decrease in co2  emission
E <- c() 
E[1] <- Qho

for (t in 1:100) {
  E[t+1]= E[t]*0.95
}

E

Qin.t <-  Qin + E

plot( time100, Qin.t, 
      # plot aesthetics
      ylim = c( 200, 230), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 inflow over time',
      ylab = 'CO2 concentration, Gtc',
      xlab = 'time, year')

# Qo.nat <- c (210, rep (210,100))
# Qho.hum <- c (5,rep(5,12),rep(0,88))

E
Qout.t <- Qo

# Qout.t <- Qo


M <- c()
M[1] <- Mo

# 1 Gt carbon = 0.4704 ppm carbon
# concentration of C equivalent = 44/12 * 0.4704 = 1.7215 ppm CO2
# calculate Mass 


for( t in 1:n.steps100){
  M[t + 1] <- M[t] + (Qin.t[t+1]-Qout.t[t+1]) * dt
}

plot( time100, M, 
      # plot aesthetics
      ylim = c( 0, 1500), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 concentration in atmosphere over time',
      ylab = 'CO2 Mass concentration, Gtc',
      xlab = 'time, year')



x <-  as.data.table(data.frame(time=time100, Mass=M))

x [, Mass2 := Mass*1.725]

x %>% filter (time==100)

plot( time100, x$Mass2, 
      # plot aesthetics
      ylim = c( 0, 2500), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'CO2 over time',
      ylab = 'CO2 concentration, ppm',
      xlab = 'time, year')

x %>% filter (time==100)
x %>% filter (time==50)

