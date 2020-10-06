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


#Problem 1


# sample mean x ̅=50 and standard deviation s_X=10. For the


mu <- 50
std <- 10

#normal distribution




# a)	Pr( X > 100)

z <- (100-mu)/std

(1-pnorm(z))* 100





# b)	Pr( 10 < X < 50)

z1 <- (50-mu)/std


pnorm(z1)

z2 <- (10-mu)/std

pnorm(z2)


(pnorm(z1)-pnorm(z2))*100



# c)	Pr( X < 35)

z <- (35-mu)/std

pnorm(z) *100



#gamma distribution

# a)	Pr( X > 100)


alpha_hat <-  (mu^2)/ (std^2)
beta_hat <-  mu/ (std^2)


(1-pgamma(q= 100, shape= alpha_hat, rate= beta_hat))*100





# b)	Pr( 10 < X < 50)

(pgamma(q= 50, shape= alpha_hat, rate= beta_hat)- pgamma(q= 10, shape= alpha_hat, rate= beta_hat)) *100


# c)	Pr( X < 35)

pgamma(q= 35, shape= alpha_hat, rate= beta_hat) *100





#monte carlo simulation



Vmax <-  1e15
Vo <- 0.8  * Vmax

#time step

dt <-  1
n.steps <-  100000

time <-  seq (from = 0, by = dt, length.out = n.steps+1)


#cfs to cfd

cfs_cfd <-  60 * 60 *24


Q1_mean_log <-  15

Q1_std_log <-  1.7


Q2_mean_log <-  10

Q2_std_log <-  2.2


#sample flow values for R1 & R2

R1_sample <-  rlnorm(n.steps+1, meanlog = Q1_mean_log, sdlog = Q1_std_log) * cfs_cfd

R2_sample <-  rlnorm(n.steps+1, meanlog = Q2_mean_log, sdlog = Q2_std_log) * cfs_cfd


#check histogram of flow


par (mfrow= c (2,1))



hist (log(R1_sample), breaks = 100, xlim = c(0,40), xlab = 'log discharge, ft^3/day',
      main = 'Histogram of Log Discharge River 1')

hist (log(R2_sample), breaks = 100, xlim = c(0,40),
      xlab = 'log discharge, ft^3/day', main = 'Histogram of Log Discharge River 1')

par (mfrow= c (1,1))

#qunatiles

quantile (R1_sample, c (0.025, 0.5, 0.975))
quantile (R2_sample, c (0.025, 0.5, 0.975))



#calculate values on the lake over time


V <-  c()

V[1] <-  Vo

Qo <-  rep (1e8, n.steps+1) * cfs_cfd

#histogram of raw variable

hist (R1_sample)


#calculate volume over time


for (t in 1: n.steps) {
  Qo [t+1] <-  ifelse (V[t] > Vmax * 0.9, 5 * Qo [t+1], Qo [t+1])
  Qo [t+1] <-  ifelse (V[t] < Vmax * 0.75, 0.1 * Qo [t+1], Qo [t+1])

  V[t+1] <-  V[t] + (R1_sample[t+1] + R2_sample [t+1] -Qo [t+1])*dt

}


summary(V)


quantile (V, c (0.025, 0.5, 0.975))

quantile (Qo, c (0.025, 0.5, 0.975))

#plots


par(mfrow = c (1, 3))

plot(x = time, y=R1_sample, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Inflow River 1, ft^3/day',
     xlab = 'time,day')

plot(x = time, y=R2_sample, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Inflow River 2, ft^3/day',
     xlab = 'time,day')

plot(x = time, y=Qo, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Outflow River, ft^3/day',
     xlab = 'time,day')

par(mfrow= c (1,1))





hist (Qo)



plot( x= time, y =V,
      ylim= c (0, max (V, Vmax)),
      type= 'l',
      main = 'lake base case',
      ylab = 'volume, cubic ft',
      xlab= 'time, day'
      )

abline(h=Vmax, col= 'red')



#calculate probabilities



#prob of outflowing the lake

length (V[V>Vmax])/n.steps * 100



#prob of opening spillway

length (Qo [Qo> 1e8 * cfs_cfd])/n.steps * 100







#sensitivity analysis



#calculate volume over time


for (t in 1: n.steps) {
  Qo [t+1] <-  ifelse (V[t] > Vmax * 0.9, 7 * Qo [t+1], Qo [t+1])
  Qo [t+1] <-  ifelse (V[t] < Vmax * 0.75, 0.1 * Qo [t+1], Qo [t+1])

  V[t+1] <-  V[t] + (R1_sample[t+1] + R2_sample [t+1] -Qo [t+1])*dt

}


summary(V)


quantile (V, c (0.025, 0.5, 0.975))

quantile (Qo, c (0.025, 0.5, 0.975))

#plots


par(mfrow = c (1, 3))

plot(x = time, y=R1_sample, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Inflow River 1, ft^3/day',
     xlab = 'time,day')

plot(x = time, y=R2_sample, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Inflow River 2, ft^3/day',
     xlab = 'time,day')

plot(x = time, y=Qo, type = 'l', log= 'y', main = 'Lake base model', ylab = 'Outflow River, ft^3/day',
     xlab = 'time,day')

par(mfrow= c (1,1))





hist (Qo)



plot( x= time, y =V,
      ylim= c (0, max (V, Vmax)),
      type= 'l',
      main = 'lake base case',
      ylab = 'volume, cubic ft',
      xlab= 'time, day'
)

abline(h=Vmax, col= 'red')



#calculate probabilities



#prob of outflowing the lake

length (V[V>Vmax])/n.steps * 100



#prob of opening spillway

length (Qo [Qo> 1e8 * cfs_cfd])/n.steps * 100




















