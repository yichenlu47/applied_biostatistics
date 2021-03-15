library(dplyr)

setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)

#Among subjects who survived at least 5 years
live.over5yr <- data$obstime >= 365*5
table(live.over5yr)

data_live5yr <- data %>% filter(obstime >= 365*5 & !is.na(crt))
summary(data_live5yr$crt)
m = mean(data_live5yr$crt)
m
n = nrow(data_live5yr)
n
se = sd(data_live5yr$crt)/sqrt(n)
se
t = qt(p=1-(1-0.95)/2, df=n-1)
t
m - t*se
m + t*se
#sample variance: Like var this uses denominator n - 1.

#Among subjects who died within 5 years
data_die5yr <- data %>% filter(death == 1 & obstime < 365*5 & !is.na(crt))
summary(data_die5yr$crt)
m = mean(data_die5yr$crt)
n = nrow(data_die5yr)
n
se = sd(data_die5yr$crt)/sqrt(n)
se
t = qt(p=0.975, df=n-1)
t
m-t*se
m+t*se

#1h
#90% CI
t = qt(p=1-(1-0.90)/2, df=n-1)
t
m-t*se
m+t*se

#99% CI
t = qt(p=1-(1-0.99)/2, df=n-1)
t
m-t*se
m+t*se

##2 GEOMETRIC MEAN
#install.packages("EnvStats")
library("EnvStats")
#a
#What are the point estimate, 
#the estimated standard error of that point estimate, 
#and the 95% confidence interval for the 
#geometric mean creatinine level 
#in a population of similar subjects who would survive at least 5 years

geoMean(data_live5yr$crt)
exp(mean(log(data_live5yr$crt))) 

exp(sd(log(data_live5yr$crt))/sqrt(n))
geoSD(data_live5yr$crt, na.rm = FALSE, sqrt.unbiased = TRUE)

n = nrow(data_live5yr)
n
t = qt(p=1-(1-0.95)/2, df=n-1)
t

#SE
exp(sd(log(data_live5yr$crt))/sqrt(n))

#95% CI
exp(mean(log(data_live5yr$crt)) 
    - t*sd(log(data_live5yr$crt))/sqrt(n))
exp(mean(log(data_live5yr$crt)) 
    + t*sd(log(data_live5yr$crt))/sqrt(n))
#b
#in a population of similar subjects who would die within 5 years
geoMean(data_die5yr$crt)
exp(mean(log(data_die5yr$crt)))

n = nrow(data_die5yr)
n

exp(sd(log(data_die5yr$crt))/sqrt(n))
geoSD(data_die5yr$crt, na.rm = FALSE, sqrt.unbiased = TRUE)


#95% CI
exp(mean(log(data_die5yr$crt)) 
    - t*sd(log(data_die5yr$crt))/sqrt(n))
exp(mean(log(data_die5yr$crt)) 
    + t*sd(log(data_die5yr$crt))/sqrt(n))








