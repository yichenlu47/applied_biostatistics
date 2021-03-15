library(dplyr)

setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)
data2<- data %>% filter(!is.na(crt)) %>% mutate(crt = ifelse(crt > 1.2, "high", "low")) %>% mutate(vital = ifelse(death == 1 & obstime < 365*5, "dead","alive"))
table(data2$high_crt, data2$die_5yr)
sum(data2$crt == "high")
sum(data2$crt == "low")
sum(data2$vital == "alive")
sum(data2$vital == "dead")
#1a. What is the estimated probability of dying within 5 years for subjects with high creatinine levels? 
#What is the estimated odds of dying within 5 years for subjects with high creatinine levels.   
sum(data2$vital == "dead" & data2$crt == "high")/sum(data2$crt == "high")
44/163

#odd
sum(data2$vital == "dead" & data2$crt == "high")/sum(data2$vital == "alive" & data2$crt == "high")
44/119

#1b. What is the estimated probability of dying within 5 years for subjects with low creatinine levels from the sample. What is the estimated odds of dying within 5 years for subjects with low creatinine levels from the sample.   
sum(data2$vital == "dead" & data2$crt == "low")/sum(data2$crt == "low")
77/570

sum(data2$vital == "dead" & data2$crt == "low")/sum(data2$vital == "alive" & data2$crt == "low")
77/493

#1c. Give full inference for an association between 5 year all-cause mortality and serum creatinine levels based on the ratio of the odds from questions 1a and 1b, i.e., the odds ratio (OR) of dying within 5 years for subjects with high creatinine levels and subjects low creatinine levels
44/119 / (77/493)
orSE <- sqrt((1/77)+(1/493)+(1/44)+(1/119))
exp(log(44/119 / (77/493)) + c(-1,1)*qnorm(0.975)*orSE)

#p value
library(fmsb)
oddsratio(44,119,77,493)

#Z-statistics
2*(1-pnorm(log(44/119/(77/493))/orSE))

m = matrix(c(44,119,77,493), nrow = 2)
fisher.test(m)


#1e. How does the odds ratio in 1c change when the response variable of interest is changed from dying within five years to surviving at least 5 years?  Does the statistical evidence for an association between 5 year all-cause mortality and serum creatinine level change when using an odds ratio for surviving at least 5 years versus using an odds ratio for dying within 5 years?
119/44/(493/77)
orSE <- sqrt((1/77)+(1/493)+(1/44)+(1/119))
exp(log(119/44/(493/77)) + c(-1,1)*qnorm(0.975)*orSE)
1/1.5539
1/3.6067

#p value
library(fmsb)
oddsratio(119,44,493,77)

#Z-statistics
2*(1-pnorm(log(119/44/(493/77)), log(1), orSE))


m = matrix(c(119,44,493,77), nrow = 2)
fisher.test(m)

#1d. How do the odds in part 1a and 1b change if  the response variable of interest is changed from dying within five years to surviving  at least 5 years? Explain and provide evidence to support your answer. 
#odd
sum(data2$vital == "alive" & data2$crt == "high")/sum(data2$vital == "dead" & data2$crt == "high")
119/44

#odd
sum(data2$vital == "alive" & data2$crt == "low")/sum(data2$vital == "dead" & data2$crt == "low")
493/77



#2a
#the estimated probability of having high serum creatinine for subjects who die within 5 years
sum(data2$vital == "dead" & data2$crt == "high")/sum(data2$vital == "dead")
44/121
#the estimated odds of having high creatinine level for subjects who die within 5 years 
44/77

#2b
#the estimated probability of having high serum creatinine for subjects who survive at least 5 years
sum(data2$vital == "alive" & data2$crt == "high")/sum(data2$vital == "alive")
119/612

#the estimated odds of having high creatinine level for subjects who survive at least 5 years
sum(data2$vital == "alive" & data2$crt == "high")/sum(data2$vital == "alive" & data2$crt == "low")
119/493

#2c
44/77/ (119/493)
orSE <- sqrt((1/77)+(1/493)+(1/44)+(1/119))
exp(log(44/77/ (119/493)) + c(-1,1)*qnorm(0.975)*orSE)

###3a. Give full inference for an association between 5-year all-cause mortality and serum creatinine level using the risk difference (RD) of death within 5 years for subjects with high serum creatinine and subjects with low serum creatinine
phigh = 44/163 #death among high crt subjects
plow = 77/570 #death among low crt subjects
phigh - plow
rdSE <- sqrt(phigh*(1-phigh)/163 + plow*(1-plow)/570)
phigh - plow + c(-1,1)*qnorm(0.975)*rdSE

#Hypothesis Testing with Two Proportions
prop.test(x=c(44, 77), n=c(163, 570))
prop.test(x=c(44, 77), n=c(163, 570), correct = FALSE)

#manual Z-statistics
p_null <- (44+77)/(163+570)
rdSE_null <- sqrt(p_null*(1-p_null)*(1/163 + 1/570))
phigh - plow + c(-1,1)*qnorm(0.975)*rdSE_null

#weird
2*(1-pnorm(log(p_null), log(1), rdSE_null))

###3b. risk ratio (RR) 
phigh/plow
rrSE <- sqrt(1/44+1/77-1/163-1/570)
rrSE
exp(log(phigh/plow) + c(-1,1)*qnorm(0.975)*rrSE)

#Z-statistics
2*(1-pnorm(log(phigh/plow)/rrSE))

