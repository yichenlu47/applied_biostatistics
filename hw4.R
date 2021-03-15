
#2a
(0.99*0.01)/(0.99*0.01+(1-0.96)*(1-0.01))
#2b
0.96*(1-0.01) /(0.96*(1-0.01)+(1-0.99)*0.01)
#2e
(0.99*0.16)/(0.99*0.16+(1-0.96)*(1-0.16))
0.96*(1-0.16) /(0.96*(1-0.16)+(1-0.99)*0.16) 
#2f
(0.99*0.0001)/(0.99*0.0001+(1-0.96)*(1-0.0001))
0.96*(1-0.0001) /(0.96*(1-0.0001)+(1-0.99)* 0.0001)

#3a
choose(5, 1) * 0.65 * (0.35^4)
#3b P(Y>=3) 
1 - choose(5, 5) * 0.35^5 - choose(5, 4) * 0.65 * (0.35^4) - choose(5, 3) * (0.65^2) * (0.35^3)
#Y = 4 or Y = 5 
choose(5, 5) * 0.65^5 + choose(5, 4) * 0.65^4 * (0.35) + choose(5, 3) * 0.65^3 * (0.35^2)

#As indicated in the documentation for this study, “normal” creatinine levels are approximately 0.5 to 1.2 mg/dl.  As such, for this question we will define “high” creatinine levels to be greater than 1.2 mg/dl
#1a
library(dplyr)
setwd("~/Google Drive/UW Seattle/BIOST 514 Biosta0.65 * (0.354)tistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)
data2<- data %>% filter(!is.na(crt)) %>% mutate(crt= ifelse(crt > 1.2, "high", "low")) %>% mutate(vital = ifelse(death == 1 & obstime < 365*5, "dead","alive"))
table(data2$high_crt, data2$die_5yr)
sum(data2$crt == "high")
sum(data2$crt == "low")
sum(data2$vital == "alive")
sum(data2$vital == "dead")
#1b survive at least 5 years
p = 612/733
p
SE = sqrt(p*(1-p)/nrow(data2))
SE
c(p - qnorm(.975)*SE, p + qnorm(.975)*SE)

#1c low creatinine level
p = 570/733
p
SE = sqrt(p*(1-p)/nrow(data2))
SE
c(p - qnorm(.975)*SE, p + qnorm(.975)*SE)

#1d dies within five years would have low creatinine level
p = 77/121
p
SE = sqrt(p*(1-p)/121)
SE
c(p - qnorm(.975)*SE, p + qnorm(.975)*SE)

#1e with high creatine would survive at least 5 years
p = 119/163
p
SE = sqrt(p*(1-p)/163)
SE
c(p - qnorm(.975)*SE, p + qnorm(.975)*SE)




#example code
dbinom(x=3, size = 10, prob = 0.3)
factorial(4)
choose(5,3)
                        