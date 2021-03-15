library(dplyr)
library(uwIntroStats)
library(EnvStats)
setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)
data1<- data %>% filter(!is.na(crt)) %>% mutate(dead = ifelse(death == 1 & obstime < 365*5, 1, 0)) %>% mutate(highcrt = ifelse(crt > 1.2, 1, 0)) 
table(data1$dead, data1$highcrt)

####QUESTION 1
q1 <- regress("odds", dead ~ highcrt, data1)
q1

#intercept
q1$coefficients[1]
exp(coefficients(q1)[1])/(1+exp(coefficients(q1)[1]))
#slope
q1$coefficients[2]
exp(coefficients(q1)[2])
exp(coefficients(q1)[1]) *exp(coefficients(q1)[2])
#high creatinine level
coefficients(q1)[1] + coefficients(q1)[2]
exp(coefficients(q1)[1] + coefficients(q1)[2])/(exp(coefficients(q1)[1] + coefficients(q1)[2])+1)

#f
data2<- data1 %>% mutate(alive = 1-dead, lowcrt = 1-highcrt)
q1_lowcrt <- regress("odds", dead ~ lowcrt, data2)
q1_lowcrt
#intercept
q1_lowcrt$coefficients[1]
exp(coefficients(q1_lowcrt)[1])/(1+exp(coefficients(q1_lowcrt)[1]))
100-42.24
1-0.2769
1-0.6444
#g
q1_alive <- regress("odds", alive ~ highcrt, data2)
q1_alive
#intercept
q1_alive$coefficients[1]
exp(coefficients(q1_alive)[1])/(1+exp(coefficients(q1_alive)[1]))
1-  0.4224
1-0.2769
1-0.6444
1/0.4224
####QUESTION 2
q2 <- regress("odds", highcrt ~ dead, data1)
q2

#dead group
q2$coefficients[1]
exp(coefficients(q2)[1])/(1+exp(coefficients(q2)[1]))

#high creatinine level for dead
coefficients(q2)[1] + coefficients(q2)[2]
exp(coefficients(q2)[1] + coefficients(q2)[2])
exp(coefficients(q2)[1] + coefficients(q2)[2])/(exp(coefficients(q2)[1] + coefficients(q2)[2])+1)


####QUESTION 3
q3 <- regress("mean", dead ~ highcrt, data1)
q3


####QUESTION 4
#a
q4a <- regress("odds", dead ~ crt, data1)
q4a
b0 = coefficients(q4a)[1]
b1 = coefficients(q4a)[2]
exp(b0)/(1+exp(b0))
exp(b0+b1*0.8)/(1+exp(b0+b1*0.8))
exp(b0+b1*1.8)/(1+exp(b0+b1*1.8))
exp(b0+b1*2.8)/(1+exp(b0+b1*2.8))
exp(b0+b1*3.8)/(1+exp(b0+b1*3.8))

#b
#regression analysis evaluating an association between 5-year all-cause mortality and 
#serum creatinine levels using risk difference as a contrast measure
q4b <- regress("mean", dead ~ crt, data1)
q4b

#intercept
coefficients(q4b)[1]
#slope
coefficients(q4b)[2]
coefficients(q4b)[1] + 0.8*coefficients(q4b)[2]
coefficients(q4b)[1] + 1.8*coefficients(q4b)[2]
coefficients(q4b)[1] + 2.8*coefficients(q4b)[2]
coefficients(q4b)[1] + 3.8*coefficients(q4b)[2]

summary(data1$crt)

#####QUESTION 5
#obstime= The total time (in days) that the participant was observed on study between the date of MRI and death or September 16, 1997, whichever came first.
#death= An indicator that the participant was observed to die while on study. If death=1, the number of days recorded in obstime is the number of days between that participant's MRI and his/her death. If death=0, the number of days recorded in obstime is the number of days between that participant's MRI and September 16, 1997.
library(survival)
survobj <- with(data1, Surv(obstime, death))
table(data1$death)
es <- survfit(survobj ~ highcrt, data1)
plot(es, mark.time = TRUE,  col = c("orange", "pink"), xlab="Time to death (in days)", ylab = "Survival", 
     main = "KM survival estimate for subjects with high vs. low creatinine level", cont.int = TRUE)
legend("bottomright", lty=1, col = c("orange", "pink"), c("low creatinine level", "high creatinine level"))

survdiff(survobj ~ highcrt, data1)
