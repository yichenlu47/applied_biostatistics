library(dplyr)
library(uwIntroStats)
library(EnvStats)
setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)



#QUESTION 1
data1<- data %>% filter(!is.na(crt)) %>% mutate(vital = ifelse(death == 1 & obstime < 365*5, "dead","alive")) %>% mutate(alive = (vital == "alive"), dead = (vital == "dead")) %>% mutate(logcrt = log(crt)) 
q1 <- regress("mean", logcrt ~ vital, data1)
q1
regress("mean", logcrt ~ dead, data1)

#intercept
q1$coefficients[1]
exp(coefficients(q1)[1])
#slope
q1$coefficients[2]
exp(coefficients(q1)[2])
exp(coefficients(q1)[1]) *exp(coefficients(q1)[2])
#lower ci
exp(q1$coefficients[8])
#upper ci
exp(q1$coefficients[10])
#1d How does the estimate obtained from the regression model compare to the geometric mean creatinine level in the sample for subjects who die within 5 years. 
geoMean(data1$crt[data1$vital == "dead"])
geoMean(data1$crt[data1$vital == "alive"])





#QUESTION 2
q2 <- regress("mean", logcrt ~ age, data1)
q2

q2_geo <- regress("geometric mean", crt ~ age, data1)
q2_geo
#intercept
coefficients(q2)[1]
exp(coefficients(q2)[1])

(1- exp(coefficients(q2)[1]))*100





0.6485 + 0.0056 * 75
0.6485 + 0.0056 * 85
0.6485 + 0.0056 * 95
#slope
q2$coefficients[2]
exp(q2$coefficients[2])
(exp(coefficients(q2)[2])-1)*100


#lower ci
q2$coefficients[8]
exp(q2$coefficients[8])
(1- exp(q2$coefficients[8]))  * 100
#upper ci
q2$coefficients[10]
exp(q2$coefficients[10])
(exp(q2$coefficients[10])-1)  * 100

exp(coefficients(q2)[1] + coefficients(q2)[2] * 75)
exp(coefficients(q2)[1] + coefficients(q2)[2] * 85)
exp(coefficients(q2)[1] + coefficients(q2)[2] * 95)

#differ by 10 years in age
exp(q2$coefficients[2]*10)
(exp(coefficients(q2)[2]*10)-1)*100

q2$coefficients[6] * 10 #robust SE of the slope
exp(q2$coefficients[6]*10)

exp(q2$coefficients[2]*10)
coefficients(q2)[2] + c(-1, 1) * qt(0.975, nrow(data1) - 2) * q2$coefficients[6]
coefficients(q2)[2] * 10 + c(-1, 1) * qt(0.975, nrow(data1) - 2) * q2$coefficients[6] * 10



upper <- exp(coefficients(q2)[2] * 10 + qt(0.975, nrow(data1) - 2) * q2$coefficients[6] * 10)
upper
lower <- exp(coefficients(q2)[2] * 10 - qt(0.975, nrow(data1) - 2) * q2$coefficients[6] * 10)
lower
(upper - 1) * 100
(1 - lower) * 100

q2_2 <- regress("geometric mean", crt ~ age, data1)
q2_2
lincom(q2_2, c(0, 10))





plot(data1$age,log(data1$crt),
     xlab="Age (Years)",ylab="log CRT (mg/dl)", pch = 20)
abline(lm(log(crt)~age, data =data1),col="blue",lwd=3)
abline(regress("geometric mean", crt ~ vital, data1))
