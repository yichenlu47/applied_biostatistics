library(dplyr)

setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)

data2<- data %>% filter(race != "4") %>% mutate(diabetes = factor(diabetes), race = factor(race)) 
summary(data2$race)
summary(data2$diabetes)


table(data2$race, data2$diabetes)

library(knitr);library(kableExtra);
tabQ2 <- addmargins(table(data2$race, data2$diabetes))
row.names(tabQ2) <- c("Whie","Black","Asian", "Total")
## output a pretty table
column_spec(row_spec(kable(tabQ2,booktabs=TRUE, col.names = c("Non-diabetic","Diabetic", "Total")), 2,hline_after = TRUE), 4,border_right=TRUE)

572*646/723
572*77/723
104*646/723
104*77/723
47*646/723
47*77/723

chisq.test(table(data2$race, data2$diabetes))

#QUESTION 2
data3<- data %>% filter(!is.na(crt)) %>% mutate(vital = ifelse(death == 1 & obstime < 365*5, "dead","alive")) %>% mutate(alive = (vital == "alive"), dead = (vital == "dead")) 
sum(data3$dead)
sum(data3$alive)
table(data3$vital)

summary(lm(crt~alive, data =data3))
summary(lm(crt~dead, data =data3))
confint.default(lm(crt~alive, data =data3))


t.test(crt ~ vital, data3, var.equal=TRUE)

summary(lm(crt~vital, data =data3))
confint.default(lm(crt~vital, data =data3))

#QUESTION 3
summary(data3$age)
summary(lm(crt~age, data =data3))


plot(data3$age,data3$crt,
     xlab="Age (Years)",ylab="CRT (mg/dl)", pch = 20)
abline(lm(crt~age, data =data3),col="blue",lwd=3)

#arthimatic mean
0.6485 + 0.0056 * 72
0.6485 + 0.0056 * 82

confint.default(lm(crt~age, data =data3))

data4<- data3 %>% mutate(age5= age/5)
summary(lm(crt~age5, data =data4))
confint.default(lm(crt~age5, data =data4))
