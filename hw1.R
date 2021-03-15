#install.packages('dplyr')
library(dplyr)

setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)

str(data)

data2 <- data %>% select(c(age, male, weight, height, atrophy, obstime, death))
summary(data2)

a <- t(do.call(cbind, lapply(data2, summary)))
sd(data2$age)
sd(data2$male)
sd(data2$weight)
sd(data2$height)
sd(data2$atrophy)

data_female <- data2 %>% filter(male == 0) %>% select (-c(male))
summary(data_female)
b <- t(do.call(cbind, lapply(data_female, summary)))
sd(data_female$age)
sd(data_female$weight)
sd(data_female$height)
sd(data_female$atrophy)

data_male <- data2 %>% filter(male == 1) %>% select (-c(male))
summary(data_male)
c <- t(do.call(cbind, lapply(data_male, summary)))
sd(data_male$age)
sd(data_male$weight)
sd(data_male$height)
sd(data_male$atrophy)
