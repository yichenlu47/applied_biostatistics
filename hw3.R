library(dplyr)

setwd("~/Google Drive/UW Seattle/BIOST 514 Biostatistics I/HW1")
read.table("mri.txt",header=TRUE) 
data <- read.table("mri.txt",header=TRUE)

#1a. Provide a histogram displaying the distribution of pack years of smoking
summary(data$packyrs)
hist(data$packyrs, xlab="Pack years of smoking", main="Histogram of smoking history in pack years")

#1b. Among subjects who died within 5 years, for pack years, provide the number of valid observations, the number of missing observations, the mean, the standard deviation, the minimum, 25th percentile, median, 50th percentile, 75th percentile, and the maximum
data_die5yr <- data %>% filter(death == 1 & obstime < 365*5)
summary(data_die5yr$packyrs) #1 missing value

data_die5yr <- data %>% filter(death == 1 & obstime < 365*5 & !is.na(packyrs))
nrow(data_die5yr)
summary(data_die5yr$packyrs, na.rm=TRUE)
sd(data_die5yr$packyrs)




#1c. Among subjects who survived at least 5 years, for pack years, provide the number of valid observations, the number of missing observations, the mean, the standard deviation, the minimum, 25th percentile, median, 50th percentile, 75th percentile, and the maximum
data_live5yr <- data %>% filter(obstime >= 365*5)
summary(data_live5yr$packyrs) #no missing value
nrow(data_live5yr)
sd(data_live5yr$packyrs)


#1d. Provide boxplots for pack years of smoking for groups defined by vital status at 5 years. The two boxplots should appear in the same figure. Make sure that the graph has a title and there are appropriable labels on the axes for each of the boxplots.
new_data_live5yr <- data_live5yr %>% mutate(vitalstatus = "alive")
new_data_die5yr <- data_die5yr %>% mutate(vitalstatus = "dead")
new_data <- rbind(new_data_live5yr, new_data_die5yr)
table(new_data$vitalstatus)
boxplot(packyrs ~ vitalstatus, new_data, ylab = "Pack years of smoking", xlab = "Vital status at 5 years", main = "Boxplots of smoking history in pack years stratified by vital status at 5 years")

#1e. What are the point estimate and the 95% confidence interval for mean pack years of smoking for a population of similar subjects that survives at least 5 years and a population of similar subjects that dies within 5 years? 
n_live5yr = nrow(data_live5yr)
t = qt(p=1-(1-0.95)/2, df=n-1)
sd_live5yr = sd(data_live5yr$packyrs)
mean(data_live5yr$packyrs) - t*sd_live5yr/sqrt(n_live5yr)
mean(data_live5yr$packyrs) + t*sd_live5yr/sqrt(n_live5yr)

n_die5yr = nrow(data_die5yr)
t = qt(p=1-(1-0.95)/2, df=n-1)
sd_die5yr = sd(data_die5yr$packyrs)
mean(data_die5yr$packyrs) - t*sd_die5yr/sqrt(n_die5yr)
mean(data_die5yr$packyrs) + t*sd_die5yr/sqrt(n_die5yr)

#1g. What is the point estimate for difference in mean pack years of smoking between a population of similar subjects that survives at least 5 years and a population of similar subjects that dies within 5 years?  Construct 95% confidence interval the difference in mean pack years for the two populations defined by vital status at 5 years, allowing for unequal variances between the two populations. What conclusions can you reach, if any, about differences in mean pack years of smoking for the two populations defined by vital status a 5 years. 
m = mean(data_die5yr$packyrs) - mean(data_live5yr$packyrs)
m

n = min(n_die5yr, n_live5yr)
n
t = qt(p=1-(1-0.95)/2, df=n-1)
m + t * sqrt(sd_die5yr^2/n_die5yr + sd_live5yr^2/n_live5yr)
m - t * sqrt(sd_die5yr^2/n_die5yr + sd_live5yr^2/n_live5yr)

t.test(packyrs~vitalstatus,data=new_data)
t.test(packyrs~vitalstatus,var.equal = TRUE, data=new_data)

#2
data2 = data%>% filter(!is.na(packyrs) & !is.na(male))
summary(data2$packyrs)
table(data2$male)
m = mean(data2$packyrs[data2$male==1]) - mean(data2$packyrs[data2$male==0])
m
n_male = sum(data2$male==1)
n_female = sum(data2$male==0)
n = min(n_male, n_female)
n
t = qt(p=1-(1-0.95)/2, df=n-1)
t
sd_male = sd(data2$packyrs[data2$male==1])
sd_female = sd(data2$packyrs[data2$male==0])
m + t * sqrt(sd_male^2/n_male + sd_female^2/n_female)
m - t * sqrt(sd_male^2/n_male + sd_female^2/n_female)
t.test(packyrs~male,data=data2)

#3
data3_die5yr <- data %>% filter(death == 1 & obstime < 365*5) %>% 
  mutate(alive = 0)
summary(data3_die5yr$crt) #no missing value


data3_live5yr <- data %>% filter(obstime >= 365*5)
summary(data3_live5yr$crt) #2 missing value
data3_live5yr <- data %>% filter(obstime >= 365*5 & !is.na(crt)) %>% 
  mutate(alive = 1)
summary(data3_live5yr$crt)

data3 <- rbind(data3_die5yr, data3_live5yr)

m = mean(data3$crt[data3$alive==1]) - mean(data3$crt[data3$alive==0])
m
n1 = sum(data3$alive == 1)
n0 = sum(data3$alive == 0)
n = min(n1, n0)
n
t = qt(p=1-(1-0.95)/2, df=n-1)
t
sd1 = sd(data3$crt[data3$alive ==1])
sd0 = sd(data3$crt[data3$alive ==0])
m + t * sqrt(sd1^2/n1 + sd0^2/n0)
m - t * sqrt(sd1^2/n1 + sd0^2/n0)
t.test(crt~alive,data=data3)
