#Load libraries needed for graphing and statistical tests
library(ggplot2)
library(ggstatsplot)


#Read the csv file with data
data <- read.csv("caffeine.csv")


#Plot boxplots of Volume, Calories, and Caffeine, separated by Type of drink
ggplot(data, aes(x = type, y = Volume..ml.)) +
  geom_boxplot()
ggplot(data, aes(x = type, y = Calories)) +
  geom_boxplot()
ggplot(data, aes(x = type, y = Caffeine..mg.)) +
  geom_boxplot()


#IQR calculations per column per type for outlier elimination
qVolume1 <- quantile(data[1:173,]$Volume..ml., probs = c(.25, .75), na.rm = FALSE)
iqrVolume1 <- IQR(data[1:173,]$Volume..ml.)
upVolume1 <- qVolume1[2] + 1.5*iqrVolume1
lowVolume1 <- qVolume1[1]-1.5*iqrVolume1

qVolume2 <- quantile(data[174:392,]$Volume..ml., probs = c(.25, .75), na.rm = FALSE)
iqrVolume2 <- IQR(data[174:392,]$Volume..ml.)
upVolume2 <- qVolume2[2] + 1.5*iqrVolume2
lowVolume2 <- qVolume2[1]-1.5*iqrVolume2

qVolume3 <- quantile(data[393:428,]$Volume..ml., probs = c(.25, .75), na.rm = FALSE)
iqrVolume3 <- IQR(data[393:428,]$Volume..ml.)
upVolume3 <- qVolume3[2] + 1.5*iqrVolume3
lowVolume3 <- qVolume3[1]-1.5*iqrVolume3

qVolume5 <- quantile(data[519:584,]$Volume..ml., probs = c(.25, .75), na.rm = FALSE)
iqrVolume5 <- IQR(data[519:584,]$Volume..ml.)
upVolume5 <- qVolume5[2] + 1.5*iqrVolume5
lowVolume5 <- qVolume5[1]-1.5*iqrVolume5

qVolume6 <- quantile(data[585:610,]$Volume..ml., probs = c(.25, .75), na.rm = FALSE)
iqrVolume6 <- IQR(data[585:610,]$Volume..ml.)
upVolume6 <- qVolume6[2] + 1.5*iqrVolume6
lowVolume6 <- qVolume6[1]-1.5*iqrVolume6

qCalories1 <- quantile(data[1:173,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories1 <- IQR(data[1:173,]$Calories)
upCalories1 <- qCalories1[2] + 1.5*iqrCalories1
lowCalories1 <- qCalories1[1]-1.5*iqrCalories1

qCalories2 <- quantile(data[174:392,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories2 <- IQR(data[174:392,]$Calories)
upCalories2 <- qCalories2[2] + 1.5*iqrCalories2
lowCalories2 <- qCalories2[1]-1.5*iqrCalories2

qCalories3 <- quantile(data[393:428,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories3 <- IQR(data[393:428,]$Calories)
upCalories3 <- qCalories3[2] + 1.5*iqrCalories3
lowCalories3 <- qCalories3[1]-1.5*iqrCalories3

qCalories4 <- quantile(data[429:518,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories4 <- IQR(data[429:518,]$Calories)
upCalories4 <- qCalories4[2] + 1.5*iqrCalories4
lowCalories4 <- qCalories4[1]-1.5*iqrCalories4

qCalories5 <- quantile(data[519:584,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories5 <- IQR(data[519:584,]$Calories)
upCalories5 <- qCalories5[2] + 1.5*iqrCalories5
lowCalories5 <- qCalories5[1]-1.5*iqrCalories5

qCalories6 <- quantile(data[585:610,]$Calories, probs = c(.25, .75), na.rm = FALSE)
iqrCalories6 <- IQR(data[585:610,]$Calories)
upCalories6 <- qCalories6[2] + 1.5*iqrCalories6
lowCalories6 <- qCalories6[1]-1.5*iqrCalories6

qCaffeine1 <- quantile(data[1:173,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine1 <- IQR(data[1:173,]$Caffeine..mg.)
upCaffeine1 <- qCaffeine1[2] + 1.5*iqrCaffeine1
lowCaffeine1 <- qCaffeine1[1]-1.5*iqrCaffeine1

qCaffeine2 <- quantile(data[174:392,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine2 <- IQR(data[174:392,]$Caffeine..mg.)
upCaffeine2 <- qCaffeine2[2] + 1.5*iqrCaffeine2
lowCaffeine2 <- qCaffeine2[1]-1.5*iqrCaffeine2

qCaffeine3 <- quantile(data[393:428,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine3 <- IQR(data[393:428,]$Caffeine..mg.)
upCaffeine3 <- qCaffeine3[2] + 1.5*iqrCaffeine3
lowCaffeine3 <- qCaffeine3[1]-1.5*iqrCaffeine3

qCaffeine4 <- quantile(data[429:518,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine4 <- IQR(data[429:518,]$Caffeine..mg.)
upCaffeine4 <- qCaffeine4[2] + 1.5*iqrCaffeine4
lowCaffeine4 <- qCaffeine4[1]-1.5*iqrCaffeine4

qCaffeine5 <- quantile(data[519:584,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine5 <- IQR(data[519:584,]$Caffeine..mg.)
upCaffeine5 <- qCaffeine5[2] + 1.5*iqrCaffeine5
lowCaffeine5 <- qCaffeine5[1]-1.5*iqrCaffeine5

qCaffeine6 <- quantile(data[585:610,]$Caffeine..mg., probs = c(.25, .75), na.rm = FALSE)
iqrCaffeine6 <- IQR(data[585:610,]$Caffeine..mg.)
upCaffeine6 <- qCaffeine6[2] + 1.5*iqrCaffeine6
lowCaffeine6 <- qCaffeine6[1]-1.5*iqrCaffeine6


#Eliminate outliers
eliminated <- subset(data, data[1:173,]$Volume..ml. > (qVolume1[1] - 1.5*iqrVolume1)
                     & data[1:173,]$Volume..ml. < (qVolume1[2] + 1.5*iqrVolume1)
                     & data[174:392,]$Volume..ml. > (qVolume2[1] - 1.5*iqrVolume2)
                     & data[174:392,]$Volume..ml. < (qVolume2[2] + 1.5*iqrVolume2)
                     & data[519:584,]$Volume..ml. > (qVolume5[1] - 1.5*iqrVolume5)
                     & data[519:584,]$Volume..ml. < (qVolume5[2] + 1.5*iqrVolume5)
                     & data[585:610,]$Volume..ml. > (qVolume6[1] - 1.5*iqrVolume6)
                     & data[585:610,]$Volume..ml. < (qVolume6[2] + 1.5*iqrVolume6)
                     & data[1:173,]$Calories > (qCalories1[1] - 1.5*iqrCalories1)
                     & data[1:173,]$Calories < (qCalories1[2] + 1.5*iqrCalories1)
                     & data[174:392,]$Calories > (qCalories2[1] - 1.5*iqrCalories2)
                     & data[174:392,]$Calories < (qCalories2[2] + 1.5*iqrCalories2)
                     & data[393:428,]$Calories > (qCalories3[1] - 1.5*iqrCalories3)
                     & data[393:428,]$Calories < (qCalories3[2] + 1.5*iqrCalories3)
                     & data[519:584,]$Calories > (qCalories5[1] - 1.5*iqrCalories5)
                     & data[519:584,]$Calories < (qCalories5[2] + 1.5*iqrCalories5)
                     & data[585:610,]$Calories > (qCalories6[1] - 1.5*iqrCalories6)
                     & data[585:610,]$Calories < (qCalories6[2] + 1.5*iqrCalories6)
                     & data[1:173,]$Caffeine..mg. > (qCaffeine1[1] - 1.5*iqrCaffeine1)
                     & data[1:173,]$Caffeine..mg. < (qCaffeine1[2] + 1.5*iqrCaffeine1)
                     & data[174:392,]$Caffeine..mg. > (qCaffeine2[1] - 1.5*iqrCaffeine2)
                     & data[174:392,]$Caffeine..mg. < (qCaffeine2[2] + 1.5*iqrCaffeine2)
                     & data[393:428,]$Caffeine..mg. > (qCaffeine3[1] - 1.5*iqrCaffeine3)
                     & data[393:428,]$Caffeine..mg. < (qCaffeine3[2] + 1.5*iqrCaffeine3)
                     & data[429:518,]$Caffeine..mg. > (qCaffeine4[1] - 1.5*iqrCaffeine4)
                     & data[429:518,]$Caffeine..mg. < (qCaffeine4[2] + 1.5*iqrCaffeine4)
                     & data[519:584,]$Caffeine..mg. > (qCaffeine5[1] - 1.5*iqrCaffeine5)
                     & data[519:584,]$Caffeine..mg. < (qCaffeine5[2] + 1.5*iqrCaffeine5)
                     & data[585:610,]$Caffeine..mg. > (qCaffeine6[1] - 1.5*iqrCaffeine6)
                     & data[585:610,]$Caffeine..mg. < (qCaffeine6[2] + 1.5*iqrCaffeine6))


#Rename rows to produce consecutive row numbers
rownames(eliminated) <- 1:nrow(eliminated)


#Find caffeine content (mg caffeine/mL), add as column to dataframe, make linear regression model
eliminated$caffeineContent <- (eliminated$Caffeine..mg./eliminated$Volume..ml.)
#B1 p-value < 0.001
summary(lm(eliminated$caffeineContent ~ eliminated$Calories))
ggplot(eliminated, aes(x = Calories, y = caffeineContent)) +
  geom_jitter() +
  geom_smooth(method = 'lm')


#Plot boxplots of Volume, Calories, and Caffeine, separated by Type of drink
ggplot(eliminated, aes(x = type, y = Volume..ml.)) +
  geom_boxplot()
ggplot(eliminated, aes(x = type, y = Calories)) +
  geom_boxplot()
ggplot(eliminated, aes(x = type, y = caffeineContent)) +
  geom_boxplot()


#Linear regression and scatterplots of Calories vs Volume, Caffeine vs Calories, Caffeine vs Volume
#B1 p-value < 0.01
summary(lm(eliminated[1:98,]$Calories ~ eliminated[1:98,]$Volume..ml.))
#B1 p-value < 0.05
summary(lm(eliminated[1:98,]$caffeineContent ~ eliminated[1:98,]$Calories))
#B1 p-value < 0.05
summary(lm(eliminated[1:98,]$caffeineContent ~ eliminated[1:98,]$Volume..ml.))
ggplot(eliminated[1:98,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[1:98,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[1:98,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

#B1 p-value < 0.05
summary(lm(eliminated[99:221,]$Calories ~ eliminated[99:221,]$Volume..ml.))
summary(lm(eliminated[99:221,]$caffeineContent ~ eliminated[99:221,]$Calories))
summary(lm(eliminated[99:221,]$caffeineContent ~ eliminated[99:221,]$Volume..ml.))
ggplot(eliminated[99:221,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[99:221,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[99:221,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

summary(lm(eliminated[222:241,]$Calories ~ eliminated[222:241,]$Volume..ml.))
summary(lm(eliminated[222:241,]$caffeineContent ~ eliminated[222:241,]$Calories))
#B1 p-value < 0.05
summary(lm(eliminated[222:241,]$caffeineContent ~ eliminated[222:241,]$Volume..ml.))
ggplot(eliminated[222:241,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[222:241,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[222:241,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

#B1 p-value < 0.01
summary(lm(eliminated[242:291,]$Calories ~ eliminated[242:291,]$Volume..ml.))
summary(lm(eliminated[242:291,]$caffeineContent ~ eliminated[242:291,]$Calories))
summary(lm(eliminated[242:291,]$caffeineContent ~ eliminated[242:291,]$Volume..ml.))
ggplot(eliminated[242:291,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[242:291,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[242:291,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

#B1 p-value < 0.001
summary(lm(eliminated[292:327,]$Calories ~ eliminated[292:327,]$Volume..ml.))
#B1 p-value < 0.05
summary(lm(eliminated[292:327,]$caffeineContent ~ eliminated[292:327,]$Calories))
#B1 p-value < 0.01
summary(lm(eliminated[292:327,]$caffeineContent ~ eliminated[292:327,]$Volume..ml.))
ggplot(eliminated[292:327,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[292:327,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[292:327,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

summary(lm(eliminated[328:343,]$Calories ~ eliminated[328:343,]$Volume..ml.))
summary(lm(eliminated[328:343,]$caffeineContent ~ eliminated[328:343,]$Calories))
summary(lm(eliminated[328:343,]$caffeineContent ~ eliminated[328:343,]$Volume..ml.))
ggplot(eliminated[328:343,], aes(x = Volume..ml., y = Calories)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[328:343,], aes(x = Calories, y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
ggplot(eliminated[328:343,], aes(x = Volume..ml., y = Caffeine..mg.)) +
  geom_jitter() +
  geom_smooth(method = 'lm')