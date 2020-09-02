############# Packages required ###########

library(e1071)
library(car)


############ Reading and understanding the data ###########

Computer_Data<-read.csv(file.choose())
nrow(Computer_Data)
ncol(Computer_Data)
names(Computer_Data)

#FIrst moment business decision
summary(Computer_Data)
attach(Computer_Data)
View(Computer_Data)

Computer_Data<-Computer_Data[,-1]
orgdata<-Computer_Data
View(orgdata)


#second moment business decision
####standard deviation
sd(price)#[1] 580.804

sd(speed)#[1] 21.15774

sd(hd)#[1] 258.5484

sd(ram)#[1] 5.631099

sd(screen)#[1] 0.9051152

sd(ads)#[1] 74.83528

sd(trend)#[1] 7.873984


####### Varience#####
var(price)
#[1] 337333.2

var(speed)
#[1] 447.6498

var(hd)
#[1] 66847.3

var(ram)
#[1] 31.70928

var(screen)
#[1] 0.8192336

var(ads)
#[1] 5600.32

var(trend)
#[1] 61.99962


#### Third moment busines decision
##skewness####

skewness(price)
#[1] 0.7113836

skewness(speed)
#[1] 0.6566931

skewness(hd)
#[1] 1.377359


skewness(ram)
#[1] 1.385538

skewness(screen)
# [1] 1.633225

skewness(ads)
#[1] -0.5530629

skewness(trend)
#[1] 0.236556


# Fourth Moment Business Decision
###Kurtosis

kurtosis(price)
#[1] 0.7276838

kurtosis(speed)
#[1] -0.2770616

kurtosis(hd)
#[1] 2.447798

kurtosis(ram)
#[1] 1.458699

kurtosis(screen)
# [1] 1.847838

kurtosis(ads)
#[1] -0.5411566

kurtosis(trend)
#[1] -0.6752971

########## Exploratory Data analysis ###########

plot(speed,price)


plot(hd, price)


plot(ram, price)


plot(screen, price)


plot(ads, price)


plot(trend, price)


plot(cd, price)


plot(multi, price)


plot(premium, price)


pairs(Computer_Data)


# Correlation Coefficient matrix - Strength & Direction of Correlation
##cor(Computer_Data)

model <- lm(price ~ speed + hd + ram + screen + ads + trend + cd + multi + premium, data = Computer_Data)
summary(model)#Multiple R-squared:  0.7756

model2 <- lm(price ~ ., data = Computer_Data[-c(1441, 1701),])
summary(model2)#Multiple R-squared:  0.7777


###### Finding out the influencial record  #####
influence.measures(model)

###################### plotting influential measures############

influenceIndexPlot(model)


influencePlot(model)


# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(model2)


avPlots(model2)


model3 <- lm(price ~ speed + hd + ram + screen + ads + trend + premium, data = Computer_Data[-c(1441, 1701),])
summary(model3)#Multiple R-squared:  0.7701


avPlots(model3)

plot(model)

qqPlot(model)
