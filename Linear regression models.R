###########################LINEAR MODELS PROJECT R-Code#########################################
#libraries
library(robustbase)
library(lmtest)
library(MASS)
library(rgl)
library(car)
library(data.table)


#Importing the data, and splitting into training and validation set
data.full = read.table("dataset_g20.txt",header=T)
set.seed(0142520)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]

attach(data.training)

#Scaling data (Only 1,4 and 5 are scaled, since these are the only relevant continuous variables
# & it makes no sense to scale the categorical variables, nor age since we don't use it)
data.training2 <- data.training
data.training2[,c(1,4,5)] <- scale(data.training[,c(1,4,5)])

############EXPLORATORY ANALYSIS####################
#histograms and boxplots for all variables
par(mfrow=c(3,4))
hist(data.full$psa, main = "Histrogram of psa", xlab = "psa")
boxplot(data.full$psa, main = "boxplot of psa")
hist(data.full$age, main = "Histrogram of age", xlab = "age")
boxplot(data.full$age, main = "boxplot of age")
hist(data.full$volume, main = "Histrogram of volume", xlab = "volume")
boxplot(data.full$volume, main = "boxplot of volume")
hist(data.full$caps, main = "Histrogram of caps", xlab = "caps")
boxplot(data.full$caps, main = "boxplot of caps")
hist(data.full$gleason, main = "Histrogram of gleason", xlab = "gleason")
#boxplot for the categorical variable gleason
boxplot(psa ~ gleason, data = data.full, main = "boxplot of gleason")
hist(data.full$invasion, main = "Histrogram of invasion", xlab = "invasion")
#boxplot for the categorical variable invasion
boxplot(psa ~ invasion, data = data.full, main = "boxplot of invasion")

#correlation matrix
correlation_matrix <- cor(data.full)
round(correlation_matrix, 8)

############BASELINE MODEL##########################
lm.OLS1 <- lm(psa~ age+ volume+caps+factor(gleason)+factor(invasion),data=data.training2)
summary(lm.OLS1)

#Checking Model Assumptions
#Wilk-Shapiro test
lmOLS1.res<-residuals(lm.OLS1)
shapiro.test(lmOLS1.res)
par(mfrow=c(2,2))
plot(lm.OLS1)

#normal quantile
library(MASS)
par(mfrow=c(1,2))
lm.OLS1.stdres<-stdres(lm.OLS1)
qqnorm(lm.OLS1.stdres,main="Normal Quantile Plot")
qqline(lm.OLS1.stdres,col="Red")
#Normality with deviations in the right tail

par(pty="s")
#residuals v/s index
plot(lm.OLS1$residuals, main="Residuals vs Index")
lines(lowess(lm.OLS1$residuals),col="red")
abs.lm.OLS1.stdres<-abs(lm.OLS1.stdres)
plot(abs.lm.OLS1.stdres~lm.OLS1$fitted.values, main="absolute standardized residuals vs fitted values")
lines(lowess(abs.lm.OLS1.stdres~lm.OLS1$fitted.values),col="red")
#|standardized residuals| v/s fitted values graph shows heteroscadasticity.

#standardised residuals
plot(lm.OLS1.stdres, xlab = "Index", ylab = "Standardized residual", main="Standardized residuals",ylim = c(-3,3))
abline(h=c(-2.5,2.5),col="red")

#Partial residual plots to detect the need for higher order terms
lm.OLS1.res<-residuals(lm.OLS1)
lm.OLS1.coef <- coefficients(lm.OLS1)
lm.OLS1.pres.age <- lm.OLS1.res + lm.OLS1.coef[2] * age
lm.OLS1.pres.invasion <- lm.OLS1.res + lm.OLS1.coef[3] * invasion
lm.OLS1.pres.caps <- lm.OLS1.res + lm.OLS1.coef[4] * caps
lm.OLS1.pres.volume<-lm.OLS1.res + lm.OLS1.coef[5] * volume

#plots
par(mfrow = c(1,1))
plot(age, lm.OLS1.pres.age, ylab = "Partial residual (age)")
abline(lm(unname(lm.OLS1.pres.age) ~ age))
lines(lowess(age, lm.OLS1.pres.age), col = "red")
#linear relationship
plot(invasion,lm.OLS1.pres.invasion,ylab = "Partial residual (invasion)")
abline(lm(unname(lm.OLS1.pres.invasion) ~ invasion))
lines(lowess(invasion,lm.OLS1.pres.invasion), col= "red")
#linear relationship
plot(caps, lm.OLS1.pres.caps, ylab = "Partial residual (caps)")
abline(lm(unname(lm.OLS1.pres.caps) ~ caps))
lines(lowess(caps, lm.OLS1.pres.caps), col = "red")
#linear relationship
plot(volume, lm.OLS1.pres.volume, ylab = "Partial residual (volume)")
abline(lm(unname(lm.OLS1.pres.volume) ~ volume))
lines(lowess(volume, lm.OLS1.pres.volume), col = "red")
#linear relationship 
#No higher order terms required



#Detecting multicolllinearity
#checking the correlation between the variables
cor(data.training2[,2:6])
#No strong correlations shown
#VIF
install.packages('car')
library(car)
vif<-car::vif(lm.OLS1)
vif
mean(vif)
#Doesn't show the existence of multicollinearity as the individual VIF values are all close to 1 
#And also the mean is not much larger than 1

#Validation of the baseline model
data.test2 <- data.test 
#Since we will compare the predictions using a model with scaled training, we scale the validation too
data.test2[,c(1,4,5)] <- scale(data.test[,c(1,4,5)])
MSEP1 <- mean((predict(lm.OLS1, newdata =data.test2) - data.test2$psa)^2) #MSEP
MSE <- mean(lm.OLS1$residuals^2) #MSE
print(c(MSEP1,MSE,MSEP1-MSE))
(PRESS1 <- sum((residuals(lm.OLS1) / (1 - lm.influence(lm.OLS1)$hat))^2))

#Conducting Step AIC

lm.output8 <- lm(psa ~ volume+caps+factor(gleason)+factor(invasion)+volume:caps+volume:factor(gleason)+volume:factor(invasion)+caps:factor(gleason)+caps:factor(invasion)+I(caps^2)+I(volume^2),data=data.training2)
summary(lm.output8)
surg.stepb <- stepAIC(lm.output8,direction="both")

#Removing the outliers
dataWOO <- data.training2[which(abs.lm.OLS1.stdres<2.5),]

# Forward selection based on F-statistic/t-statistic
fit.null<-lm(psa~1, data=dataWOO)

addterm(fit.null, ~ .  + factor(invasion) + caps + volume + factor(gleason)+ factor(gleason):volume + factor(gleason):caps
        + factor(invasion):caps + factor(invasion):volume + I(volume^2) + I(caps^2), test = "F")
# Add volume
fit.1 <- update(fit.null, ~ . + factor(invasion) + caps + volume)
addterm(fit.1, ~ . + factor(invasion) + caps + volume + factor(gleason)+ factor(gleason):volume + factor(gleason):caps
        + factor(invasion):caps + factor(invasion):volume + I(volume^2) + I(caps^2), test = "F")
# Add caps
fit.2 <- update(fit.1, ~ . + factor(gleason))
addterm(fit.2, ~. + factor(invasion) + caps + volume + factor(gleason)+ factor(gleason):volume + factor(gleason):caps
        + factor(invasion):caps + factor(invasion):volume + I(volume^2) + I(caps^2), test = "F")
# Add invasion
fit.3 <- update(fit.2, ~ . + caps:factor(gleason) )
addterm(fit.3, ~. + factor(invasion) + caps + volume + factor(gleason)+ factor(gleason):volume + factor(gleason):caps
        + factor(invasion):caps + factor(invasion):volume + I(volume^2) + I(caps^2), test = "F")

############MODEL 1:Weighted Least Squares without interaction terms################
#######Data used: Scaled training dataset with outliers removed

lm.OLS2 <- lm(psa~volume+caps+factor(gleason)+factor(invasion),data=dataWOO)
resOLS2 <- resid(lm.OLS2)
fittedOLS2 <- fitted.values(lm.OLS2)

########First iteration
#In the next two lines, we calculate the correct weights
sdv <- lm(abs(resOLS2)~fittedOLS2,data=dataWOO)
w <- 1/sdv$fitted^2

######First Fit
#And here, we conduct a weighted least squares
lm.W1 <- lm(psa~volume+caps+factor(gleason)+factor(invasion),data=dataWOO,weights = w)
summary(lm.W1)

#Then, we define relevant variables
resW <- residuals(lm.W1)
fitW <- fitted(lm.W1)
stdresW1 <- stdres(lm.W1)



#######Second iteration
sdv2 <- lm(abs(resW)~fitW,data=dataWOO)
w2 <- 1/sdv2$fitted.values^2

#######Second fit
model2 <- lm(psa~volume+caps+factor(gleason)+factor(invasion),data=dataWOO,weights=w2)
summary(model2)


#Defining relevant variables
model2.res <- residuals(model2)
model2.stdres <- stdres(model2)
model2.fittedvalues <- fitted.values(model2)


#Checking Model Assumptions
#Wilk-Shapiro test
shapiro.test(model2.res)
par(pty="s")
par(mfrow = c(2,2))
qqnorm(model2.stdres, main="Normal Q-Q")
qqline(model2.stdres, col="red")
#standardized residuals v/s index
plot(model2.stdres, main="standardized residuals v/s index")
lines(lowess(model2.stdres), col="red")
#check heteroscedasticity
plot(model2.fittedvalues,(model2.res)*sqrt(w2),ylab="Weighted Residuals",main= "Weighted residuals v/s fitted values")
lines(lowess((model2.res)*sqrt(w2)~model2.fittedvalues),col="red")
#Standardized residuals
plot(model2.stdres, xlab = "Index", ylab = "Standardized residual", main="Standardized residuals",ylim = c(-3,3))
abline(h=c(-2.5,2.5),col="red")

#Validation
#Since we will compare the predictions using a model with scaled training, we scale the validation too
data.test2 <- data.test #Since we will compare the predictions using a model with scaled training, we scale the validation too
data.test2[,c(1,4,5)] <- scale(data.test[,c(1,4,5)])
MSEP1 <- mean((predict(model2, newdata =data.test2) - data.test2$psa)^2) #MSEP
MSE <- mean(model2$residuals^2) #MSE
print(c(MSEP1,MSE,MSEP1-MSE)) 
#very close to each other


############MODEL 2: Weighted Least Squares with interaction term caps:gleason########
########Data used: Scaled training dataset with outliers removed

#Ordinary Least Squares procedure on dataWOO, for the purposes of weighting (see later)
lm.OLS2 <- lm(psa~volume+caps+factor(gleason)+factor(invasion)+ caps:factor(gleason),data=dataWOO)
resOLS2 <- resid(lm.OLS2)
fittedOLS1 <- fitted.values(lm.OLS2)
#In the next two lines, we calculate the correct weights
sdv <- lm(abs(resOLS2)~fittedOLS1,data=dataWOO)
w <- 1/sdv$fitted^2
#And here, we conduct a weighted least squares
lm.W1 <- lm(psa~volume+caps+factor(gleason)+factor(invasion) + caps:factor(gleason),data=dataWOO,weights = w)
summary(lm.W1)
#Then, we define relevant variables
resW <- residuals(lm.W1)
fitW <- fitted(lm.W1)
stdresW1 <- stdres(lm.W1)
#Second iteration of weights. 
sdv2 <- lm(abs(resW)~fitW,data=dataWOO)
w2 <- 1/sdv2$fitted.values^2
model2 <- lm(psa~volume+caps+factor(gleason)+factor(invasion)+ caps:factor(gleason),data=dataWOO,weights=w2) #model 2 is the weighted model we will consider
summary(model2)

#Defining relevant variables
model2.res <- residuals(model2)
model2.stdres <- stdres(model2)
model2.fittedvalues <- fitted.values(model2)

#Diagnostic plots
#Wilk-Shapito test
shapiro.test(stdres(model2))
par(pty="s")
par(mfrow = c(2,2))
#Q-Q plot
qqnorm(model2.stdres, main="Normal Q-Q", cex.main=0.8)
qqline(model2.stdres)
#residuals v/s index
plot(model2.stdres, xlab = "Index", ylab = "Standardized Residual", main="Standardized Residuals vs Index", cex.main=0.8)
lines(lowess(model2.stdres), col = "red")
#Weighted Residuals vs Fitted Values
plot(model2.fittedvalues, resid(model2)*sqrt(w2), main="Weighted Residuals vs Fitted Values", xlab = "Fitted value", ylab = "residuals*sqrt(weights)", cex.main=0.8)
lines(lowess(resid(model2)*sqrt(w2) ~ model2.fittedvalues), col = "red")
#Standardiezd residuals
plot(model2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-5,5), cex.main=0.8, main="Standardized Residuals vs Index: Outliers")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)


#validation
data.test2 <- data.test
data.test2[,c(1,4,5)] <- scale(data.test[,c(1,4,5)])
MSEP1 <- mean((predict(model2, newdata =data.test2) - data.test2$psa)^2) #MSEP
MSE <- mean(model2$residuals^2) #MSE
print(c(MSEP1,MSE,MSEP1-MSE))

#PRESS
PRESS1 <- sum((residuals(model2) / (1 - lm.influence(model2)$hat))^2)


############Model 3:Robust Regression without any interaction terms##################
#########Data used: Scaled training dataset with no outliers removed


ltsOutput <- lmrob(psa~volume+caps+factor(gleason)+factor(invasion),data=data.training2)
summary(ltsOutput)

par(mfrow=c(1,1))
plot(ltsOutput)

#Diagnostic plots
#Wilk-Shapito test
shapiro.test(residuals(ltsOutput))
par(mfrow=c(2,2))
#Normal Q-Q
ltsOutput.stdres<-stdres(ltsOutput)
qqnorm(ltsOutput.stdres,main="Normal Quantile Plot")
qqline(ltsOutput.stdres,col="Red")
#residuals v/s index
plot(ltsOutput$residuals,main="residuals v/s index")
lines(lowess(ltsOutput$residuals),col="red")
#abs standardized residuals v/s index
abs.ltsoutput.stdres<-abs(ltsOutput.stdres)
plot(abs.ltsoutput.stdres~ltsOutput$fitted.values, main="absolute standardized residuals vs fitted values")
lines(lowess(abs(ltsOutput.stdres)~ltsOutput$fitted.values),col="red")
#standardized residuals
plot(ltsOutput.stdres, xlab = "Index", ylab = "Standardized residual", main="Standardized residuals",ylim = c(-3,3))
abline(h=c(-2.5,2.5),col="red")


residlts <- residuals(ltsOutput)
fittedlts <- fitted.values(ltsOutput)

#Validation
data.test2 <- data.test 
data.test2[,c(1,4,5)] <- scale(data.test[,c(1,4,5)])
MSEP3 <- mean((predict(ltsOutput,newdata=data.test2)-data.test2$psa)^2)
MSE3 <- mean(ltsOutput$residuals^2)
print(c(MSEP3,MSE3,MSEP3-MSE3))
(PRESS1 <- sum((residuals(ltsOutput) / (1 - lm.influence(ltsOutput)$hat))^2))


