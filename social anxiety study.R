rm(list=ls())
set.seed(27) #we choose 27 just as our preference because we are Group 27 in this LM course
library(dplyr) 
library(tidyverse)
#install.packages("rstatix")
library(ggpubr)
library(rstatix)
library(car)
library(MASS) 

#########################
### TABLE OF CONTENTS ###
#########################

# Here is the structure of this R script:
# 0.  Preparing Data
# 1.  Explore the data extensively using graphical tools
# 2.  Interaction Plot
# 3.  Mean & Std Deviation of SocialAnxiety for each combination
# 4.  ANOVA TWO WAY WITH INTERACTION  (USING TYPE III SS) including testing all of the assumptions
# 5.  ANOVA TWO WAY WITH INTERACTION  (USING TYPE I SS) including testing all of the assumptions
# 6.  DOING TRANSFORMATION TO REMEDY THE ASSUMPTION VIOLATION including the Box Cox Transformation and WLS 
# 7.A Applying Package "twowaytests" for Two Way Anova unbalanced sample and heteroscedasticity problem Using The Parametric Bootstrap Approach                                                    #
# 7.B.Step by Step Syntax to Perform the Parametric Bootstrap Approach for Two Way Anova Unbalanced Sample and Heteroscedasticity Problem
# 8.  PostHoc Test


#########################
### 0.Preparing Data  ###
#########################

SocialAnxietyData <- read.csv("SocialAnxiety.csv", header = TRUE)

#Removing the observation 58 as it has missing value of gender
social_anxiety_data<-SocialAnxietyData[-58,]
str(social_anxiety_data)

#make variable "group1" (factor) as a conversion of variable "group" (character)
social_anxiety_data$group1<-as.factor(social_anxiety_data$group)
str(social_anxiety_data)
attach(social_anxiety_data)

#################################################################
###   1. Explore the data extensively using graphical tools  ###
#################################################################

dev.off()

ggboxplot(social_anxiety_data, "group", "socialanxiety",
          ggtheme =  theme_pubr(), title = "Boxplot Between Group and Social Anxiety",
          xlab = "Group", ylab = "Social Anxiety",
          fill = "group", palette = c("#fcbcdf", "#e6657f", "#c3242b","#620404"))

ggboxplot(social_anxiety_data, "sex", "socialanxiety",
          ggtheme =  theme_pubr(), title = "Boxplot Among Sex, Group, and Social Anxiety",
          xlab = "Sex", ylab = "Social Anxiety",
          fill = "group", palette = c("#fcbcdf", "#e6657f", "#c3242b","#620404"))


ggboxplot(social_anxiety_data, "sex", "socialanxiety",
          ggtheme =  theme_pubr(), title = "Boxplot Between Sex and Social Anxiety",
          xlab = "Sex", ylab = "Social Anxiety",
          fill = "sex", palette = c("#587ce0","#e013f0"))

ggboxplot(social_anxiety_data, "group", "socialanxiety",
          ggtheme =  theme_pubr(), title = "                                                                                       Boxplot Among Group, Sex, and Social Anxiety",
          xlab = "Group", ylab = "Social Anxiety",
          fill = "sex", palette = c("#e013f0","#587ce0"))

##############################
###   2. Interaction Plot  ###
##############################

interaction.plot(x.factor = group, #x-axis variable
                 trace.factor = sex, #variable for lines
                 response = socialanxiety, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Social Anxiety",
                 xlab = "Group",
                 col = c("#587ce0","#e013f0"),
                 lty = 1, #line type
                 lwd = 5, #line width
                 las = 1,
                 trace.label = "Sex", cex.lab= 0.5, cex.axis = 0.5,
                 xpd = FALSE,
                 main="Interaction Plot (Between Sex and Group on Social Anxiety) Based on Mean")

par(mar=c(5,5,5,5))
interaction.plot(x.factor = group, #x-axis variable
                 trace.factor = sex, #variable for lines
                 response = socialanxiety, #y-axis variable
                 fun = sd, #metric to plot
                 ylab = "Social Anxiety",
                 xlab = "Group",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 las = 1,
                 trace.label = "Sex",
                 xpd = FALSE,main="Interaction Plot (Between Sex and Group on Social Anxiety) Based on Standar Deviation")


########################################################################
###   3. Mean & Std Deviation of SocialAnxiety for each combination  ###
########################################################################

(freq.combination <- table(group1, sex))

(mean.combination <- tapply(socialanxiety, list(group1, sex), mean))

(stdeviation.combination<-tapply(socialanxiety,list(group1, sex), sd))


#############################################
###   4. ANOVA TWO WAY WITH INTERACTION   ###
###           USING TYPE III SS           ###
#############################################

(anova_model_type3SS<-Anova(lm(socialanxiety~group1*sex,
                      contrasts=list(group1='contr.sum', sex='contr.sum')),type='III'))
#can also be written like this:
anova_model_type3SS<- aov(socialanxiety~group1*sex,
                  contrasts = list(group1='contr.sum',
                                   sex='contr.sum'))
drop1(anova_model_type3SS,~.,test="F")

#BIC Value
BIC(anova_model_type3SS)

#Check Assumption : Homoscedasticity
plot(fitted.values(anova_model_type3SS), rstandard(anova_model_type3SS),
     xlab="Fitted values", ylab="Studentized Residuals",
     main="Plot of Residuals vs Fitted Value")
leveneTest(socialanxiety ~ group*sex)
#Reject Ho at 5% level of significance
#Hence, homoscedasticity assumption is NOT fulfilled

#Check Assumption : Normality
qqnorm(residuals(anova_model_type3SS)) 
qqline(residuals(anova_model_type3SS))
shapiro.test(residuals(anova_model_type3SS))
ks.test(unique(residuals(anova_model_type3SS)),"pnorm",alternative="two.sided") # using UNIQUE() in KS Test to get the ties out
#Based on QQ Plot and Shapiro Wilk Test : Normality assumption is fulfilled
#Based on Kolmogoriv Smirnov Test: Normality assumption is NOT fulfilled


#Check Assumption : Independence
plot(rstandard(anova_model_type3SS)[-c(1)], rstandard(anova_model_type3SS)[-c(length(socialanxiety))],
     xlab="Studentized residuals at 1 lag", ylab="Studentized residuals", main="Sequence Plot")
abline(a=0,b=1,lty="dashed")
durbinWatsonTest(anova_model_type3SS, alternative="two.sided", data=social_anxiety_data)
#Based on Sequence Plot and Durbin Watson Test (Failed to reject Ho at 5% level of significance)
#Hence, Independence assumption is fulfilled


#Detecting Outlier(s)

###Detecting Outlier(s) by Graph
dev.off()
studentized_res<-rstandard(anova_model_type3SS)
ggboxplot(social_anxiety_data, "group", "studentized_res",
          ggtheme =  theme_pubr(), title = "Residuals VS Factor Levels Plot",
          xlab = "Social Anxiety (Observed)", ylab = "Studentized Residuals",
          fill = "sex", palette = c("#e013f0","#587ce0"))
#There's indication of 2 outliers


###Detecting Outlier(s) by Test with Bonferoni Correction
pvalue_outliers = NULL
n_T <- length(socialanxiety)
r <- 8
for(i in 1:length(socialanxiety)){
           pvalue_outliers[i]=1-pt(abs(rstudent(anova_model_type3SS)[i]),n_T-r-1)}
pvalue_outliers[pvalue_outliers>(0.05/(n_T))] <- 1
Stud.Deleted.Res <- rstudent(anova_model_type3SS)
Outlier.p.value <- pvalue_outliers
(Out.data <- data.frame(Stud.Deleted.Res, Outlier.p.value))
#This test indicates NO outliers since all the p-values > 5%


###Detecting Outlier(s) by using boxplot methods. There are two categories of outlier: (1) outliers and (2) extreme points.
###Values above Q3 + 1.5*IQR or below Q1 - 1.5*IQR are considered as outliers.
###Values above Q3 + 3*IQR or below Q1-3*IQR are considered as extreme points (or extreme outliers)
social_anxiety_data %>%
  group_by(group, sex) %>%
  identify_outliers(socialanxiety)
#There is 1 outlier found



summary(anova_model_type3SS)

##Summary:
#### Data is normal, independent but NOT homoscedastic.
#### There's a big chance that the data contains outlier(s) but we decide to still not drop the outlier(s)
#### Interaction effect is not significant. Group has a significant main effect on the SocialAnxiety but Gender doesn't have


#############################################
###   5. ANOVA TWO WAY WITH INTERACTION   ###
###           USING TYPE I SS             ###
#############################################

anova_model_type1SS<- aov(socialanxiety~group1*sex)

#BIC Value
BIC(anova_model_type1SS)

#Check Assumption : Homoscedasticity
plot(fitted.values(anova_model_type1SS), rstandard(anova_model_type1SS),
     xlab="Fitted values", ylab="Studentized Residuals",
     main="Plot of Residuals vs Fitted Value")
leveneTest(socialanxiety ~ group*sex)
#Reject Ho at 5% level of significance
#Hence, homoscedasticity assumption is NOT fulfilled

#Check Assumption : Normality
qqnorm(residuals(anova_model_type1SS)) 
qqline(residuals(anova_model_type1SS))
shapiro.test(residuals(anova_model_type1SS))
ks.test(unique(residuals(anova_model_type1SS)),"pnorm",alternative="two.sided")  #using UNIQUE() in KS Test to get the ties out
#Based on QQ Plot and Shapiro Wilk Test : Normality assumption is fulfilled
#Based on Kolmogoriv Smirnov Test: Normality assumption is NOT fulfilled

#Check Assumption : Independence
plot(rstandard(anova_model_type1SS)[-c(1)], rstandard(anova_model_type1SS)[-c(length(socialanxiety))],
     xlab="Studentized residuals at 1 lag", ylab="Studentized residuals", main="Sequence Plot")
abline(a=0,b=1,lty="dashed")
durbinWatsonTest(anova_model_type1SS, alternative="two.sided", data=social_anxiety_data)
#Based on Sequence Plot and Durbin Watson Test (Failed to reject Ho at 5% level of significance)
#Hence, Independence assumption is fulfilled

#Detecting Outlier(s)

###Detecting Outlier(s) by Graph
dev.off()
studentized_res<-rstandard(anova_model_type1SS)
ggboxplot(social_anxiety_data, "group", "studentized_res",
          ggtheme =  theme_pubr(), title = "Residuals VS Factor Levels Plot",
          xlab = "Social Anxiety (Observed)", ylab = "Studentized Residuals",
          fill = "sex", palette = c("#e013f0","#587ce0"))
#There's indication of 2 outliers


###Detecting Outlier(s) by Test with Bonferoni Correction
pvalue_outliers = NULL
n_T <- length(socialanxiety)
r <- 8
for(i in 1:length(socialanxiety)){
  pvalue_outliers[i]=1-pt(abs(rstudent(anova_model_type1SS)[i]),n_T-r-1)}
pvalue_outliers[pvalue_outliers>(0.05/(n_T))] <- 1
Stud.Deleted.Res <- rstudent(anova_model_type1SS)
Outlier.p.value <- pvalue_outliers
(Out.data <- data.frame(Stud.Deleted.Res, Outlier.p.value))
#This test indicates NO outliers since all the p-values > 5%


###Detecting Outlier(s) by using boxplot methods. There are two categories of outlier: (1) outliers and (2) extreme points.
###Values above Q3 + 1.5*IQR or below Q1 - 1.5*IQR are considered as outliers.
###Values above Q3 + 3*IQR or below Q1-3*IQR are considered as extreme points (or extreme outliers)
social_anxiety_data %>%
  group_by(group, sex) %>%
  identify_outliers(socialanxiety)
#There is 1 outlier found


summary(anova_model_type1SS)

##Summary:
#### Data is normal, independent but NOT homoscedastic.
#### There's a big chance that the data contains outlier(s) but we decide to still not drop the outlier(s)
#### Interaction effect is not significant. Group has a significant main effect on the SocialAnxiety but Gender doesn't have


#####################################################################
###   6. DOING TRANSFORMATION TO REMEDY THE ASSUMPTION VIOLATION  ###
#####################################################################

###################  Box-Cox Transformation  ########################

data_boxcox <- boxcox(anova_model_type3SS,lambda=seq(-10,10,by=0.0001))
(lambda<-data_boxcox$x[which.max(data_boxcox$y)])

boxcox_socialanxiety<-(socialanxiety^lambda-1)/lambda

(boxcox_anova_model_type3SS<-Anova(lm(boxcox_socialanxiety~group1*sex,
                               contrasts=list(group1='contr.sum', sex='contr.sum')),type='III'))
#can also be written as folow:
boxcox_anova_model_type3SS<- aov(boxcox_socialanxiety~group1*sex,
                          contrasts = list(group1='contr.sum',
                                           sex='contr.sum'))
drop1(boxcox_anova_model_type3SS,~.,test="F")


#Check Assumption : Homoscedasticity
leveneTest(boxcox_socialanxiety ~ group*sex)
#Reject Ho at 5% level of significance
#Hence, homoscedasticity assumption is STILL NOT fulfilled

#Check Assumption : Normality
shapiro.test(residuals(boxcox_anova_model_type3SS))
ks.test(unique(residuals(anova_model_type3SS)),"pnorm",alternative="two.sided") #using UNIQUE() in KS Test to get the ties out
#Based on QQ Plot and Shapiro Wilk Test : Normality assumption is fulfilled
#Based on Kolmogoriv Smirnov Test: Normality assumption is STILL NOT fulfilled


#Check Assumption : Independence
plot(rstandard(boxcox_anova_model_type3SS)[-c(1)], rstandard(boxcox_anova_model_type3SS)[-c(length(boxcox_socialanxiety))],
     xlab="Studentized residuals at 1 lag", ylab="Studentized residuals", main="Sequence Plot")
abline(a=0,b=1,lty="dashed")
durbinWatsonTest(boxcox_anova_model_type3SS, alternative="two.sided", data=social_anxiety_data)
#Based on Sequence Plot and Durbin Watson Test (Failed to reject Ho at 5% level of significance)
#Hence, Independence assumption is fulfilled


##Summary:
#### Data is normal, independent but STILL NOT homoscedastic.
#### So, the Box Cox Transformation doesn't help with the ANOVA assumption violation


#################  WEIGHTED LEAST SQUARES  ####################
#################
summary(anova_model_type3SS)
weight<- 1/anova_model_type3SS$fitted^2
wls_anova_model_type3SS<- aov(socialanxiety~group1*sex,
                              contrasts = list(group1='contr.sum',
                                               sex='contr.sum'), weights=weight)
drop1(wls_anova_model_type3SS,~.,test="F")
(wls_socialanxiety<- weight*socialanxiety)


# As we see the change in parameters we try to use iterated WLS
weight_it1<- 1/wls_anova_model_type3SS$fitted^2
wls_anova_model_type3SS_it1<- aov(socialanxiety~group1*sex,
                                  contrasts = list(group1='contr.sum',
                                                   sex='contr.sum'), weights=weight_it1)
drop1(wls_anova_model_type3SS_it1,~.,test="F")

#Check Assumption : Homoscedasticity
plot(fitted.values(wls_anova_model_type3SS_it1), rstandard(wls_anova_model_type3SS_it1)*sqrt(weight_it1),
     xlab="Fitted values", ylab="Studentized Residuals",
     main="Plot of Residuals vs Fitted Value")
# We get the same result as the previous one, heteroscedascticity still exists

#Check Assumption : Homoscedasticity
leveneTest(wls_socialanxiety ~ group1*sex)
#Reject Ho at 5% level of significance
#Hence, homoscedasticity assumption is STILL NOT fulfilled

#Check Assumption : Normality
shapiro.test(residuals(wls_anova_model_type3SS))
ks.test(unique(residuals(wls_anova_model_type3SS)),"pnorm",alternative="two.sided") #using UNIQUE() in KS Test to get the ties out
#Based on QQ Plot and Shapiro Wilk Test : Normality assumption is fulfilled
#Based on Kolmogoriv Smirnov Test: Normality assumption is STILL NOT fulfilled


#Check Assumption : Independence
plot(rstandard(wls_anova_model_type3SS)[-c(1)], rstandard(wls_anova_model_type3SS)[-c(length(wls_anova_model_type3SS))],
     xlab="Studentized residuals at 1 lag", ylab="Studentized residuals", main="Sequence Plot")
abline(a=0,b=1,lty="dashed")
durbinWatsonTest(wls_anova_model_type3SS, alternative="two.sided", data=social_anxiety_data)
#Based on Sequence Plot and Durbin Watson Test (Failed to reject Ho at 5% level of significance)
#Hence, Independence assumption is fulfilled

##Summary:
#### Data is normal, independent but STILL NOT homoscedastic.
#### So, the WLS doesn't help with the ANOVA assumption violation


#########################################################################################################
###   7.A. Package "twowaytests" for Two Way Anova unbalanced sample and heteroscedasticity problem   ###
###        Using The Parametric Bootstrap Approach                                                    ###
#########################################################################################################

library(twowaytests)
# This packages has License GPL (>= 2) and the description of this package is attached
## The FUNCTION that will be used is "gpTwoWay" function which computes a two-way ANOVA for main effects and interaction effect under heteroscedasticity
## Reference : Xu L., Yang F., Abula A., Qin, S. (2013). A parametric bootstrap approach for two-way ANOVA in presence of possible interactions with unequal variances.
## in the Journal of Multivariate Analysis, 115,172-180.

# Test the homoscedasticity assumption
homogtestTwoWay(socialanxiety~group1*as.factor(sex), data = social_anxiety_data, method ="Levene", alpha = 0.05, na.rm = TRUE, verbose = TRUE)
#Result shows not homoscedasticity OR in other word: heteroscedasticity


#### Using the "gpTwoWay" function to assess Two-Way ANOVA under Heteroscedasticity
# The tests available from this R library are based on two Generalized P-value approach, for Two-Way ANOVA under UNEQUAL Variances and Cell Frequencies
# Parametric Bootstrap based Generalized Test from the Reference
set.seed(27)
result <- gpTwoWay(socialanxiety ~ group1*as.factor(sex), data = social_anxiety_data, method = "gPB")
result$output
result$method
result$formula
result$variance
#Result under 5% significance level:
############# The interaction effect is NOT significant
############# For the main effect, significance is only found in "Group" while "Sex" (as main effect) is not significant.


###################################################################################
###   7.B. Step by Step Syntax to Perform the Parametric Bootstrap Approach     ###
###        for Two Way Anova Unbalanced Sample and Heteroscedasticity Problem   ###
###################################################################################

#data is your data frame, input here:
data <- social_anxiety_data
#formula is the anova formula (two way with interaction)
formula <- socialanxiety~group1*sex
#input the alpha:
alpha <- 0.05

data <- model.frame(formula, data)
fml <-as.character(formula)
ftmp <- strsplit(fml,"~")
y <- as.vector(ftmp[[2]])
Factors <- strsplit(ftmp[[3]][1],"[*]")[[1]]
FacA <- strsplit(Factors[1], " ")[[1]][1] #Drop spaces
FacB <- strsplit(Factors[2], " ")[[1]][2]
InterFacAFacB <- paste(FacA,":",FacB, sep = "")
nM <-10000 # MonteCarlo samples

if (!is.data.frame(data)) stop("Data must be in data.frame class.")
if(length(Factors)!=2) stop("Please correct the formula. Formula must include two factors.")

completeObs <- complete.cases(data)
data <- data[completeObs,]
cnames <- colnames(data)

ADat <- as.vector(data[,cnames==FacA])
BDat <- as.vector(data[,cnames==FacB])
yDat <- as.vector(data[,cnames==y])

As <- unique(ADat) #table(ADat)
Bs <- unique(BDat) #table(BDat)
I <- length(As)  # number of A Factor levels
J <- length(Bs)


# Compute Sample means and variances 
XBij <- matrix(NA,I,J) # Sample means
S2ij <- matrix(NA,I,J) # Sample variances
Nij <- matrix(NA,I,J) # Sample sizes
set.seed(27)
for (i in 1:I) {
  if (1==1) { # This method fails when there are cells with no data
    Yi <- yDat[ADat==As[i]]
    Bi <- BDat[ADat==As[i]]
    XBij[i,]<- tapply(Yi, Bi, mean) 
    S2ij[i,]<- tapply(Yi, Bi, var) 
    Nij[i,]<- tapply(Yi, Bi, length) 
  }
  else {
    for (j in 1:J)	{
      Yij <- yDat[ADat==As[i] & BDat==Bs[j]] # data from ijth cell
      XBij[i,j] <- mean(Yij, na.rm=T)
      S2ij[i,j] <- var(Yij, na.rm=T)
      Nij[i,j] <- length(Yij)
    }
  }
}

(colnames(XBij)=colnames(S2ij)=colnames(Nij)<- Bs)
(rownames(XBij)=rownames(S2ij)=rownames(Nij)<- As) 


#################  MAKING ALL THE FORMULA   ##################

# To Compute Observed Interaction Sum of Squares 
InterSumSqMat = function(nij,XX, S2obs) # Need both Xrand and Xobs both when passing Monte Carlo samples
{	
  # Re test mean for testing
  I = nrow(XX)
  J = ncol(XX)
  IJ = I*J
  wt = nij/S2obs
  Ii = diag(1,I)
  Ij = diag(1,J)
  Oneij = rep(1, IJ)
  Onei = rep(1,I)
  Onej = rep(1,J)
  # Design Matrix always observed
  X = as.matrix(cbind(Oneij, kronecker(Ii, Onej), kronecker(Onei, Ij)))
  I1 = I+1
  I2 = I+2
  wtvec = as.vector(t(wt))
  wi = rowSums(wt)  
  wj = colSums(wt)
  Iw = length(wtvec)+1
  IJS = I+J+1
  l1 =l2 = rep(0, IJS)
  L = matrix(0, IJS,2)
  l1[2:I1] = wi # As ib Xu et al we can also set this to 1
  l2[I2:IJS] = wj
  L[,1]=l1
  L[,2]=l2
  LL = t(L%*%t(L))
  SIG = diag(1/wtvec) # In Xu notastion wt is reciprocal
  SigI = diag(wtvec)
  SigIhalf = diag(sqrt(wtvec))
  XbVec = as.vector(t(XX)) 
  
  leftPart <-SigIhalf %*% X  
  midPart <- t(X) %*% SigI %*% X + LL
  midPartI <- solve(midPart) # ginv(midPart) 
  midmidPart  <- SigI %*% X %*% midPartI %*% t(X)%*%SigI
  T = t(XbVec) %*% (SigI - midmidPart) %*% XbVec
  return(T)
}

###############################################################################
# For commputing Tobs = InterSumSqMat(nn,xb,s2) and pvalGPB(nij, Xobs, S2obs, Tobs)
pvalGPB=function(nij, Xobs, S2obs, Tobs, M=1000,method="gPB") 
{
  # All matrices are read columnn by column
  ndat = as.vector(t(nij))
  nroot = sqrt(ndat)
  ndat1 = ndat-1 # ndat is a vector
  I = nrow(Xobs)
  J = ncol(Xobs)
  IJ = I*J
  sqr = sqrt(S2obs)
  sqrvec = as.vector(t(sqr))
  Ones = matrix(1,I, J)
  pvec = ttvec =rep(0, M)
  
  # Use a sample from readily created random numbers in calling program
  
  sdd = sqrvec/nroot
  for (i in 1:M) { 	
    Xmean = mean(Xobs)
    Zvec = sdd*rnorm(IJ, 0, 1)
    Zvec = Zvec -mean(Zvec) # Mean center for better performnec
    XB = t(matrix(Zvec, J, I))
    Uvec = rchisq(IJ, ndat1)
    S2 = S2obs * t(matrix(Uvec, J, I)) / (nij-1)
    
    Tvec = rt(IJ, ndat1)
    Tmat = t(matrix(Tvec, J, I))
    if (method=="gPB") TmGPB = InterSumSqMat(nij,XB,S2)
    else { 
      wt = nij/S2obs
      Tsn = Tmat / sqrt(wt) 
      TmGPB = InterSumSqMat(nij,Tsn,S2obs) 
    }	
    pvec[i] = TmGPB > Tobs
    ttvec[i] = TmGPB
    
  }  
  
  pval=mean(pvec)
  
  # print(pval)
  return(pval)
  
}

###############################################################################
AInterSumSqMat = function(nij,XX, S2obs) 
{	
  # Re test mean for testing
  I = nrow(XX)
  J = ncol(XX)
  IJ = I*J
  wt = nij/S2obs
  Ii = diag(1,I)
  Ij = diag(1,J)
  Oneij = rep(1, IJ)
  Onei = rep(1,I)
  Onej = rep(1,J)
  # Design Matrix always observed
  X = as.matrix(cbind(Oneij, kronecker(Onei, Ij)))
  I1 = I+1
  I2 = I+2
  wtvec = as.vector(t(wt))
  wj = colSums(wt)
  Iw = length(wtvec)+1
  #
  IJS = J+1
  L = rep(0, IJS)
  L[2:IJS]=wj
  LL = t(L%*%t(L))
  SIG = diag(1/wtvec) # In Xu notation wt is reciprocal
  SigI = diag(wtvec)
  SigIhalf = diag(sqrt(wtvec))
  XbVec = as.vector(t(XX)) 
  
  leftPart <-SigIhalf %*% X  #### S^-1/2 X # Correct
  midPart <- t(X) %*% SigI %*% X + LL
  #  midpart <<- midPart
  midPartI <- solve(midPart) # ginv(midPart) # Note ^-1 is typo in Xu et al paper
  midmidPart  <- SigI %*% X %*% midPartI %*% t(X)%*%SigI
  T = t(XbVec) %*% (SigI - midmidPart) %*% XbVec
  return(T)
}

###############################################################################
FacApvalGPB=function(nij, Xobs, S2obs, Tobs, M=1000,method="gPB") 
{
  # All matrices are read columnn by column
  ndat = as.vector(t(nij))
  nroot = sqrt(ndat)
  ndat1 = ndat-1 # ndat is a vector
  I = nrow(Xobs)
  J = ncol(Xobs)
  IJ = I*J
  sqr = sqrt(S2obs)
  sqrvec = as.vector(t(sqr))
  Ones = matrix(1,I, J)
  pvec = ttvec =rep(0, M)
  
  # Use a sample from readily created random numbers in calling program
  
  sdd = sqrvec/nroot
  for (i in 1:M) { 	
    Xmean = mean(Xobs)
    Zvec = sdd*rnorm(IJ, 0, 1)
    Zvec = Zvec -mean(Zvec) # Mean center for better performnec
    XB = t(matrix(Zvec, J, I))
    Uvec = rchisq(IJ, ndat1)
    S2 = S2obs * t(matrix(Uvec, J, I)) / (nij-1)
    
    Tvec = rt(IJ, ndat1)
    Tmat = t(matrix(Tvec, J, I))
    if (method=="gPB") TmGPB = AInterSumSqMat(nij,XB,S2)
    else { 
      wt = nij/S2obs
      Tsn = Tmat / sqrt(wt) 
      TmGPB = AInterSumSqMat(nij,Tsn,S2obs) 
    }	
    pvec[i] = TmGPB > Tobs
    ttvec[i] = TmGPB
    
  }  
  
  pval=mean(pvec)
  
  return(pval)
}

###############################################################################
BInterSumSqMat = function(nij,XX, S2obs) # Need both Xrand and Xobs both when passing Monte Carlo samples
{	
  # Re test mean for testing
  I = nrow(XX)
  J = ncol(XX)
  IJ = I*J
  wt = nij/S2obs
  Ii = diag(1,I)
  Ij = diag(1,J)
  Oneij = rep(1, IJ)
  Onei = rep(1,I)
  Onej = rep(1,J)
  # Design Matrix always observed
  X = as.matrix(cbind(Oneij, kronecker(Ii, Onej)))
  I1 = I+1
  I2 = I+2
  wtvec = as.vector(t(wt))
  wi = rowSums(wt)  
  Iw = length(wtvec)+1
  #
  IJS = I+1
  L =rep(0, IJS)
  L[2:I1] = wi 
  LL = t(L%*%t(L))
  SIG = diag(1/wtvec) # In Xu notastion wt is reciprocal
  SigI = diag(wtvec)
  SigIhalf = diag(sqrt(wtvec))
  XbVec = as.vector(t(XX)) 
  
  leftPart <-SigIhalf %*% X  #### S^-1/2 X # Correct
  midPart <- t(X) %*% SigI %*% X + LL
  #  midpart <<- midPart
  midPartI <- solve(midPart) # ginv(midPart) # Note ^-1 is typo in Xu et al paper
  midmidPart  <- SigI %*% X %*% midPartI %*% t(X)%*%SigI
  T = t(XbVec) %*% (SigI - midmidPart) %*% XbVec
  return(T)
}

###############################################################################
FacBpvalGPB=function(nij, Xobs, S2obs, Tobs, M=1000,method="gPB") 
{
  # All matrices are read columnn by column
  ndat = as.vector(t(nij))
  nroot = sqrt(ndat)
  ndat1 = ndat-1 # ndat is a vector
  I = nrow(Xobs)
  J = ncol(Xobs)
  IJ = I*J
  sqr = sqrt(S2obs)
  sqrvec = as.vector(t(sqr))
  Ones = matrix(1,I, J)
  pvec = ttvec =rep(0, M)
  
  # Use a sample from readily created random numbers in calling program
  
  sdd = sqrvec/nroot
  for (i in 1:M) { 	
    Xmean = mean(Xobs)
    Zvec = sdd*rnorm(IJ, 0, 1)
    Zvec = Zvec -mean(Zvec) # Mean center for better performnec
    XB = t(matrix(Zvec, J, I))
    Uvec = rchisq(IJ, ndat1)
    S2 = S2obs * t(matrix(Uvec, J, I)) / (nij-1)
    
    Tvec = rt(IJ, ndat1)
    Tmat = t(matrix(Tvec, J, I))
    if (method=="gPB") TmGPB = BInterSumSqMat(nij,XB,S2)
    else { 
      wt = nij/S2obs
      Tsn = Tmat / sqrt(wt) 
      TmGPB = BInterSumSqMat(nij,Tsn,S2obs)
    }	
    pvec[i] = TmGPB > Tobs
    ttvec[i] = TmGPB
    
  }  
  
  pval=mean(pvec)
  
  #  print(pval)
  return(pval)
  
}

#############  DONE MAKING ALL THE FORMULA   #############

#--------------------------------------------------------#
#----               NOW COMPUTE THE PVALUE           ----#
#----            FOR THE PARAMETRIC BOOTSTRAP        ----#
#--------------------------------------------------------#

#Based on the Algorithm in the Reference Journal (page 177)

### 1. For the given sample size, compute "y(lowercase) bar" and "s square (lowercase) tilde"
Nij #n OR sample size
XBij #y (lowercase) bar OR the sample means
S2ij #s square (lowercase) tilde OR the sample variances

### 2. For k=1,..,m DO THE BOOTSTRAP so we can generate "Y(uppercase) bar" that follows some kind of Normal Distribution
#      and also generate "S(uppercase) tilde" that follows some kind of Chi Square Distribution 

#for interaction effect
Tobs = InterSumSqMat(Nij,XBij,S2ij)
#for main effect A
TobsA = AInterSumSqMat(Nij,XBij,S2ij)
#for main effect B
TobsB = BInterSumSqMat(Nij,XBij,S2ij)

### 3. Only if result from step 2 > result from step 1 then set Qk=1
#      then estimate the Monte Carlo p-value as the average of Qk 

#Monte Carlo p-value for interaction effect
pval.Int=pvalGPB(Nij,XBij,S2ij,Tobs,nM,method="gPB")

#Monte Carlo p-value for main effect A
pval.FacA=FacApvalGPB(Nij, XBij,S2ij,TobsA, nM, method="gPB")

#Monte Carlo p-value for main effect B
pval.FacB=FacBpvalGPB(Nij,XBij,S2ij,TobsB, nM, method="gPB")

### Done!. Let's then print the result/conclusion of this parametric bootstrap
result = data.frame(matrix(NA, nrow = 3, ncol = 3))
result$X1 = c(FacA, FacB, InterFacAFacB)
result$X2 = c(pval.FacA,pval.FacB, pval.Int)
result$X3 = ifelse(result$X2 > alpha, "Not reject", "Reject")
colnames(result) = c("Factor", "   P-value", "  Result")
print(result)


#################################
###    8.   PostHoc Test      ###
#################################

anova_model_type3SS
###Tukey_hsd
dev.off()
par(mar=c(5,25,5,1))
plot(TukeyHSD(anova_model_type3SS, conf.level=.99, which="group1"), cex=0.5, cex.axis=1, las=1, col="red", xpd = NA)

anova_model_type1SS
###Tukey_hsd
dev.off()
par(mar=c(5,25,5,1))
plot(TukeyHSD(anova_model_type3SS, conf.level=.99, which="group1"), cex=0.5, cex.axis=1, las=1, col="red", xpd = NA)
