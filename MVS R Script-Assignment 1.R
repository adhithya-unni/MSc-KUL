# MULTIVARIATE STATISTICS - ASSIGNMENT 1
rm(list = ls())

# Load data - provide the path
path <- "/Users/sanyaanand/Downloads/wvs(3).Rdata"
load(path)
View(wvs)

#*******************************************
# QUESTION 1 - PCA + BIPLOT OF VALUE ITEMS
#*******************************************


# Table 1 consisting of first 10 columns of criteria and country column
wvs_table1 <- wvs[,c(1:10,33)]
head(wvs_table1)
summary(wvs_table1)

# standardized data 
z_wvs_table1 <- scale(wvs_table1[,1:10],center = TRUE, scale = TRUE)

# Standardized data frame with country column
wvs_standardized <- data.frame(z_wvs_table1,wvs_table1$country)
names(wvs_standardized)[11] <- "Country"
head(wvs_standardized)

# Make a matrix of 34 countries * 10 items with mean score of respondents 
install.packages("tidyverse")
library(tidyverse)
wvs_matrix1 <-wvs_standardized %>% group_by(Country) %>% summarise_all("mean")
wvs_matrix <- wvs_matrix1 %>% select(-Country) %>% as.matrix
rownames(wvs_matrix) = wvs_matrix1$Country

# correlation 
install.packages("corrplot")
library(corrplot)
corrplot(cor(wvs_matrix),method = "number")

# Conducting PCA on standardized wvs_matrix 
pca_wvs <- prcomp(wvs_matrix[,1:10], center = TRUE, scale = TRUE)
pca_wvs
summary(pca_wvs)

# eigenvalues
round(pca_wvs$sdev^2,2)

# proportion of variance explained by each component
round(pca_wvs$sdev^2/10,2)

# Plot scree plot to choose the number of components
par(pty = "s", cex = 1.3)
screeplot(pca_wvs,type = "lines",main = "Screeplot of PCA")
abline(h=1,col="red",lty = 5)
legend("topright", legend=c("Eigenvalue = 1"),col=c("red"), lty=5, cex=0.6)

# Horn's procedure to choose the number of components
bootstrap <- matrix(rep(0,34*10),ncol = 10)
for (i in 1:10) {
  samp <- sample(seq(1,34),size = 34, replace = TRUE)
  bootstrap[,i] <- wvs_matrix[samp,i]
}
# standardize the data
z_boot_wvs <- scale(bootstrap,center = TRUE,scale = TRUE)

# apply PCA to bootstrapped data
pca_boot_wvs <- prcomp(z_boot_wvs)
pca_boot_wvs
summary(pca_boot_wvs)

# Plot eigenvalues
plot(c(1:10),pca_wvs$sdev[1:10]^2, type = "b", xlab = "Component", ylab = "Eigenvalues", xlim = c(1,10), main = "Component Selection - Horn's Procedure")
lines(c(1:10), pca_boot_wvs$sdev[1:10]^2,type="b", col = "red")
legend(4,5,c("real data","bootstrapped data"),bty="n",lty=c(1,1),col=c("black","red"),cex = 0.8) 

# component loading
A <- pca_wvs$rotation %*% diag(pca_wvs$sdev)
round(A,2)

# plot loadings for first 2 components
install.packages("maptools")
library(maptools)
plot(A[,1:2], xlim = c(0,1),ylim = c(-1,1), xlab = "PC1", ylab = "PC2", cex = 1, lwd = 2)
pointLabel(A[,1],A[,2], colnames(wvs_matrix), cex =0.5, lwd = 2,col = 2)

# Biplot 
biplot(pca_wvs,pc.biplot = TRUE,xlim= c(-3,3), ylim = c(-3,3),cex = 0.5)
round(apply(A[,1:2]^2,1,sum),2)
round(A[,1:2] %*% t(A[,1:2]),2)

## compute standardized component scores
score <- wvs_matrix %*%  pca_wvs$rotation[, 1:10] %*%  diag(1/pca_wvs$sdev[1:10])
round(score[,1],2)
round(cbind(z1 = score[c(1:10,20:25),1], z2 = score[c(1:10,20:25),2], wvs_matrix[c(1:10,20:25),]),2)


#*******************************************
# QUESTION 2 - EXPLORATORY FACTOR ANALYSIS
#*******************************************


#Load data (wvs.Rdata) in R
load("C:/Users/sanne/OneDrive/Documenten/wvs.Rdata")
load(psych)
load(GPArotation) 

#Question 2
#Make a data frame with only variables from Table 3
table3 <- data.frame(wvs$J_claiming_benefits, wvs$J_avoiding_fare, wvs$J_stealing_property, wvs$J_cheating_taxes, wvs$J_accept_bribe, wvs$J_homosexuality, wvs$J_prostitution, wvs$J_abortion, wvs$J_divorce, wvs$J_sex_before_marriage, wvs$J_suicide, wvs$J_beat_wife, wvs$J_parents_beating_children, wvs$J_violence)
colnames(table3) <- c("J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence")

#Standardize all variables from Table 3
ztable3 <- scale(table3, center=TRUE,scale=TRUE)
ztable3 <- data.frame(ztable3)
ztable3_pca <- prcomp(ztable3)

#Determine the number of factors for EFA
#Scree plot
screeplot(ztable3_pca, type="lines")

#Kaiser's rule
round(ztable3_pca$sdev^2,3)

#Chosen factors = 3 factors

#Print matrix of observed correlations
cormat <- cor(ztable3)
#colnames(cormat) <- c("J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence")
colnames(cormat) <- c("","","","","","","","","","","","","","")
rownames(cormat) <- c("J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence")
round(cormat,2)

#EFA with 3 factors
fa_ztable3 <- factanal(~J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe+J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide+J_beat_wife+J_parents_beating_children+J_violence, data=ztable3, factors=3, rotation="none")
print(fa_ztable3, cutoff=0)

#EFA with varimax rotation and 3 factors 
fa_vm_ztable3 <- factanal(~J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe+J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide+J_beat_wife+J_parents_beating_children+J_violence, data=ztable3, factors=3, rotation="varimax")
print(fa_vm_ztable3, cutoff=0)

#EFA with oblique rotation and 3 factors
fa_ob_ztable3 <- fa(ztable3, 3, rotate="oblimin", fm="mle")
print(fa_ob_ztable3, cutoff=0)

#Save factor scores per country
score <-cbind(wvs, fa_ob_ztable3$scores)

#Distribution Factor 1 per country
par(pty="s")
par(las=1, cex=1)
par(oma=c(2,6,2,2))
boxplot(score$ML1~score$country, horizontal=TRUE, xlab="factor score", ylab="", main="Factor 1")

#Distribution Factor 2 per country
par(pty="s")
par(las=1, cex=1)
par(oma=c(2,6,2,2))
boxplot(score$ML2~score$country, horizontal=TRUE, xlab="factor score", ylab="", main="Factor 2")

#Distribution Factor 3 per country
par(pty="s")
par(las=1, cex=1)
par(oma=c(2,6,2,2))
boxplot(score$ML3~score$country, horizontal=TRUE, xlab="factor score", ylab="", main="Factor 3")

#*******************************************
# QUESTION 3 - CONFIRMATORY FACTOR ANALYSIS
#*******************************************



install.packages("lavaan")
library(lavaan)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
library(datasets)

attach(wvs)
names(wvs)

#creating the dataframe 
table3 <- data.frame(wvs$J_claiming_benefits, wvs$J_avoiding_fare, wvs$J_stealing_property, 
                     wvs$J_cheating_taxes, wvs$J_accept_bribe, wvs$J_homosexuality, wvs$J_prostitution,
                     wvs$J_abortion, wvs$J_divorce,wvs$J_sex_before_marriage, wvs$J_suicide, wvs$J_beat_wife,
                     wvs$J_parents_beating_children, wvs$J_violence)
colnames(table3) <- c("J_claiming_benefits", "J_avoiding_fare",
                      "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality","J_prostitution",
                      "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife",
                      "J_parents_beating_children", "J_violence")
table1<-subset(table3,country=="Netherlands")
table2<-subset(table3,country=="Malaysia")
que3.df<-rbind(table1,table2)

#exploratory analysis
nrow(que3.df)
par(mfrow=c(1,1))
summary(que3.df)
boxplot( J_claiming_benefits, main="J_claiming_benefits")
boxplot( J_avoiding_fare,main="J_avoiding_fare")
boxplot(J_stealing_property,main="J_stealing_property")
boxplot(J_cheating_taxes,main="J_cheating_taxes")
boxplot(J_accept_bribe,main="J_accept_bribe")
boxplot(J_homosexuality,main="J_homosexuality")
boxplot( J_prostitution ,main=" J_prostitution ")
boxplot(J_abortion,main="J_abortion")
boxplot(J_divorce,main="J_divorce")
boxplot(J_sex_before_marriage,main="J_sex_before_marriage")
boxplot( J_suicide,main=" J_suicide")
boxplot(J_beat_wife,main="J_beat_wife")
boxplot(J_parents_beating_children,main="J_parents_beating_children")
boxplot( J_violence,main=" J_violence")
#boxplots shows outliers that might indicate the differences in the same group

q<-is.na(que3.df)
sum(q)
#no missing values

#Exploratory factor analysis
#compute cov and corr matrix
covmat<-cov(que3.df)
covmat
cormat<-cor(que3.df)
cormat


#exploratory factor analysis on corr matrix with 3 factors and oblique rotation
efa_obl3<-fa(cormat,covar = FALSE, 3, rotate="oblimin",fm="ml")
efa_obl3


#list matrix of structure loadings
print(efa_obl3$Structure,cutoff = 0,digits=3)
#Matrix shows that variables have high loadings on ML1 has smaller or closer to 0 loadings on others
#Cases for variable loadings on ML2 and ML3 differ as they have high loadings on both which 
#may be contributed by a general quality between those factors


#list matrix of residual correlations
round(efa_obl3$residual,3)
#model fits correlation matrix well as the correlations are very small 


###Confirmatory Factor Analysis###
#specify model with 3 specific correlated factors for F1, F2 and F3
cfa3<-'F1=~NA*J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
       F2=~NA*J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe
       F3=~NA*J_beat_wife+J_parents_beating_children+J_violence
       F1~~1*F1
       F2~~1*F2
       F3~~1*F3
       F1~~F2
       F1~~F3
       F2~~F3'


#fit model on covariance matrix
fitcfa3<-cfa(cfa3,sample.cov = covmat,sample.nobs = 2473)

#summary of results
summary(fitcfa3,fit.measures= TRUE)
#model has 31 free parameters and 74 df
#loadings are all positive and significant
#corr between F1, F3 and F2, F3 are significant

#standardized solutions
standardizedSolution(fitcfa3)
fitmeasures(fitcfa3,c("chisq","df","cfi","tli","rmsea","srmr"))
#From the error variance of standardized solutions which contains the estimates for satndardized variables and factors,
#the proportion of variance that is cannot be explained by the model is very small

#chi square goodness of fit as p value is <0.001 indicates that the model doesnt support the data well
#TLI=0.939<0.95 not good fit
#CFI=0.95 satisfied
#RMSEA=0.87 and SRMR=0.070 - not satisfied for RMSEA
#Fit measures indicates that there is room for improvement for the model

#modification indices
modificationindices(fitcfa3)

#######Improved model############
cfa3b<-'F1=~NA*J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
       F2=~NA*J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe
       F3=~NA*J_beat_wife+J_parents_beating_children+J_violence
       F1~~1*F1
       F2~~1*F2
       F3~~1*F3
       F1~~F2
       F2~~F3
       F1~~F3
       J_homosexuality ~~J_sex_before_marriage'
fitcfa3b<-cfa(cfa3b,sample.cov=covmat,sample.nobs=2473)
summary(fitcfa3b)
fitmeasures(fitcfa3b,c("chisq","df","cfi","tli","rmsea","srmr"))

modificationindices(fitcfa3b)

cfa3b1<-'F1=~NA*J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
       F2=~NA*J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe+J_prostitution+J_suicide
       F3=~NA*J_beat_wife+J_parents_beating_children+J_violence+J_prostitution+J_suicide
       F1~~1*F1
       F2~~1*F2
       F3~~1*F3
       F1~~F2
       F1~~F3
       F2~~F3
       J_divorce ~~J_sex_before_marriage
       J_homosexuality ~~J_sex_before_marriage'
fitcfa3b1<-cfa(cfa3b1,sample.cov=covmat,sample.nobs=2473)
summary(fitcfa3b1)
fitmeasures(fitcfa3b1,c("chisq","df","cfi","tli","rmsea","srmr"))
standardizedSolution(fitcfa3b1)

modificationindices(fitcfa3b1)



cfa3b2<-'F1=~NA*J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
       F2=~NA*J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe
       F3=~NA*J_beat_wife+J_parents_beating_children+J_violence+J_prostitution+J_suicide
       F1~~1*F1
       F2~~1*F2
       F3~~1*F3
       F1~~F2
       F1~~F3
       F2~~F3
       J_divorce ~~J_sex_before_marriage
       J_homosexuality ~~J_sex_before_marriage'
fitcfa3b2<-cfa(cfa3b2,sample.cov=covmat,sample.nobs=2473)
summary(fitcfa3b2)
fitmeasures(fitcfa3b2,c("chisq","df","cfi","tli","rmsea","srmr"))
standardizedSolution(fitcfa3b2)

modificationindices(fitcfa3b2)
residuals(fitcfa3b2,type="standardized")

# multi-group analysis

table31 <- data.frame(wvs$J_claiming_benefits, wvs$J_avoiding_fare, wvs$J_stealing_property, 
                      wvs$J_cheating_taxes, wvs$J_accept_bribe, wvs$J_homosexuality, wvs$J_prostitution,
                      wvs$J_abortion, wvs$J_divorce,wvs$J_sex_before_marriage, wvs$J_suicide, wvs$J_beat_wife,
                      wvs$J_parents_beating_children, wvs$J_violence, wvs$country)
colnames(table31) <- c("J_claiming_benefits", "J_avoiding_fare",
                       "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality","J_prostitution",
                       "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife",
                       "J_parents_beating_children", "J_violence","country")
table11<-subset(table31,country=="Netherlands")
table21<-subset(table31,country=="Malaysia")
que31.df<-rbind(table11,table21)


CFAb2<-'F1=~1*J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
       F2=~1*claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe
       F3=~1*J_beat_wife+J_parents_beating_children+J_violence+J_prostitution+J_suicide
       F1~~1*F1
       F2~~1*F2
       F3~~1*F3
       F1~~F2
       F1~~F3
       F2~~F3
       J_divorce ~~J_sex_before_marriage
       J_homosexuality ~~J_sex_before_marriage'


#configural measurement invariance model
config<-cfa(CFAb2,que31.df,group="country")

summary(config,fit.measures=TRUE)
standardizedSolution(config)


#metric measurement invariance model
metric<-cfa(CFAb2,data=que31.df,group="country",group.equal="loadings")
summary(metric,fit.measures=TRUE)
standardizedSolution(metric)

#strong measurement invariance model
strong<-cfa(CFAb2,que31.df,group="country",group.equal=c("loadings","intercepts"))
summary(strong,fit.measures=TRUE)

#likelihood ratio test
anova(config,metric)
anova(config,strong)

#summarize fitmeasures
fitconfig<-fitmeasures(config,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))
fitmetric<-fitmeasures(metric,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))
fitstrong<-fitmeasures(strong,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))
fit1<-rbind(fitconfig,fitmetric,fitstrong)
rownames(fit1)<-c("configural","metric","strong")
chidf<-fit1[,1]/fit1[,2]
fit1<-cbind(fit1,chidf)
round(fit1,3)



#*******************************************
# QUESTION 4 - STRUCTURAL EQUATION MODELS
#*******************************************



# we set the working directory
setwd("C:/Users/Balazs/Documents")

# we load the wvs data
load("wvs.Rdata")

#We install the "lavaan" package.
install.packages("lavaan")
# we load the 'lavaan' package
library(lavaan)


# We create a new data frame : "MNdata.Mbq". This new data frame filters the countries, to only include data (rows) corresponding to Malaysia and the Netherlands.
MNdata.Mbq = subset.data.frame(wvs, country == "Malaysia" | country == "Netherlands")
MNdata.Mbq


#We create a data frame for the data (rows) corresponding to Malaysia.We call this "totalmatrix_Malaysia".
totalmatrix_Malaysia = subset.data.frame(MNdata.Mbq,country == "Malaysia")
totalmatrix_Malaysia
#We create a data frame for the data (rows) corresponding to the Netherlands. We call this "totalmatrix_Netherlands".
totalmatrix_Netherlands = subset.data.frame(MNdata.Mbq,country == "Netherlands")
totalmatrix_Netherlands

#There is no need to create a new data frame to only contain the data (columns #16-33)  corresponding to "Importance of Religion" and "Justifiability of Behaviors", because when defining our Exogenous and Endogenous variables for SEM, we only select the columns which we need anyway.



#Analysis of the Structural Equation Model
sem1 <- '


  # Our Measurement Model: Here, we define our Exogenous or Explanatory (Importance of Religion) and Endogenous or Explained (Justifiability of Behaviors) variables using indicators like "R_pray" and "J_claiming_benefits" feeding into the variables.
  # We force the factor "R_pray" to be freely estimated by multiplying it with NA. We also force the indicator "J_claiming_benefits" to be equal to 1.
  relig       =~ NA*R_pray + R_importance_God + R_attend_religious_services
	justif      =~ 1*J_claiming_benefits + J_avoiding_fare + J_stealing_property + J_cheating_taxes + J_accept_bribe + J_homosexuality + J_prostitution + J_abortion +  J_divorce + J_sex_before_marriage + J_suicide +  J_beat_wife + J_parents_beating_children + J_violence
  


  # Our Structural Model: This describes the causal relations between the latent variables.
  justif      ~ relig 

  # We estimate the variances of the latent variables. We fix the variance of "relig" ("Xi") to 1. We do not set the variance of  "justif" ("eta") to 1, since the structural relations cause the covariance structure of "justif"("eta") to be more complex.
  relig       ~~ 1*relig
  justif      ~~ justif

  
  '
#We first run the analysis for Malaysia, and then for the Netherlands.

#The SEM analysis for Malaysia and its summary.
fit1 = sem(sem1, data=totalmatrix_Malaysia)
summary(fit1)

#The standardizedSolution values show the values of the factor loadings. From this we can calculate how reliable the factor loadings are by taking the values' square.
standardizedSolution(fit1)

#The fitmeasures show different statistics, including whether our model is a good fit of the data.
fitmeasures(fit1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#The modificationIndices tell us how we can improve our fit of the model to the data. When we see a high enough value under mi, depending on whether it is a covariance or a correlation, by adding it to our model, we will improve our model's fit to the data.
modificationIndices(fit1)


#The SEM analysis for the Netherlands and its summary.
fit2 = sem(sem1, data=totalmatrix_Netherlands)
summary(fit2)
#The standardizedSolution values show the values of the factor loadings. From this we can calculate how reliable the factor loadings are by taking the values' square.

standardizedSolution(fit2)
#The fitmeasures show different statistics, including whether our model is a good fit of the data.

fitmeasures(fit2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#The modificationIndices tell us how we can improve our fit of the model to the data. When we see a high enough value under mi, depending on whether it is a covariance or a correlation, by adding it to our model, we will improve our model's fit to the data.
modificationIndices(fit2)




#*******************************************
# QUESTION 5 - CANONICAL CORRELATION
#*******************************************

#Only data for Malaysia and Netherlands
library(candisc)
MNdata = subset.data.frame(wvs, country == "Malaysia" | country == "Netherlands")

#Set of X and Y variables (for Malaysia and Netherlands)
XY <- data.frame(MNdata$R_attend_religious_services, MNdata$R_pray, MNdata$R_importance_God, MNdata$CR_robberies, MNdata$CR_alcohol, MNdata$CR_police_military, MNdata$CR_racist_behavior, MNdata$CR_drug_sale, MNdata$J_claiming_benefits, MNdata$J_avoiding_fare, MNdata$J_stealing_property, MNdata$J_cheating_taxes, MNdata$J_accept_bribe, MNdata$J_homosexuality, MNdata$J_prostitution, MNdata$J_abortion, MNdata$J_divorce, MNdata$J_sex_before_marriage, MNdata$J_suicide, MNdata$J_beat_wife, MNdata$J_parents_beating_children, MNdata$J_violence,MNdata$country)
colnames(XY) <- c("R_attend_religious_services", "R_pray", "R_importance_God", "CR_robberies", "CR_alcohol", "CR_police_military", "CR_racist_behavior", "CR_drug_sale", "J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence","Country")

#Standardize X and Y variables (for Malaysia and Netherlands) ##Make data frame for these standardized variables
zMNdata <- data.frame(XY)
zMNdata[,1:22] <- scale(zMNdata[,1:22], center=TRUE,scale=TRUE)

lm.out<-lm(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zMNdata)
summary(lm.out)

#Conduct canonical correlation analysis
cancor.out <- cancor(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zMNdata)
summary(cancor.out)

#print canonical loadings
cancor.out$structure$X.xscores
cancor.out$structure$Y.yscores

#print redundancies
redu <- redundancy(cancor.out)
round(redu$Ycan,3)

#Computation redundancies from output
R2tu <- cancor.out$cancor^2
VAFYbyt <- apply(cancor.out$structure$Y.yscores^2,2,sum)/14
redund <- R2tu*VAFYbyt
round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),3)

#Canonical variates
cancor.out$scores$X
cancor.out$scores$Y

can1<-cbind(cancor.out$scores$X[,1],cancor.out$scores$Y[,1])
rownames(can1)<-as.character(XY[,23])
plot(can1,xlab="u1",ylab="t1",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
#identify points in the plot
identify(can1,labels=as.character(XY[,23]),col = "blue")
cancor.out$structure$X.xscores[,1]
cancor.out$structure$Y.yscores[,1]

can2<-cbind(cancor.out$scores$X[,2],cancor.out$scores$Y[,2])
rownames(can2)<-as.character(XY[,23])
plot(can2,xlab="u2",ylab="t2",xlim=c(-6,5),ylim=c(-8,4))
#identify points in the plot
identify(can2,labels=as.character(XY[,23]),col = "blue")
cancor.out$structure$X.xscores[,2]
cancor.out$structure$Y.yscores[,2]

can3<-cbind(cancor.out$scores$X[,3],cancor.out$scores$Y[,3])
rownames(can3)<-as.character(XY[,23])
plot(can3,xlab="u3",ylab="t3",xlim=c(-5,5),ylim=c(-5,4))
#identify points in the plot
identify(can3,labels=as.character(XY[,23]),col = "blue")
cancor.out$structure$X.xscores[,3]
cancor.out$structure$Y.yscores[,3]

can4<-cbind(cancor.out$scores$X[,4],cancor.out$scores$Y[,4])
rownames(can4)<-as.character(XY[,23])
plot(can4,xlab="u4",ylab="t4",xlim=c(-6,6),ylim=c(-6,5))
#identify points in the plot
identify(can4,labels=as.character(XY[,23]),col = "blue")
cancor.out$structure$X.xscores[,4]
cancor.out$structure$Y.yscores[,4]

can5<-cbind(cancor.out$scores$X[,5],cancor.out$scores$Y[,5])
rownames(can5)<-as.character(XY[,23])
plot(can4,xlab="u5",ylab="t5",xlim=c(-6,6),ylim=c(-6,5))
#identify points in the plot
identify(can5,labels=as.character(XY[,23]),col = "blue")
cancor.out$structure$X.xscores[,5]
cancor.out$structure$Y.yscores[,5]

#*****************************************
#canonical variates per country
#*****************************************


#Only data for Malaysia or Netherlands
MYdata = subset.data.frame(wvs, country == "Malaysia")
NLdata = subset.data.frame(wvs, country == "Netherlands")

#Set of X and Y variables (for Malaysia)
MYXY <- data.frame(MYdata$R_attend_religious_services, MYdata$R_pray, MYdata$R_importance_God, MYdata$CR_robberies, MYdata$CR_alcohol, MYdata$CR_police_military, MYdata$CR_racist_behavior, MYdata$CR_drug_sale, MYdata$J_claiming_benefits, MYdata$J_avoiding_fare, MYdata$J_stealing_property, MYdata$J_cheating_taxes, MYdata$J_accept_bribe, MYdata$J_homosexuality, MYdata$J_prostitution, MYdata$J_abortion, MYdata$J_divorce, MYdata$J_sex_before_marriage, MYdata$J_suicide, MYdata$J_beat_wife, MYdata$J_parents_beating_children, MYdata$J_violence,MYdata$country)
colnames(MYXY) <- c("R_attend_religious_services", "R_pray", "R_importance_God", "CR_robberies", "CR_alcohol", "CR_police_military", "CR_racist_behavior", "CR_drug_sale", "J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence","Country")
#Set of X an Y variables (for Netherlands)
NLXY <- data.frame(NLdata$R_attend_religious_services, NLdata$R_pray, NLdata$R_importance_God, NLdata$CR_robberies, NLdata$CR_alcohol, NLdata$CR_police_military, NLdata$CR_racist_behavior, NLdata$CR_drug_sale, NLdata$J_claiming_benefits, NLdata$J_avoiding_fare, NLdata$J_stealing_property, NLdata$J_cheating_taxes, NLdata$J_accept_bribe, NLdata$J_homosexuality, NLdata$J_prostitution, NLdata$J_abortion, NLdata$J_divorce, NLdata$J_sex_before_marriage, NLdata$J_suicide, NLdata$J_beat_wife, NLdata$J_parents_beating_children, NLdata$J_violence,NLdata$country)
colnames(NLXY) <- c("R_attend_religious_services", "R_pray", "R_importance_God", "CR_robberies", "CR_alcohol", "CR_police_military", "CR_racist_behavior", "CR_drug_sale", "J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe", "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_suicide", "J_beat_wife", "J_parents_beating_children", "J_violence","Country")

#Standardize X and Y variables (for Malaysia)
zMYdata <- data.frame(MYXY)
zMYdata[,1:22] <- scale(zMYdata[,1:22], center=TRUE,scale=TRUE)
#Standardize X and Y variables (for Netherlands)
zNLdata <- data.frame(NLXY)
zNLdata[,1:22] <- scale(zNLdata[,1:22], center=TRUE,scale=TRUE)

#LM for Malaysia
lm.out.MY<-lm(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zMYdata)
summary(lm.out.MY)
#LM for Netherlands
lm.out.NL<-lm(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zNLdata)

#Conduct canonical correlation analysis for Malaysia
cancor.out.MY <- cancor(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zMYdata)
summary(cancor.out.MY)
#Conduct canonical correlation analysis for Netherlands
cancor.out.NL <- cancor(cbind(J_claiming_benefits, J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe, J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, J_parents_beating_children, J_violence)~R_attend_religious_services+R_pray+R_importance_God+CR_robberies+CR_alcohol+CR_police_military+CR_racist_behavior+CR_drug_sale,data=zNLdata)
summary(cancor.out.NL)

#Canonical variates for Malaysia
can1MY<-cbind(cancor.out.MY$scores$X[,1],cancor.out.MY$scores$Y[,1])
rownames(can1MY)<-as.character(MYXY[,23])
plot(can1MY,main="Malaysia",xlab="u1",ylab="t1",xlim=c(-5,4),ylim=c(-5,4))
#identify points in the plot
identify(can1,labels=as.character(MYXY[,23]),col = "blue")
cancor.out.MY$structure$X.xscores[,1]
cancor.out.MY$structure$Y.yscores[,1]

can2MY<-cbind(cancor.out.MY$scores$X[,2],cancor.out.MY$scores$Y[,2])
rownames(can2MY)<-as.character(MYXY[,23])
plot(can2MY,main="Malaysia",xlab="u2",ylab="t2",xlim=c(-5,5),ylim=c(-5,7))
#identify points in the plot
identify(can2MY,labels=as.character(MYXY[,23]),col = "blue")
cancor.out.MY$structure$X.xscores[,2]
cancor.out.MY$structure$Y.yscores[,2]

can3MY<-cbind(cancor.out.MY$scores$X[,3],cancor.out.MY$scores$Y[,3])
rownames(can3MY)<-as.character(MYXY[,23])
plot(can3MY,main="Malaysia",xlab="u3",ylab="t3",xlim=c(-5,5),ylim=c(-5,5))
#identify points in the plot
identify(can3MY,labels=as.character(MYXY[,23]),col = "blue")
cancor.out.MY$structure$X.xscores[,3]
cancor.out.MY$structure$Y.yscores[,3]

can4MY<-cbind(cancor.out.MY$scores$X[,4],cancor.out.MY$scores$Y[,4])
rownames(can4MY)<-as.character(MYXY[,23])
plot(can4MY,main="Malaysia",xlab="u4",ylab="t4",xlim=c(-6,6),ylim=c(-6,6))
#identify points in the plot
identify(can4MY,labels=as.character(MYXY[,23]),col = "blue")
cancor.out.MY$structure$X.xscores[,4]
cancor.out.MY$structure$Y.yscores[,4]

can5MY<-cbind(cancor.out.MY$scores$X[,5],cancor.out.MY$scores$Y[,5])
rownames(can5MY)<-as.character(MYXY[,23])
plot(can5MY,main="Malaysia",xlab="u5",ylab="t5",xlim=c(-6,5),ylim=c(-6,6))
#identify points in the plot
identify(can5MY,labels=as.character(MYXY[,23]),col = "blue")
cancor.out.MY$structure$X.xscores[,5]
cancor.out.MY$structure$Y.yscores[,5]

#Canonical variates for Netherlands
can1NL<-cbind(cancor.out.NL$scores$X[,1],cancor.out.NL$scores$Y[,1])
rownames(can1NL)<-as.character(NLXY[,23])
plot(can1NL,main="Netherlands",xlab="u1",ylab="t1",xlim=c(-4,2.5),ylim=c(-4,3))
#identify points in the plot
identify(can1NL,labels=as.character(NLXY[,23]),col = "blue")
cancor.out.NL$structure$X.xscores[,1]
cancor.out.NL$structure$Y.yscores[,1]

can2NL<-cbind(cancor.out.NL$scores$X[,2],cancor.out.NL$scores$Y[,1])
rownames(can2NL)<-as.character(NLXY[,23])
plot(can2NL,main="Netherlands",xlab="u2",ylab="t2",xlim=c(-5.5,2),ylim=c(-4,3))
#identify points in the plot
identify(can2NL,labels=as.character(NLXY[,23]),col = "blue")
cancor.out.NL$structure$X.xscores[,2]
cancor.out.NL$structure$Y.yscores[,2]

can3NL<-cbind(cancor.out.NL$scores$X[,3],cancor.out.NL$scores$Y[,3])
rownames(can3NL)<-as.character(NLXY[,23])
plot(can3NL,main="Netherlands",xlab="u3",ylab="t3",xlim=c(-5,5),ylim=c(-5,5))
#identify points in the plot
identify(can3NL,labels=as.character(NLXY[,23]),col = "blue")
cancor.out.NL$structure$X.xscores[,3]
cancor.out.NL$structure$Y.yscores[,3]

can4NL<-cbind(cancor.out.NL$scores$X[,4],cancor.out.NL$scores$Y[,4])
rownames(can4NL)<-as.character(NLXY[,23])
plot(can4NL,main="Netherlands",xlab="u4",ylab="t4",xlim=c(-5.5,4),ylim=c(-5,5))
#identify points in the plot
identify(can4NL,labels=as.character(NLXY[,23]),col = "blue")
cancor.out.NL$structure$X.xscores[,4]
cancor.out.NL$structure$Y.yscores[,4]

can5NL<-cbind(cancor.out.NL$scores$X[,5],cancor.out.NL$scores$Y[,5])
rownames(can5NL)<-as.character(NLXY[,23])
plot(can5NL,main="Netherlands",xlab="u5",ylab="t5",xlim=c(-4.5,4.5),ylim=c(-5,5))
#identify points in the plot
identify(can5NL,labels=as.character(NLXY[,23]),col = "blue")
cancor.out.NL$structure$X.xscores[,5]
cancor.out.NL$structure$Y.yscores[,5]

#***************************
# Validity of the solution
#***************************

# split data into training and validation set
train_data <- XY[seq(2,2473,by = 2),]
valid_data <- XY[seq(1,2473,by = 2),]

# standardizing the data
train_data[,1:22] <- scale(train_data[,1:22],center = TRUE, scale = TRUE)
valid_data[,1:22] <- scale(valid_data[,1:22],center = TRUE, scale = TRUE)

# conduct CCA on training data to obtain u = Xa and t = Yb
cancor.train <- cancor(cbind(J_claiming_benefits,J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe,
                             J_homosexuality ,J_prostitution,J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, 
                             J_parents_beating_children, J_violence) ~ 
                         R_attend_religious_services+R_pray+R_importance_God+CR_robberies+ CR_alcohol+ CR_police_military+ CR_racist_behavior + CR_drug_sale,
                       data = train_data)
summary(cancor.train)
cancor.train$structure$X.xscores 
cancor.train$structure$Y.yscores

# conduct CCA on validation data to obtain a* and b*
cancor.valid <- cancor(cbind(J_claiming_benefits,J_avoiding_fare, J_stealing_property, J_cheating_taxes, J_accept_bribe,
                             J_homosexuality ,J_prostitution,J_abortion, J_divorce, J_sex_before_marriage, J_suicide, J_beat_wife, 
                             J_parents_beating_children, J_violence) ~ 
                         R_attend_religious_services+R_pray+R_importance_God+CR_robberies+ CR_alcohol+ CR_police_military+ CR_racist_behavior + CR_drug_sale,
                       data = valid_data)
summary(cancor.valid)
cancor.valid$structure$X.xscores 
cancor.valid$structure$Y.yscores

# Canonical variates - Training set
train.X1<-cancor.train$score$X
train.Y1<-cancor.train$score$Y


# Compute u* = Xa* and t* = Yb* - cononical variates of training set and coefficients of the validation set
train.X2<-as.matrix(train_data[,1:8])%*%cancor.valid$coef$X
train.Y2<-as.matrix(train_data[,9:22])%*%cancor.valid$coef$Y

#R(T,T*) and R(U,U*)
round(cor(train.Y1,train.Y2)[1:5,1:5],3)
round(cor(train.X1,train.X2)[1:5,1:5],3)

#R(U*,T*) versus R(U,T)
round(cor(train.X1,train.Y1)[1:5,1:5],3)
round(cor(train.X2,train.Y2)[1:5,1:5],3)

#R(T*,T*) and R(U*,U*)
round(cor(train.Y2,train.Y2)[1:5,1:5],3)
round(cor(train.X2,train.X2)[1:5,1:5],3)


round(cancor.out$structure$X.xscores[,1],2)
round(cancor.out$structure$Y.yscores[,1],2)






























