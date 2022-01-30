#============================= Generalized Linear Models Group 6 =======================================

#================================== Assignment 1 ========================================================
library(tidyverse)
library(DHARMa) # randomized quantile residuals
library(MASS)
library(sandwich) # robust var-cov estimator
library(lmtest) # robust var-cov estimator
library(countreg)

#================================ Loading the dataset =================================
#setwd('C:\\Users\\user\\Desktop\\KUL - Mstat\\Generalized Linear Models')
setwd("C://Users/sanne/Documents/Sanne/KULeuven/MA Statistics 2020-2021/2th Semester/Generalized Linear Models/Assignment1")
victim_data <- read.table('16-victim.txt', header = TRUE)
victim_data <- victim_data %>% transform(race = factor(race, levels = c('white', 'black')))
victim_data$race <- relevel(victim_data$race, ref = 'white')
# victim_data <- victim_data %>% transform(race = ifelse(race == 'black', 1, 0)) 

#================================ EXPLORATORY DATA ANALYSIS ============================
hist(victim_data$resp,xlab="Resp",breaks = 7,labels=T,ylim=c(0,1400),main="")

# =========================== 1. Fit a Poisson Model ============================
victim_glm.poisson <- glm(resp ~ race, family = poisson(link = 'log'), data = victim_data)
summary(victim_glm.poisson)

# Likelihood Ratio Test
victim_glm.poisson_null <- glm(resp ~ 1,  family = poisson(link = 'log'),  data = victim_data)
anova(victim_glm.poisson, victim_glm.poisson_null, test = 'Chisq')

#Score Test
anova(victim_glm.poisson, victim_glm.poisson_null, test = 'Rao')

#====================== 2.Calculate the risk ratio and the corresponding confidence interval ==============================
# C.I. Wald method
cbind('Coefficients' = exp(victim_glm.poisson$coefficients), exp(confint.default(victim_glm.poisson)))

# C.I. Profile Likelihood method
cbind('Coefficients' = exp(victim_glm.poisson$coefficients), exp(confint(victim_glm.poisson)))

# ===================== 3. Calculate the ratio of the means of the response for each race 
#                            (mean response for black/mean response for white) ====================
new_data <- data.frame('race' = 'black')
pred_black_mean <- exp(predict(victim_glm.poisson, new_data))

new_data <- data.frame('race' = 'white')
pred_white_mean <- exp(predict(victim_glm.poisson, new_data))

cbind('mean_pred_resp_black' = pred_black_mean, 
      'mean_pred_resp_white' = pred_white_mean, 
      'pred_ratio' = pred_black_mean / pred_white_mean)

# ===================  4. Calculate the predictions of the models for each race ==============
cbind('mean_pred_resp_black' = pred_black_mean*10, 'mean_pred_resp_white' = pred_white_mean*10)

# =================== 5. Analyze the GOF of the model (GOF) ====================================
# Pearson-Chisq test
chisq <- sum(resid(victim_glm.poisson, type = 'pearson')^2)
df <- summary(victim_glm.poisson)$df.residual
pchisq(chisq, df, lower.tail = FALSE) #Rejects the model

# Deviance test
dev <- sum(resid(victim_glm.poisson, type = 'deviance')^2)
df <- summary(victim_glm.poisson)$df.residual
pchisq(dev, df, lower.tail = FALSE) #Do not reject

# Randomized Quantile Residuals
victim_glm.poisson_sim <- simulateResiduals(victim_glm.poisson, plot = TRUE)

# Residuals distribution (Optional graph)
hist(victim_glm.poisson_sim)

# Rootogram
rootogram(victim_glm.poisson, col = 'blue', ylab = 'Root Square of Frequency', main = 'Poisson Model')

# Observed vs predicted zeros from Poisson regression
zero_obs <- victim_data$resp == 0
zero_poisson <- exp(-exp(predict(victim_glm.poisson)))
c('obs_zero' = mean(zero_obs), 'poisson_zero' = mean(zero_poisson))

# Uniformity test
testUniformity(victim_glm.poisson_sim, plot = TRUE)

# Dispersion test
testDispersion(victim_glm.poisson_sim, plot = TRUE)

# Robust variance-covariance estimator
(victim_glm.poisson_rb <- coeftest(victim_glm.poisson, vcov = sandwich(victim_glm.poisson)))

# ================================ 6. Fit a negative binomial model  ==========================
victim_glm.nb <- glm.nb(resp ~ race, data = victim_data)
summary(victim_glm.nb)

# Randomized Quantile Residuals
victim_glm.nb_sim <- simulateResiduals(victim_glm.nb, plot = TRUE)

# Uniformity test
testUniformity(victim_glm.nb_sim, plot = TRUE)

# Dispersion test
testDispersion(victim_glm.nb_sim, plot = TRUE)

# Rootogram
rootogram(victim_glm.nb, col = 'blue', ylab = 'Root Square of Frequency', main = 'Poisson Model')

# estimated model based variances (per race) for the counts and Compare them with the observed variances.
estimated_var.nb <- function(model, race){
  new_data <- data.frame('race' = race)
  mu <- predict(model, new_data, type = 'response')
  return(as.numeric(mu + mu^2/model$theta))
}
c('obs_var(black)' = var(victim_data$resp[victim_data$race == 'black']),
  'obs_var(white)' = var(victim_data$resp[victim_data$race == 'white']),
  'est_var(black)' = estimated_var.nb(victim_glm.nb, 'black'),
  'est_var(white)' = estimated_var.nb(victim_glm.nb, 'white'))

# ================================ 7. Fit a Quasi-likelihood model ============================
victim_glm.qpoisson <- glm(resp ~ race, family = quasipoisson, data = victim_data)
summary(victim_glm.qpoisson)

# =============================== 8. Discussion ===========================
# Standard error
round(data.frame('Poisson' = summary(victim_glm.poisson)$coefficients[, 2],
             'Poisson_Sandwich' = victim_glm.poisson_rb[, 2],
             'Negative_Bin' = summary(victim_glm.nb)$coefficients[, 2],
             'Quasi_Likelihood' = summary(victim_glm.qpoisson)$coefficients[, 2]),
  digits = 4)

## Mean-varinace relationship of Quasi and Negative Binomial Models
# Observed mean
resp_mean <- tapply(victim_data$resp, victim_data$race, FUN = mean)
# Observed variance
resp_var <- tapply(victim_data$resp, victim_data$race, FUN = var)
# Plot data
plot(resp_mean, resp_var, col = 'blue', xlim = c(0, 0.7), ylim = c(0.1, 1.5),
     main = 'Mean-Variance Relationship', xlab = 'Mean', ylab = 'Variance')
# Plot Negative Binomial and Quasilikelihood Poisson means and variances
nb.func <- function(x) x + x^2/victim_glm.nb$theta
qpo.func <- function(x) x*summary(victim_glm.qpoisson)$dispersion
curve(nb.func(x), from = 0, to = 0.7, add = TRUE, col = 'red')
curve(qpo.func(x), from = 0, to = 0.7, add = TRUE)
# Legend
legend('topleft', legend = c('Data', 'Quasi-Likelihood Poisson', 'Negative Binomial'), col = c('blue', 'black', 'red'), lty = c(NA, 1, 1), pch = c(1, NA, NA))

# =============================== Assignment 2 ======================================================

install.packages('aod')
install.packages('msm')
library(tidyverse)
library(aod)
library(msm)
library(boot)
library(dplyr)
# ============================= loading the dataset ==================================================

#setwd('C:\\Users\\user\\Desktop\\KUL - Mstat\\Generalized Linear Models')
setwd("C://Users/sanne/Documents/Sanne/KULeuven/MA Statistics 2020-2021/2th Semester/Generalized Linear Models/Assignment2")
admission_data <- read.table('01-admission.txt', header = TRUE)

# ============================== Transformation =====================================================
admission_data <- admission_data %>% 
  transform(rank = as.factor(rank), gre = as.numeric(gre), gpa = as.numeric(gpa)) %>%
  select(-Obs)

# ============================== logistic regression model =========================================
admission_glm <- glm(admit ~ gre + gpa + rank, family = binomial(link = 'logit'), data = admission_data)
summary(admission_glm)

# ============= 1. Constructing Confidence Intervals for the coefficients ==============
# CI Wald method
cbind('Coefficients' = admission_glm$coefficients, confint.default(admission_glm))

# CI Profile Likelihood method
cbind('Coefficients' = admission_glm$coefficients, confint(admission_glm))

# ============ 2. Test the overall effect of rank ====================
# Likelihood ratio test
admission_glm.reduced <- glm(admit ~ gre + gpa, family = binomial(link = 'logit'), data = admission_data)
anova(admission_glm.reduced, admission_glm, test = 'Chisq')

# Wald's test
wald.test(Sigma = vcov(admission_glm), b = coef(admission_glm), Terms = 4:6)

# score test 
anova(admission_glm.reduced, admission_glm, test = 'Rao')

# =============== 3. Is the coefficient for rank=2 is equal to the coefficient for rank=3? ========
# Wald's test Delta Method
coef_mle <- coef(admission_glm)
vcov_mle <- vcov(admission_glm)
se_diff <- deltamethod(g ~ x4 - x5, coef_mle, vcov_mle)
hat_diff <- coef_mle[4] - coef_mle[5]
cat('\n difference between rank2 and rank3 is estimated to be', '\n', hat_diff, 'with 95% Wald CI (',
    round(hat_diff + c(-1, 1)*qnorm(0.975)*se_diff, 2),') \n')

# Bootstrap method
hat_diff <- function(data, indices, formula){
  glm_boot <- glm(formula, family = binomial(link = 'logit'), data = data[indices, ])
  coef_mle <- coef(glm_boot)
  return(as.numeric(coef_mle[4] - coef_mle[5]))
}
ci_type <- c('norm', 'basic', 'perc', 'bca')
set.seed(20210303)
boot_obj <- boot(admission_data, hat_diff, R = 1000, 
                 formula = admit ~ gre + gpa + rank)
boot.ci(boot_obj, index = 1, type = ci_type)

# Plots
par(mfrow=c(1, 2))
hist(boot_obj$t, col = NULL, border = 'blue', main = 'Bootstrapped statistics', 
     xlab = expression(paste('Bootstrapped statistic for ', beta[4], ' - ', beta[5])))
qqnorm(boot_obj$t, col = 'blue')
qqline(boot_obj$t, col = 'red')

# =========== 4. Calculate and interpret the odds ratios associated with each covariate ===========
# Odd ratios

# 10 points increase in GRE
odd_ratios <- exp(admission_glm$coefficients[2]*10)
odd_ratios_confint <- exp(confint(admission_glm)[2,]*10)
cbind('Odd ratios' = odd_ratios, 'Lower' = odd_ratios_confint[1], 'Upper' = odd_ratios_confint[2])

# 0.1 point increase in GPA
odd_ratios <- exp(admission_glm$coefficients[3]*0.1)
odd_ratios_confint <- exp(confint(admission_glm)[3,]*0.1)
cbind('Odd ratios' = odd_ratios, 'Lower' = odd_ratios_confint[1], 'Upper' = odd_ratios_confint[2])

# rank relative to rank1
odd_ratios <- exp(admission_glm$coefficients[4:6])
odd_ratios_confint <- exp(confint(admission_glm)[4:6,])
cbind('Odd ratios' = odd_ratios, 'Lower' = odd_ratios_confint[, 1], 'Upper' = odd_ratios_confint[, 2])

# =========== 5. Calculating the predicted probability of admission at each value of rank, ==========
#                         holding the other covariates constant at their means
mean_gre <- mean(admission$gre)
mean_gpa <- mean(admission$gpa)
pred_rank <- function(x) {
  pred <- predict(admission_glm, 
                  newdata = data.frame(gre = mean_gre, gpa = mean_gpa, rank = x), 
                  type = 'response')
}
df <- rbind('Rank 1' = pred_rank('1'), 'Rank 2' = pred_rank('2'), 
            'Rank 3' = pred_rank('3'), 'Rank 4' = pred_rank('4'))
colnames(df) <- "Predicted Probabilities"
df

# ============================= 6. Assess the model fit ======================================
# Hosmer-Lemeshow test
hosmerlem <- function(y, yhat, g = 10){
  fcutyhat <- cut(yhat,  
                  breaks = quantile(yhat, probs = seq(0, 1, 1/g)), 
                  include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ fcutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ fcutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  return(list(chisq = chisq, p.value = P))
}
hl_test <- hosmerlem(admission$admit, fitted(admission_glm))
cbind('Chi. Sq.' = hl_test$chisq, 'p-value' = hl_test$p.value)

# Detect influential observations
# Delta Chi-squares, Delta Deviance and Cook's distance
res_ps <- resid(admission_glm, type = 'pearson')/
  sqrt(1 - hatvalues(admission_glm))
res_dev <- resid(admission_glm, type = 'deviance')
delta_chisq <- res_ps^2
delta_dev <- res_dev^2 + hatvalues(admission_glm)*res_ps
cook_dist <- resid(admission_glm, type = 'pearson')^2 * 
  hatvalues(admission_glm) /(length(coef(admission_glm)) * 
                               (1 - hatvalues(admission_glm))^2)

# Delta Chi-Square vs Case Number
plot(1:nrow(admission), delta_chisq, type = 'l', col = 'black', las = 1,cex.lab = 1.5,cex.main = 1.5,
     main = 'Delta Chi-square', xlab = 'Case Number', ylab = 'Delta Chi-square')
points(1:nrow(admission), delta_chisq, col = 'red', pch = 16)
text(198, 7.8, labels = '198')
text(40, 7.8, labels = '40')
text(342, 7.8, labels = '342')
text(156, 7.8, labels = '156')

# Delta Deviance vs Case Number
plot(1:nrow(admission), delta_dev, type = 'l', col = 'black', las = 1,cex.lab = 1.5,cex.main = 1.5,
     main = 'Delta Deviance', xlab = 'Case Number', ylab = 'Delta Deviance')
points(1:nrow(admission), delta_dev, col = 'red', pch = 16)
text(198, 4.4, labels = '198')
text(40, 4.4, labels = '40')
text(342, 4.4, labels = '342')
text(156, 4.4, labels = '156')

# Cook's distance vs Case Number
plot(1:nrow(admission), cook_dist, type = 'l', col = 'black', las = 1,cex.lab = 1.5,cex.main = 1.5,
     main = 'Cook distance', xlab = 'Case Number', ylab = 'Cook distance')
points(1:nrow(admission), cook_dist, col = 'red', pch = 16)
text(198, 0.019, labels = '198')
text(40, 0.019, labels = '40')
text(342, 0.019, labels = '342')
text(156, 0.019, labels = '156')


## Deviance residuals vs predictors
# vs GRE
plot(admission$gre, res_dev, col = 'black',cex.lab = 1.5,cex.main = 1.5,
     xlab = 'GRE', ylab = 'Deviance Residuals', main = 'Residuals against GRE plot')
abline(h = 0, col = 'grey')

loess_fit<- loess(res_dev ~ admission$gre)
loess_pred <- predict(loess_fit, se = TRUE)
ord <- order(admission$gre)
lines(admission$gre[ord], loess_pred$fit[ord], col = 'blue')
lines(admission$gre[ord], loess_pred$fit[ord] + 2*loess_pred$se[ord], 
      col = 'red', lty = 2)
lines(admission$gre[ord], loess_pred$fit[ord] - 2*loess_pred$se[ord], 
      col = 'red', lty = 2)

# vs GPA
plot(admission$gpa, res_dev, col = 'black',cex.lab = 1.5,cex.main = 1.5,
     xlab = 'GPA', ylab = 'Deviance Residuals', main = 'Residuals against GPA plot')
abline(h = 0, col = 'grey')

loess_fit<- loess(res_dev ~ admission$gpa)
loess_pred <- predict(loess_fit, se = TRUE)
ord <- order(admission$gpa)
lines(admission$gpa[ord], loess_pred$fit[ord], col = 'blue')
lines(admission$gpa[ord], loess_pred$fit[ord] + 2*loess_pred$se[ord], 
      col = 'red', lty = 2)
lines(admission$gpa[ord], loess_pred$fit[ord] - 2*loess_pred$se[ord], 
      col = 'red', lty = 2)

# Main Effects - GRE
plot(admission$gre, jitter(admission$admit, factor = 0.2), cex.lab = 1.5,cex.main = 1.5,
     xlab = 'GRE', ylab = 'Probabilty (jitter)',
     main = 'Main effect - GRE')
x <- seq(min(admission$gre), max(admission$gre), by = 1)
add_line_gre <- function(x, g, col){
  fitted_prob <- predict(admission_glm, 
                         newdata = data.frame(gre = x, gpa = mean_gpa, rank = g), 
                         type = 'response')
  lines(x, fitted_prob, col = col)
}
add_line_gre(x, '1', 'blue')
add_line_gre(x, '2', 'red')
add_line_gre(x, '3', 'green')
add_line_gre(x, '4', 'purple')
legend('topleft', legend = c('rank1', 'rank2', 'rank3', 'rank4'), cex = 0.7,
       col = c('blue', 'red', 'green', 'purple'), lty = 1)

# Main Effects - GPA
plot(admission$gpa, jitter(admission$admit, factor = 0.2), cex.lab = 1.5,cex.main = 1.5,
     xlab = 'GPA', ylab = 'Probabilty (jitter)',
     main = 'Main effect - GPA')
x <- seq(min(admission$gpa), max(admission$gpa), by = 0.01)
add_lines_gpa <- function(x, g, col){
  fitted_prob <- predict(admission_glm, 
                         newdata = data.frame(gre = mean_gre, gpa = x, rank = g), 
                         type = 'response')
  lines(x, fitted_prob, col = col)
}
add_lines_gpa(x, '1', 'blue')
add_lines_gpa(x, '2', 'red')
add_lines_gpa(x, '3', 'green')
add_lines_gpa(x, '4', 'purple')
legend('topleft', legend = c('rank1', 'rank2', 'rank3', 'rank4'), cex = 0.7,
       col = c('blue', 'red', 'green', 'purple'), lty = 1)



