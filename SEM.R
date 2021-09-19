#----------------------------STRUCTURAL EQUATION MODELS-------------------------------------

remove(list = ls())

attach(wvs)
names(wvs)

## Creation of the dataframe

wvs_data <- wvs %>% select(V_tradition, 
                    #indicators for importance of religion
                    R_attend_religious_services, R_pray, R_importance_God,
                    #indicators of justification of social taboos
                    J_homosexuality, J_prostitution, J_abortion, J_divorce, J_sex_before_marriage,
                    #Country
                    country)

wvs_df <- as.data.frame(wvs_data)


colnames(wvs_df) <- c("I_tradition", "R_attend_religious_services", "R_pray", "R_importance_god",
                      "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce",
                      "J_sex_before_marriage", "country")

table_1 <- subset(wvs_df, country == "Australia")
table_aus <- table_1[-10]
table_2 <- subset(wvs_df, country == "Pakistan")
table_pak <- table_2[-10]

wvs_coded <- rbind(table_1,table_2)
sem_data <- rbind(table_aus, table_pak)

## Exploration

descriptive_sem_data <- as.data.frame(psych::describe(sem_data))

descriptive_sem_data <- dplyr::select(descriptive_sem_data, n, mean, sd, median, min,
                                      max, skew, kurtosis)
descriptive_sem_data

# Covariance matrix of the variables

(cov_sem_data <- cov(sem_data))
(cov_table_aus <- cov(table_aus))
(cov_table_pak <- cov(table_pak))

# Plotting cov matrix using corrplot

(cor_sem_data <- cov2cor(cov_sem_data))
(cor_table_aus <- cov2cor(cov_table_aus))
(cor_table_pak <- cov2cor(cov_table_pak))


corrplot::corrplot(cor_sem_data, 
                   is.corr = TRUE,        
                   method = "circle",     
                   type = "upper",        
                   addCoef.col = "black")  

corrplot::corrplot(cor_table_aus, 
                   is.corr = TRUE,        
                   method = "circle",     
                   type = "upper",        
                   addCoef.col = "black")  

corrplot::corrplot(cor_table_pak, 
                   is.corr = TRUE,        
                   method = "circle",     
                   type = "upper",        
                   addCoef.col = "black")  


## Checking the direct and indirect effect of I_tradition and I_religious_values on J_social_taboos
# using Mediation model

effects_model <- '## latent factors ##
       J_social_taboos =~ J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage
       I_religious_values =~ R_attend_religious_services+R_pray+R_importance_god
       ## Direct effect ##
       J_social_taboos ~ c*I_tradition
       ## Mediator ##
       I_religious_values ~ a*I_tradition
       J_social_taboos ~ b*I_religious_values
       ## Indirect effect ##
       ab := a*b
       ## Total effect ##
       total := c + (a*b)'

fit_effects_model <- cfa(effects_model, data = sem_data)
summary(fit_effects_model, fit.measures = TRUE, standardized = TRUE)

# Checking the residuals (Difference between observed and implied covariance matrix)
residuals(fit_effects_model)

# Checking the fit measures
fitmeasures(fit_effects_model)

# Checking the parameter estimates with CI
parameterEstimates(fit_effects_model)

# Checking the standardized factor loadings
inspect(fit_effects_model, what = "std")$lambda

# looking at residual variances. 
theta <- round(inspect(fit_effects_model,"est")$theta,3)
theta.std <- round(inspect(fit_effects_model,"std")$theta,3) 
r2 <- round(inspect(fit_effects_model,"r2"),3)
r2
data.frame(row.names = c(),                      
           Variables=colnames(theta),            
           "Residuals"=diag(theta),            
           "Std. Residuals"=diag(theta.std))


inspect(fit_effects_model, "r2")

# Global fit measures 
fitMeasures(fit_effects_model, c("chisq", "df", "pvalue", "srmr", "cfi", "tli","rmsea"), output = "matrix")

# Modification fit indices
modificationindices(fit_effects_model)

# Local fit measures: modification indices 
mi <- inspect(fit_effects_model,"mi")
mi.sorted <- mi[order(-mi$mi),] 
mi.sorted[1:5,]


# Modifying the model
model_modified <- '## measurement part ##
                 J_social_taboos =~ J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage
                 I_religious_values =~ R_attend_religious_services+R_pray+R_importance_god
                 ## Structural part ##
                 J_social_taboos ~ I_religious_values + I_tradition
                 I_religious_values ~ I_tradition
                 ## Modification ##
                 R_attend_religious_services ~~ R_pray
                 R_attend_religious_services ~~ R_importance_god
                 J_abortion ~~ J_sex_before_marriage
                 J_prostitution ~~ J_abortion'

fit_model_modified <- cfa(model_modified, data = sem_data)
summary(fit_model_modified, fit.measures = T, standardized = T)

# Global fit measures 
fitMeasures(fit_model_modified, c("chisq", "df", "pvalue", "srmr", "cfi", "tli","rmsea"), output = "matrix")

# Checking the standardized factor loadings
inspect(fit_model_modified, what = "std")$lambda

## Multigroup modelling

wvs_coded$country <- factor(wvs_coded$country, levels = c("Australia", "Pakistan"), labels = c("Australia","Pakistan"))
str(wvs_coded)

# Configural invariance
 
fit_configural <- cfa(effects_model, data = wvs_coded, group = "country")
summary(fit_configural, fit.measures = TRUE, standardized = TRUE)

# Global fit measures
fitMeasures(fit_configural, c("chisq", "df", "pvalue", "srmr", "cfi", "tli","rmsea"), output = "matrix")


# Metric invariance

fit_metric <- cfa(effects_model, data = wvs_coded, group = "country", group.equal = c("loadings"))
summary(fit_metric, fit.measures = T, standardized = T)


# Global fit measures
fitMeasures(fit_metric, c("chisq", "df", "pvalue", "srmr", "cfi", "tli","rmsea"), output = "matrix")


# Function to extract fit indices

model_fit <-  function(lavobject) {
        vars <- c("df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
        return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit <- 
        list(model_fit(fit_configural), 
             model_fit(fit_metric)) %>% 
        reduce(rbind)


rownames(table_fit) <- c("Configural", "Metric")

table_fit

# compare the nested model using the anova function

table_anova <- list(anova(fit_configural, fit_metric),
                    anova(fit_metric)) %>%  
        reduce(rbind) %>% 
        .[-c(3, 5, 7),]

table_anova

