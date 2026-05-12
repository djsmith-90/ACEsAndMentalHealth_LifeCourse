### ACEs, Mental Health and SLCMA
### Script 4a - All analyses (except emotional neglect, as only 2 time-points: see script 4b)
### Created 30/10/2025
### R version 4.3.1

## Set working directory, clear workspace, and load packages
rm(list = ls())

setwd("X:/Studies/RSBB Team/Dan/B4563 - ACEs and MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("glmnet")
library(glmnet)

#install.packages("selectiveInference")
library(selectiveInference)

#install.packages("mice")
library(mice)

#install.packages("corrplot")
library(corrplot)


### Function to make a neater summary table of the lasso model output
lasso_table <- function(lasso_model) {
  old_covars <- ""
  old_deviance <- 0
  old_varNum <- 0
  old_lambda <- NA
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  #df
  
  for (i in 1:length(lasso_model$df)) {
    #print(i)
    new_covars <- attributes(which(lasso_model$beta[, i] != 0))$names
    new_deviance <- lasso_model$dev.ratio[i]
    new_varNum <- lasso_model$df[i]
    new_lambda <- lasso_model$lambda[i]
    #print(new_covars); print(new_deviance); print(new_varNum)
    
    # See if covars added
    if (new_varNum > old_varNum) {
      change <- setdiff(new_covars, old_covars) # Find the new covariate(s) added
      change <- paste(change, collapse = " ") # Combine variable together, if > 1
      change <- paste0(change, " (+)") # Append a "(+)" sign
      change_drop <- setdiff(old_covars, new_covars) # Make sure no covariates dropped at same time
      if (!identical(change_drop, character(0))) { # If a covar also dropped, then combine with 'change'
        change_drop <- paste(change_drop, collapse = " ") # Combine variable together, if > 1
        change <- paste0(change, " ", change_drop, " (-)")
      }
      dev_diff <- round((new_deviance - old_deviance) * 100, 3) # Diff in deviance between current and previous lambda
      new_dev <- round(new_deviance * 100, 3) # Current deviance value
      vars_noSpaces <- paste(new_covars, collapse = " ") # Combine all variables together
      temp <- cbind(change, new_dev, dev_diff, new_varNum, new_lambda, vars_noSpaces, NA, NA) # Combine values together
      df <- rbind(df, temp) # Merge with template data frame
    }
    
    # See if covars removed
    if (new_varNum < old_varNum) {
      change <- setdiff(old_covars, new_covars) # Find the covariate(s) removed
      change <- paste(change, collapse = " ") # Combine variable together, if > 1
      change <- paste0(change, " (-)") # Append a "(-)" sign
      change_add <- setdiff(new_covars, old_covars) # Make sure no covariates added at same time
      if (!identical(change_add, character(0))) { # If a covar also dropped, then combine with 'change'
        change_add <- paste(change_add, collapse = " ") # Combine variable together, if > 1
        change <- paste0(change, " ", change_add, " (+)")
      }
      dev_diff <- round((new_deviance - old_deviance) * 100, 3) # Diff in deviance between current and previous lambda
      new_dev <- round(new_deviance * 100, 3) # Current deviance value
      vars_noSpaces <- paste(new_covars, collapse = " ") # Combine all variables together
      temp <- cbind(change, new_dev, dev_diff, new_varNum, new_lambda, vars_noSpaces, NA, NA) # Combine values together
      df <- rbind(df, temp) # Merge with template data frame
    }
    
    # See if covars added and removed at the same time (where number of variables stays the same)
    if (new_varNum == old_varNum & setequal(old_covars, new_covars) == FALSE) {
      change_add <- setdiff(new_covars, old_covars) # Find the covariate(s) added
      change_add <- paste(change_add, collapse = " ") # Combine variables together, if > 1
      change_add <- paste0(change_add, " (+)") # Append a "(+)" sign
      change_drop <- setdiff(old_covars, new_covars) # Find the covariate(s) removed
      change_drop <- paste(change_drop, collapse = " ") # Combine variables together, if > 1
      change_drop <- paste0(change_drop, " (-)") # Append a "(-)" sign
      change <- paste0(change_add, " ", change_drop) # Combine the added and dropped variables together
      dev_diff <- round((new_deviance - old_deviance) * 100, 3) # Diff in deviance between current and previous lambda
      new_dev <- round(new_deviance * 100, 3) # Current deviance value
      vars_noSpaces <- paste(new_covars, collapse = " ") # Combine all variables together
      temp <- cbind(change, new_dev, dev_diff, new_varNum, new_lambda, vars_noSpaces, NA, NA) # Combine values together
      df <- rbind(df, temp) # Merge with template data frame
    }
    
    # Rename the old covars, deviance and variable number
    old_covars <- new_covars
    old_deviance <- new_deviance
    old_varNum <- new_varNum
  }
  
  colnames(df) <- c("Variables", "DevRatio", "DevDiff", "VarNum", "Lambda", "model_vars", "aic", "bic")
  #df
  
  # Make a var to show number of steps where variables added, and rename the covars to blank (as are included by default)
  df$steps <- 1:nrow(df)
  df$Variables[df$steps == 1] <- ""
  return(df)
}


###########################################################################################
###### Read in the imputed data
load("./Data/data_imputed_B4563.RData")


### Take the relevant imputed dataset
dat <- complete(imp, 1)
#dat <- complete(imp, 2)
#dat <- complete(imp, 3)
#dat <- complete(imp, 4)
#dat <- complete(imp, 5)


### Encode the lifecourse hypotheses

## Critical periods - Select ACEs of interest

# Any ACEs
crit1 <- dat$clon145
crit2 <- dat$clon152
crit3 <- dat$clon159

# Bullying ACEs
#crit1 <- dat$clon144
#crit2 <- dat$clon147
#crit3 <- dat$clon154

# Domestic violence ACEs
#crit1 <- dat$clon142
#crit2 <- dat$clon150
#crit3 <- dat$clon158

# Sexual abuse ACEs
#crit1 <- dat$clon143
#crit2 <- dat$clon151
#crit3 <- dat$clon157

# Emotional abuse ACEs
#crit1 <- dat$clon141
#crit2 <- dat$clon149
#crit3 <- dat$clon155

# Physical abuse ACEs
#crit1 <- dat$clon140
#crit2 <- dat$clon148
#crit3 <- dat$clon156

## Accumulation
accum <- crit1 + crit2 + crit3

## Ever exposed
ACE_ever <- ifelse(crit1 == 1 | crit2 == 1 | crit3 == 1, 1, 0)

## Always exposed
ACE_always <- ifelse(crit1 == 1 & crit2 == 1 & crit3 == 1, 1, 0)


## Outcomes - Select age of interest
dep <- dat$dep17
anx <- dat$anx17

dep <- dat$dep24
anx <- dat$anx24


## Confounders
male <- as.numeric(dat$male) - 1
mat_age <- dat$mat_age
home <- as.numeric(dat$home) - 1
marital <- as.numeric(dat$marital) - 1
edu <- dat$edu
imd <- dat$imd
mat_dep <- dat$mat_dep
mat_anx <- dat$mat_anx
mat_aces <- dat$mat_aces


### Combine hypotheses and confounders into one matrix
x_hypos <- cbind(male, mat_age, home, marital, edu, imd, mat_dep, mat_anx, mat_aces,
                 crit1, crit2, crit3, accum, ACE_ever, ACE_always)
summary(x_hypos)


## Descriptive stats of ACEs
table(crit1); round(prop.table(table(crit1)) * 100, 1)
table(crit2); round(prop.table(table(crit2)) * 100, 1)
table(crit3); round(prop.table(table(crit3)) * 100, 1)
table(accum); round(prop.table(table(accum)) * 100, 1)
table(ACE_ever); round(prop.table(table(ACE_ever)) * 100, 1)
table(ACE_always); round(prop.table(table(ACE_always)) * 100, 1)


# Correlation plot
corrplot(cor(x_hypos[,10:15]),
         method = "color",
         type = "lower",
         addCoef.col = "black"
)

# Script to save image in .EPS format
#setEPS()
#postscript("./Results/corrMat.eps", height = 8, width = 8)
#corrplot(cor(x_hypos[,10:15]),
#         method = "color",
#         type = "lower",
#         addCoef.col = "black")
#dev.off()

cor(x_hypos[,10:15]) > 0.9


#### SLMCA

### Depression
slcma_mod <- glmnet(x_hypos, 
                   dep, 
                   alpha = 1,
                   family = "binomial",
                   penalty.factor = c(rep(0, 9), rep(1, 6)))

slcma_mod

# Look at the variables included at each step
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 9])); min(slcma_mod$dev.ratio[slcma_mod$df == 9])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 10])); min(slcma_mod$dev.ratio[slcma_mod$df == 10])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 11])); min(slcma_mod$dev.ratio[slcma_mod$df == 11])


## Visual inspection of model

# Summary table of each lasso step
df <- lasso_table(slcma_mod)
df

## Put lambda on the log scale, else differences between early models inflated, as lambda decreases on log scale. This does make the plot slightly more readable, and groups early-included variables closer together
slcma_mod$log_lambda <- log(slcma_mod$lambda)
slcma_mod

df$log_lambda <- log(as.numeric(df$Lambda))
df

plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
     xlab = "Log lambda value", ylab = "Deviance ratio", 
     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)


# Script to save image in .EPS format
#setEPS()
#postscript("./Results/dep17_anyACEs.eps", height = 6, width = 10)
#plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
#     xlab = "Log lambda value", ylab = "Deviance ratio", 
#     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
#text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)
#dev.off()


### Model results using AIC and BIC to select best-fitting model

# Loop over each step of the lasso, performing a standard logistic model and storing the AIC and BIC values
for (i in 1:nrow(df)) {
  
  vars_temp <- strsplit(df$model_vars[i], " ")[[1]] # Split the variables at each stage of the lasso
  x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Matrix of the covariates at each step of lasso
  mod_temp <- glm(dep ~ x_hypos_new, family = "binomial") # Run the model
  #print(summary(mod_temp))
  
  df$aic[i] <- AIC(mod_temp)
  df$bic[i] <- BIC(mod_temp)
  
}

# Check the dataframe, and select the models with the lowest AIC and BIC values
df

df$model_vars[which.min(df$aic)]
df$model_vars[which.min(df$bic)]


#### Selective model

### AIC
lambda <- as.numeric(df$Lambda[which.min(df$aic)])
#lambda
#lambda <- 0.008462711

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = dep, 
             penalty.factor = c(rep(0, 9), rep(1, 6)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(dep) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$aic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(dep ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))


### BIC
lambda <- as.numeric(df$Lambda[which.min(df$bic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = dep, 
             penalty.factor = c(rep(0, 9), rep(1, 6)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(dep) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$bic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(dep ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))



######################
### Anxiety
slcma_mod <- glmnet(x_hypos, 
                     anx, 
                     alpha = 1,
                     family = "binomial",
                     penalty.factor = c(rep(0, 9), rep(1, 6)))

slcma_mod

# Look at the variables included at each step
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 9])); min(slcma_mod$dev.ratio[slcma_mod$df == 9])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 10])); min(slcma_mod$dev.ratio[slcma_mod$df == 10])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 11])); min(slcma_mod$dev.ratio[slcma_mod$df == 11])


## Visual inspection of model

# Summary table of each lasso step
df <- lasso_table(slcma_mod)
df

## Put lambda on the log scale, else differences between early models inflated, as lambda decreases on log scale. This does make the plot slightly more readable, and groups early-included variables closer together
slcma_mod$log_lambda <- log(slcma_mod$lambda)
slcma_mod

df$log_lambda <- log(as.numeric(df$Lambda))
df

plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
     xlab = "Log lambda value", ylab = "Deviance ratio", 
     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)


# Script to save image in .EPS format
#setEPS()
#postscript("./Results/anx17_anyACEs.eps", height = 6, width = 10)
#plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
#     xlab = "Log lambda value", ylab = "Deviance ratio", 
#     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
#text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)
#dev.off()


### Model results using AIC and BIC to select best-fitting model

# Loop over each step of the lasso, performing a standard logistic model and storing the AIC and BIC values
for (i in 1:nrow(df)) {
  
  vars_temp <- strsplit(df$model_vars[i], " ")[[1]] # Split the variables at each stage of the lasso
  x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Matrix of the covariates at each step of lasso
  mod_temp <- glm(anx ~ x_hypos_new, family = "binomial") # Run the model
  #print(summary(mod_temp))
  
  df$aic[i] <- AIC(mod_temp)
  df$bic[i] <- BIC(mod_temp)
  
}

# Check the dataframe, and select the models with the lowest AIC and BIC values
df

df$model_vars[which.min(df$aic)]
df$model_vars[which.min(df$bic)]


#### Selective model

### AIC
lambda <- as.numeric(df$Lambda[which.min(df$aic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = anx, 
             penalty.factor = c(rep(0, 9), rep(1, 6)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(anx) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$aic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(anx ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))


### BIC
lambda <- as.numeric(df$Lambda[which.min(df$bic)])
#lambda
#lambda <- 0.006771721

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = anx, 
             penalty.factor = c(rep(0, 9), rep(1, 6)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(anx) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$bic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(anx ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))



########################################################################################################
#### Sensitivity analyses excluding 'ACE_ever' due to collinearity with accumulation

###### Read in the imputed data
load("./Data/data_imputed_B4563.RData")


### Take the relevant imputed dataset
dat <- complete(imp, 1)
#dat <- complete(imp, 2)
#dat <- complete(imp, 3)
#dat <- complete(imp, 4)
#dat <- complete(imp, 5)


### Encode the lifecourse hypotheses

## Critical periods - Select ACEs of interest

# Any ACEs
#crit1 <- dat$clon145
#crit2 <- dat$clon152
#crit3 <- dat$clon159

# Bullying ACEs
#crit1 <- dat$clon144
#crit2 <- dat$clon147
#crit3 <- dat$clon154

# Domestic violence ACEs
#crit1 <- dat$clon142
#crit2 <- dat$clon150
#crit3 <- dat$clon158

# Sexual abuse ACEs
#crit1 <- dat$clon143
#crit2 <- dat$clon151
#crit3 <- dat$clon157

# Emotional abuse ACEs
#crit1 <- dat$clon141
#crit2 <- dat$clon149
#crit3 <- dat$clon155

# Physical abuse ACEs
#crit1 <- dat$clon140
#crit2 <- dat$clon148
#crit3 <- dat$clon156

## Accumulation
accum <- crit1 + crit2 + crit3

## Always exposed
ACE_always <- ifelse(crit1 == 1 & crit2 == 1 & crit3 == 1, 1, 0)


## Outcomes - Select age of interest
dep <- dat$dep17
anx <- dat$anx17

dep <- dat$dep24
anx <- dat$anx24


## Confounders
male <- as.numeric(dat$male) - 1
mat_age <- dat$mat_age
home <- as.numeric(dat$home) - 1
marital <- as.numeric(dat$marital) - 1
edu <- dat$edu
imd <- dat$imd
mat_dep <- dat$mat_dep
mat_anx <- dat$mat_anx
mat_aces <- dat$mat_aces


### Combine hypotheses and confounders into one matrix
x_hypos <- cbind(male, mat_age, home, marital, edu, imd, mat_dep, mat_anx, mat_aces,
                 crit1, crit2, crit3, accum, ACE_always)
summary(x_hypos)


## Descriptive stats of ACEs
table(crit1); round(prop.table(table(crit1)) * 100, 1)
table(crit2); round(prop.table(table(crit2)) * 100, 1)
table(crit3); round(prop.table(table(crit3)) * 100, 1)
table(accum); round(prop.table(table(accum)) * 100, 1)
table(ACE_always); round(prop.table(table(ACE_always)) * 100, 1)


# Correlation plot
corrplot(cor(x_hypos[,10:14]),
         method = "color",
         type = "lower",
         addCoef.col = "black"
)

cor(x_hypos[,10:14]) > 0.9


#### SLMCA

### Depression
slcma_mod <- glmnet(x_hypos, 
                    dep, 
                    alpha = 1,
                    family = "binomial",
                    penalty.factor = c(rep(0, 9), rep(1, 5)))

slcma_mod

# Look at the variables included at each step
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 9])); min(slcma_mod$dev.ratio[slcma_mod$df == 9])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 10])); min(slcma_mod$dev.ratio[slcma_mod$df == 10])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 11])); min(slcma_mod$dev.ratio[slcma_mod$df == 11])


## Visual inspection of model

# Summary table of each lasso step
df <- lasso_table(slcma_mod)
df

## Put lambda on the log scale, else differences between early models inflated, as lambda decreases on log scale. This does make the plot slightly more readable, and groups early-included variables closer together
slcma_mod$log_lambda <- log(slcma_mod$lambda)
slcma_mod

df$log_lambda <- log(as.numeric(df$Lambda))
df

plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
     xlab = "Log lambda value", ylab = "Deviance ratio", 
     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)


### Model results using AIC and BIC to select best-fitting model

# Loop over each step of the lasso, performing a standard logistic model and storing the AIC and BIC values
for (i in 1:nrow(df)) {
  
  vars_temp <- strsplit(df$model_vars[i], " ")[[1]] # Split the variables at each stage of the lasso
  x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Matrix of the covariates at each step of lasso
  mod_temp <- glm(dep ~ x_hypos_new, family = "binomial") # Run the model
  #print(summary(mod_temp))
  
  df$aic[i] <- AIC(mod_temp)
  df$bic[i] <- BIC(mod_temp)
  
}

# Check the dataframe, and select the models with the lowest AIC and BIC values
df

df$model_vars[which.min(df$aic)]
df$model_vars[which.min(df$bic)]


#### Selective model

### AIC
lambda <- as.numeric(df$Lambda[which.min(df$aic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = dep, 
             penalty.factor = c(rep(0, 9), rep(1, 5)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(dep) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$aic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(dep ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))


### BIC
lambda <- as.numeric(df$Lambda[which.min(df$bic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = dep, 
             penalty.factor = c(rep(0, 9), rep(1, 5)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(dep) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$bic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(dep ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))



######################
### Anxiety
slcma_mod <- glmnet(x_hypos, 
                    anx, 
                    alpha = 1,
                    family = "binomial",
                    penalty.factor = c(rep(0, 9), rep(1, 5)))

slcma_mod

# Look at the variables included at each step
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 9])); min(slcma_mod$dev.ratio[slcma_mod$df == 9])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 10])); min(slcma_mod$dev.ratio[slcma_mod$df == 10])
coef(slcma_mod, s = max(slcma_mod$lambda[slcma_mod$df == 11])); min(slcma_mod$dev.ratio[slcma_mod$df == 11])


## Visual inspection of model

# Summary table of each lasso step
df <- lasso_table(slcma_mod)
df

## Put lambda on the log scale, else differences between early models inflated, as lambda decreases on log scale. This does make the plot slightly more readable, and groups early-included variables closer together
slcma_mod$log_lambda <- log(slcma_mod$lambda)
slcma_mod

df$log_lambda <- log(as.numeric(df$Lambda))
df

plot(slcma_mod$log_lambda, slcma_mod$dev.ratio, type = "l",
     xlab = "Log lambda value", ylab = "Deviance ratio", 
     xlim = rev(range(slcma_mod$log_lambda)), ylim = c(0.0, max(slcma_mod$dev.ratio)))
text(df$log_lambda, 0.0, labels = df$Variables, srt = 90, adj = 0)


### Model results using AIC and BIC to select best-fitting model

# Loop over each step of the lasso, performing a standard logistic model and storing the AIC and BIC values
for (i in 1:nrow(df)) {
  
  vars_temp <- strsplit(df$model_vars[i], " ")[[1]] # Split the variables at each stage of the lasso
  x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Matrix of the covariates at each step of lasso
  mod_temp <- glm(anx ~ x_hypos_new, family = "binomial") # Run the model
  #print(summary(mod_temp))
  
  df$aic[i] <- AIC(mod_temp)
  df$bic[i] <- BIC(mod_temp)
  
}

# Check the dataframe, and select the models with the lowest AIC and BIC values
df

df$model_vars[which.min(df$aic)]
df$model_vars[which.min(df$bic)]


#### Selective model

### AIC
lambda <- as.numeric(df$Lambda[which.min(df$aic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = anx, 
             penalty.factor = c(rep(0, 9), rep(1, 5)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(anx) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$aic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(anx ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))


### BIC
lambda <- as.numeric(df$Lambda[which.min(df$bic)])

# Extract the coefficients (note: have to *include* the intercept for logistic models).
beta <- coef(slcma_mod, s = lambda, x = x_hypos, y = anx, 
             penalty.factor = c(rep(0, 9), rep(1, 5)), exact = TRUE) 

# Perform selective inference
si_res <- fixedLassoInf(x_hypos, as.numeric(anx) - 1, beta, lambda, alpha = 0.05, family = "binomial")
si_res
si_res$vars

# Save results in nicer data frame
si_res <- as.data.frame(cbind(si_res$vars, si_res$coef0, si_res$sd, si_res$pv, si_res$ci))
colnames(si_res) <- c("Variable", "Beta", "SE", "Pvalue", "CI.lo", "CI.up")

si_res[,-1] <- lapply(si_res[,-1], function(x) round(as.numeric(as.character(x)), 4))
si_res
exp(si_res)


## Model without selective inference
vars_temp <- strsplit(df$model_vars[which.min(df$bic)], " ")[[1]] # Take variables from relevant model
x_hypos_new <- x_hypos[, colnames(x_hypos) %in% vars_temp] # Make matrix with just said variables

mod_noSI <- glm(anx ~ x_hypos_new, family = "binomial")
summary(mod_noSI)
exp(coef(summary(mod_noSI)))
exp(confint(mod_noSI))




