##Loading packages
library(haven)
library(tidyverse)
library(dplyr)
library(mice)
library(ggplot2)
library(ggmice)
library(naniar)
library(miceadds)
library(lavaan)
library(tinytex)
library(tidyverse)
library(psych)
library(car)
library(aod)
library(gridExtra)
library(rcompanion)

#Setting variables as factor or numeric
youthfinalvar$response <- as.factor(youthfinalvar$response)
youthfinalvar$sex_1 <- as.factor(youthfinalvar$sex_1)
youthfinalvar$important_talks_with_mother <- as.factor(youthfinalvar$important_talks_with_mother)
youthfinalvar$quarrel_with_mother_1 <- as.factor(youthfinalvar$quarrel_with_mother_1)
youthfinalvar$SDQconduct_problems <- as.numeric(youthfinalvar$SDQconduct_problems)
youthfinalvar$SDQhyperactivity <- as.numeric(youthfinalvar$SDQhyperactivity)
youthfinalvar$SDQpeer_problems <- as.numeric(youthfinalvar$SDQpeer_problems)
youthfinalvar$SDQprosocial <- as.numeric(youthfinalvar$SDQprosocial)
youthfinalvar$SDQemotional_symptoms <- as.numeric(youthfinalvar$SDQemotional_symptoms)
youthfinalvar$household_size <- as.numeric(youthfinalvar$household_size)
youthfinalvar$mean_authoritative <- as.numeric(youthfinalvar$mean_authoritative)
youthfinalvar$mean_authorarian <- as.numeric(youthfinalvar$mean_authorarian)
youthfinalvar$mean_permissive <- as.numeric(youthfinalvar$mean_permissive)
youthfinalvar$feeling_supported_by_family_1 <- as.factor(youthfinalvar$feeling_supported_by_family_1)

#LogReg parentchild (model 1)
model1 <- glm(response ~ sex_1 + household_size + feeling_supported_by_family_1 + quarrel_with_mother_1 + important_talks_with_mother, data = youthfinalvar, family = "binomial")
summary(model1)
nagelkerke(model1)
vif(model1)
exp(coef(model1))
ses1 <- sqrt(diag(vcov(model1)))
get.or.ses1 <- function(model1) {
  broom::tidy(model1) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model1)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}
get.or.ses1(model1)

#coefficients model1 (OR)
coef1 <- summary(model1)$coefficients
B1 <- coef1[, 1]
OR1 <- exp(coef1[, 1])
SE1 <- coef1[, 2]
Wald1 <- coef1[, 3]
pvalue1 <- coef1[, 4]
ci_lower1 <- exp(coef1[, 1] - 1.96 * coef1[, 2])
ci_upper1 <- exp(coef1[, 1] + 1.96 * coef1[, 2])
coefresult1 <- data.frame(B1 = B1, OR1 = OR1, SE1 = SE1, Wald1 = Wald1, pvalue1 = pvalue1, ci_lower1 = ci_lower1, ci_upper1 = ci_upper1)
view(coefresult1)

#LogReg SDQ (model2)
model2 <- glm(response ~ sex_1 + misbehaviour_school_1 + SDQhyperactivity + SDQemotional_symptoms + SDQpeer_problems + SDQconduct_problems + SDQprosocial, data = youthfinalvar, family = "binomial")
summary(model2)
nagelkerke(model2)
vif(model2)
summary(model2)
exp(coef(model2))
ses2 <- sqrt(diag(vcov(model2)))
get.or.ses2 <- function(model2) {
  broom::tidy(model2) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model2)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}
get.or.ses2(model2)
print(exp(coef(model2)))

#estimates
coef2 <- summary(model2)$coefficients
B2 <- coef2[, 1]
OR2 <- exp(coef2[, 1])
SE2 <- coef2[, 2]
Wald2 <- coef2[, 3]
pvalue2 <- coef2[, 4]
ci_lower2 <- exp(coef2[, 1] - 1.96 * coef2[, 2])
ci_upper2 <- exp(coef2[, 1] + 1.96 * coef2[, 2])
coefresult2 <- data.frame(B2 = B2, OR2 = OR2, SE2 = SE2, Wald2 = Wald2, pvalue2 = pvalue2, ci_lower2 = ci_lower2, ci_upper2 = ci_upper2)
view(coefresult2)

#LogReg all (model 3)
model3 <- glm(response ~ sex_1 + household_size + feeling_supported_by_family_1 + quarrel_with_mother_1 + important_talks_with_mother + misbehaviour_school_1 + SDQconduct_problems + SDQemotional_symptoms + SDQhyperactivity  + SDQpeer_problems + SDQprosocial, data = youthfinalvar, family = "binomial")
summary(model3)
nagelkerke(model3)
vif(model3)
ses3 <- sqrt(diag(vcov(model3)))
get.or.ses3 <- function(model3) {
  broom::tidy(model3) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model3)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}
get.or.ses3(model3)
print(exp(coef(model3)))

#creating dataset for imputation (for model 4 and 5)
youthfinalvarmice <- youthfinalvar[ ,c("sex_1", "feeling_supported_by_family_1", "quarrel_with_mother_1", "important_talks_with_mother", "misbehaviour_school_1", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial","mean_authoritative", "mean_authorarian", "mean_permissive", "household_size", "response")]
youthfinalvarmice$mean_authoritative <- as.numeric(youthfinalvarmice$mean_authoritative)
youthfinalvarmice$mean_authorarian <- as.numeric(youthfinalvarmice$mean_authorarian)
youthfinalvarmice$mean_permissive <- as.numeric(youthfinalvarmice$mean_permissive)
youthfinalvarmice$response <- as.factor(youthfinalvarmice$response)
youthfinalvarmice$sex_1 <- as.factor(youthfinalvarmice$sex_1)
youthfinalvarmice$important_talks_with_mother <- as.factor(youthfinalvarmice$important_talks_with_mother)
youthfinalvarmice$quarrel_with_mother_1 <- as.factor(youthfinalvarmice$quarrel_with_mother_1)
youthfinalvarmice$SDQconduct_problems <- as.numeric(youthfinalvarmice$SDQconduct_problems)
youthfinalvarmice$SDQhyperactivity <- as.numeric(youthfinalvarmice$SDQhyperactivity)
youthfinalvarmice$SDQpeer_problems <- as.numeric(youthfinalvarmice$SDQpeer_problems)
youthfinalvarmice$SDQprosocial <- as.numeric(youthfinalvarmice$SDQprosocial)
youthfinalvarmice$SDQemotional_symptoms <- as.numeric(youthfinalvarmice$SDQemotional_symptoms)
youthfinalvarmice$household_size <- as.numeric(youthfinalvarmice$household_size)
youthfinalvarmice$feeling_supported_by_family_1 <- factor(youthfinalvarmice$feeling_supported_by_family_1)

summary(youthfinalvarmice)#making sure the missing is only in parenting style

#imputing data
impdata <- mice(youthfinalvarmice, method = "pmm", m =18, maxit = 18, seed = 22315, print = FALSE)

#retrieving mean and sd for imputed data (permissive)
dataimp <- complete(impdata,action="long",include = FALSE)
pool_mean <- with(dataimp, by(dataimp, .imp, function(x) c(mean(x$mean_permissive),sd(x$mean_permissive))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean)

#retrieving mean and sd for imputed data (authoritarian)
pool_mean1 <- with(dataimp, by(dataimp, .imp, function(x) c(mean(x$mean_authorarian),sd(x$mean_authorarian))))
pool_mean1
Reduce("+",pool_mean1)/length(pool_mean1)

#retrieving mean and sd for imputed data (authoritative)
pool_mean2 <- with(dataimp, by(dataimp, .imp, function(x) c(mean(x$mean_authoritative),sd(x$mean_authoritative))))
pool_mean2
Reduce("+",pool_mean2)/length(pool_mean2)

impcomplete <- complete(impdata,"long")
library(stargazer)
stargazer(impcomplete)

#checking convergence and model diagnostics
plot_trace(impdata) + 
  geom_smooth(color = "black", linetype = "dashed", se = FALSE)

#diagnostics + checking for convergence
densityplot(impdata)

mice::stripplot(impdata, mean_permissive)
mice::stripplot(impdata, mean_authorarian)
mice::stripplot(impdata, mean_authoritative)

#pooled results for the full analysis (model 5)
fit1 <- with(impdata, glm(response ~ sex_1 + household_size + feeling_supported_by_family_1 + quarrel_with_mother_1 + important_talks_with_mother + misbehaviour_school_1 + SDQconduct_problems + SDQemotional_symptoms + SDQhyperactivity  + SDQpeer_problems  + SDQprosocial + mean_authorarian + mean_authoritative + mean_permissive, family = "binomial"))
pool1 <- pool(fit1)
summary(pool(fit1), conf.int = TRUE, exponentiate = TRUE)

get.or.ses <- function(model) {
  tidy_model <- broom::tidy(model)
  tidy_model <- tidy_model %>%
    mutate(or = exp(estimate),  
           or.se = sqrt(exp(estimate)^2 * std.error^2)) 
  return(tidy_model %>% select(term, or, or.se)) 
}

#retrieving OR on the pooled model
or_results <- get.or.ses(pool1)
print(or_results)

#checking models
models <- fit1$analyses

#computing nagelkerke R-square, chi-square and p-values
compute_model_statistics <- function(models) {
  nagelkerke_r2 <- numeric(length(models))
  chi2_values <- numeric(length(models))
  p_values <- numeric(length(models))
  
for (i in seq_along(models)) {
    model <- models[[i]]
    
#Nagelkerke 
nk <- nagelkerke(model)
nagelkerke_r2[i] <- tryCatch({
nk$Pseudo.R.squared.for.model.vs.null["Nagelkerke", "Pseudo.R.squared"]
}, error = function(e) NA)
    
#chi-square and p-value
null_model <- update(model, . ~ 1)
logLik_full <- logLik(model)
logLik_null <- logLik(null_model)
chi2_values[i] <- as.numeric(-2 * (logLik_null - logLik_full))
df1 <- attr(logLik_full, "df") - attr(logLik_null, "df")
p_values[i] <- pchisq(chi2_values[i], df = df1, lower.tail = FALSE)
  }
list(nagelkerke_r2 = nagelkerke_r2, chi2_values = chi2_values, p_values = p_values)
}

#compute statistics
model_statistics1 <- compute_model_statistics(models)

#calculate the mean values
mean_nagelkerke_r2 <- mean(model_statistics1$nagelkerke_r2, na.rm = TRUE)
mean_chi2 <- mean(model_statistics1$chi2_values, na.rm = TRUE)
mean_p <- mean(model_statistics1$p_values, na.rm = TRUE)

view(mean_nagelkerke_r2)
view(mean_chi2)
view(mean_p)

#computing VIF
vifs <- lapply(models, function(model) {
  tryCatch({
    vif(model)
  }, error = function(e) NULL)
})

#average VIF
vifs <- Filter(Negate(is.null), vif_list)
vif_df <- do.call(cbind, lapply(vif_list, function(x) {
  if (is.matrix(x)) {
    x[, "GVIF^(1/(2*Df))"]
  } else {
    x
  }
}))
mean_vif <- rowMeans(vif_df, na.rm = TRUE)

#pooled results for parentchild analysis (model 4)
fit2 <- with(impdata, glm(response ~ sex_1 + household_size + feeling_supported_by_family_1 + quarrel_with_mother_1 + important_talks_with_mother + mean_authorarian + mean_permissive + mean_authoritative, family = "binomial"))
pool2 <- pool(fit2)
summary(pool(fit2), conf.int = TRUE, exponentiate = TRUE)

get.or.ses <- function(model) {
  tidy_model <- broom::tidy(model)
  tidy_model <- tidy_model %>%
    mutate(or = exp(estimate), 
           or.se = sqrt(exp(estimate)^2 * std.error^2))
  return(tidy_model %>% select(term, or, or.se))
}

#retrieving OR on the pooled model5
or_results1 <- get.or.ses(pool2)
print(or_results1)

#checking models2
models2 <- fit2$analyses

#computing nagelkerke R-square, chi-square and p-values
compute_model_statistics <- function(models2) {
  nagelkerke_r2 <- numeric(length(models2))
  chi2_values <- numeric(length(models2))
  p_values <- numeric(length(models2))
  
for (i in seq_along(models2)) {
model <- models2[[i]]
    
#nagelkerke
nk <- nagelkerke(model)
nagelkerke_r2[i] <- tryCatch({
nk$Pseudo.R.squared.for.model.vs.null["Nagelkerke", "Pseudo.R.squared"]
}, error = function(e) NA)
    
#chi-square and p-value
null_model <- update(model, . ~ 1)
logLik_full <- logLik(model)
logLik_null <- logLik(null_model)
chi2_values[i] <- as.numeric(-2 * (logLik_null - logLik_full))
df1 <- attr(logLik_full, "df") - attr(logLik_null, "df")
p_values[i] <- pchisq(chi2_values[i], df = df1, lower.tail = FALSE)
}
  
list(nagelkerke_r2 = nagelkerke_r2, chi2_values = chi2_values, p_values = p_values)
}

#computing statistics
model_statistics <- compute_model_statistics(models2)

#calculating mean values
mean2_nagelkerke_r2 <- mean(model_statistics$nagelkerke_r2, na.rm = TRUE)
mean2_chi2 <- mean(model_statistics$chi2_values, na.rm = TRUE)
mean2_p <- mean(model_statistics$p_values, na.rm = TRUE)

view(mean2_nagelkerke_r2)
view(mean2_chi2)
view(mean2_p)

#computing VIF
vif_list <- lapply(models2, function(model) {
  tryCatch({
    vif(model)
  }, error = function(e) NULL)
})

#average VIF
vif_list <- Filter(Negate(is.null), vif_list)
vif_df <- do.call(cbind, lapply(vif_list, function(x) {
  if (is.matrix(x)) {
    x[, "GVIF^(1/(2*Df))"]
  } else {
    x
  }
}))
mean_vif <- rowMeans(vif_df, na.rm = TRUE)