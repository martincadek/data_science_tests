# Virtual environment -----------------------------------------------------
# renv::init()
# renv::snapshot()
# renv::restore()

# libraries ---------------------------------------------------------------
library(survival)
library(tidyverse)
library(survminer)
library(patchwork)
# library(gtsummary) # not to load

# data --------------------------------------------------------------------
data <- read_csv("simulated_data.csv")

# ID - a unique row ID.
# Organisation - The organisation the patient was seen at.
# Age - The patient’s age.
# LOS - The patent’s length-of-stay in hospital, in whole days.
# Death - A flag indicating whether the patient died, coded: 0 = survived, 1 = died.
# Category - The risk category the patient falls into.


# explore data ------------------------------------------------------------
# Quickly visualise key variables
qplot(data = data, x = Age, bins = 10) + qplot(data = data, x = LOS, bins = 18) +
     qplot(data = data, x = Death, bins = 3) + qplot(data = data, x = Category)

# Look a little closer at the age
(ggplot(data = data, aes(sample = Age, colour = Category)) +
          geom_qq() +
          stat_qq_line() + labs(subtitle = "Age")) +
     (ggplot(data = data, aes(sample = Age, colour = Organisation)) +
           geom_qq() +
           stat_qq_line() + labs(subtitle = "Age")) +
     (ggplot(data = data, aes(sample = LOS, colour = Category)) +
           geom_qq() +
           stat_qq_line() + labs(subtitle = "LOS")) + 
     (ggplot(data = data, aes(sample = LOS, colour = Organisation)) +
           geom_qq() +
           stat_qq_line() + labs(subtitle = "LOS"))


# Coerce ------------------------------------------------------------------
# Coerce Category and Organisation to Factors
data <- data %>% mutate(Category = factor(Category, levels = c("Low", "Moderate", "High")),
                        Organisation = factor(Organisation, levels = c("Trust1", "Trust2",
                                                                       "Trust3", "Trust4",
                                                                       "Trust5", "Trust6",
                                                                       "Trust7", "Trust8",
                                                                       "Trust9", "Trust10")))

data$LOS


# create survival object --------------------------------------------------
surv_data <- Surv(data$LOS, data$Death==1)
plot(surv_data)



# Estimation using the Kaplan-Meier ---------------------------------------
surv_intercept <- survfit(surv_data ~ 1, conf.type="log-log")
ggsurvplot(
     fit = surv_intercept, data = data, 
     xlab = "Days", 
     ylab = "Length of Stay [LOS]")

surv_cat <- survfit(surv_data ~ Category, data = data, conf.type = "log-log")
ggsurvplot(
     fit = surv_cat, data = data, 
     xlab = "Days", 
     ylab = "Length of Stay [LOS]")

surv_org <- survfit(surv_data ~ Organisation, data = data, conf.type = "log-log")
ggsurvplot(
     fit = surv_org, data = data, 
     xlab = "Days", 
     ylab = "Length of Stay [LOS]")


# cox-proportional hazard -------------------------------------------------
cox_simple <- coxph(surv_data ~ Category, data=data)
bind_cols(broom::tidy(cox_simple, exp = TRUE), as_tibble(confint(cox_simple))) 
# Exponentiate the coefficients to obtain odds ratio / hazard ratio

cox_age_cat <- coxph(surv_data ~  Age + Category, data=data)
bind_cols(broom::tidy(cox_age_cat, exp = TRUE), as_tibble(confint(cox_age_cat))) 

cox_model_all <- coxph(surv_data ~  Age + Category + Organisation, data=data)
bind_cols(broom::tidy(cox_model_all, exp = TRUE), as_tibble(confint(cox_model_all))) 

AIC(cox_simple); AIC(cox_age_cat); AIC(cox_model_all)

