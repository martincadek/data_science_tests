# Virtual environment -----------------------------------------------------
# library(renv)
# renv::init()
# renv::snapshot()
# renv::restore()

# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(ranger)
library(skimr)
library(patchwork)
library(lme4)
library(merTools) # masks select;
library(viridis)
library(Cairo)

theme_set(theme_minimal() + theme(panel.grid.minor = element_blank()))

trace(grDevices::png, quote({
   if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
   }
}), print = FALSE)


# Load data ---------------------------------------------------------------
data <- read_csv("simulated_data.csv")

# Explore data ------------------------------------------------------------
glimpse(data) # Look at variables

skim(data) # Get further details about the variables

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



# Visualise raw data ------------------------------------------------------
CairoPNG(filename = "Raw_Data.png", width = 800, height = 800)
ggplot(data, aes(x = LOS, y = Death, colour = factor(Death), fill = Age)) +
     geom_jitter(width = 0.2, height = 0.2, size = 2, stroke = 2, alpha = 0.9, shape = 21) +
     scale_y_continuous(n.breaks = 2, breaks = c(0, 1)) +
     facet_grid(Organisation ~ Category) +
     scale_color_brewer(palette = "Set1") +
     scale_fill_viridis_c(option = "B", begin = 0.4) +
     labs(color = "Deaths\n[1 = died]",
          x = "Length of Stay (LOS) [Days]",
          title = "Deaths of patients across different NHS Trusts",
          subtitle = "Plots are further split by severity") 
dev.off()

# Analysis ----------------------------------------------------------------
# Plan: 
# Logistic/logit regression -> 
# Mixed Effects Logistic Regression -> 
# Random Forest


# A1 - Quick evaluation ---------------------------------------------------
# Look for variance and association across categories
tidy(aov(Death ~ LOS + Category + Age + Organisation, data = data))



# A2 - Simple white box model ---------------------------------------------
# Use simple model to understand the relationship further
model_glm <- glm(Death ~ LOS + Category + Age + Organisation, data = data, family = "binomial")

summary(model_glm)

bind_cols(tidy(model_glm),
      as_tibble(coef(model_glm)),
      as_tibble(confint(model_glm)))

glance(model_glm)

model_glm_predicted <- augment(model_glm, se_fit = TRUE) %>%
     mutate(Probabilities = plogis(.fitted),
            LCI = plogis(.fitted - (1.96 * .se.fit)),
            UCI = plogis(.fitted + (1.96 * .se.fit)))

(gg_model_a2 <- ggplot(data, aes(x = LOS, y = Death, colour = factor(Death))) +
     geom_jitter(width = 0.2, height = 0.2, size = 2, alpha = 0.9, shape = 21) +
     scale_y_continuous(n.breaks = 3, breaks = c(0, 1)) +
     facet_grid(Organisation ~ Category) +
     geom_ribbon(data = model_glm_predicted, 
                 aes(ymin = LCI,
                     ymax = UCI), alpha = 0.2, colour = "#B22222") +
     geom_line(data = model_glm_predicted,
                 aes(y = Probabilities), colour = "black") +
     scale_color_brewer(palette = "Set1") +
     labs(color = "Deaths\n[1 = died]",
          x = "Length of Stay (LOS) [Days]",
          subtitle = "Model: glm (Death ~ LOS + Category + Age + Organisation)"))


# A3 - Advanced white box model -------------------------------------------
model_glmer_base <- glmer(Death ~ Age + LOS + Category + (1|Organisation), data = data, family = "binomial")
model_glmer_LOSxCAT <- glmer(Death ~ Age + LOS : Category + (1|Organisation), data = data, family = "binomial")
model_glmer_LOSxCATxAGE <- glmer(Death ~ Age : LOS : Category + (1|Organisation), data = data, family = "binomial")


anova(model_glmer_base, model_glmer_LOSxCAT, model_glmer_LOSxCATxAGE)

summary(model_glmer_base)
coef(model_glmer_base)

summary(model_glmer_LOSxCAT)
coef(model_glmer_LOSxCAT)

model_glmer_LOSxCAT_predicted <- bind_cols(data, predictInterval(merMod = model_glmer_LOSxCAT, newdata = data,
                           level = 0.95, n.sims = 1000,
                           stat = "mean", type = "probability",
                           include.resid.var = TRUE))

(gg_model_a3 <- ggplot(data, aes(x = LOS, y = Death, colour = factor(Death))) +
     geom_jitter(width = 0.2, height = 0.2, size = 2, alpha = 0.9, shape = 21) +
     scale_y_continuous(n.breaks = 3, breaks = c(0, 1)) +
     facet_grid(Organisation ~ Category) +
     geom_ribbon(data = model_glmer_LOSxCAT_predicted, 
                 aes(ymin = lwr,
                     ymax = upr), alpha = 0.2, colour = "#B22222") +
     geom_line(data = model_glmer_LOSxCAT_predicted,
               aes(y = fit), colour = "black") +
     scale_color_brewer(palette = "Set1") +
     labs(color = "Deaths\n[1 = died]",
          x = "Length of Stay (LOS) [Days]",
          subtitle = "Model: glmer (Death ~ Age + LOS : Category + (1|Organisation)"))



# A4 - Black box model no-tuning ------------------------------------------
data_factorised <- data %>% mutate(Death = as.factor(Death))

set.seed(2020)
data_split <- initial_split(data_factorised, p = 0.75)
data_train <- training(data_split)
data_test  <- testing(data_split)

rf_setup <- rand_forest(mode = "classification", mtry = 2)

model_rf_train <- 
     rf_setup %>%
     set_engine("ranger") %>%
     fit(Death ~ Age + LOS + Category + Organisation,
         data = data_train)

model_rf_train

model_rf_test <- 
     data_test %>%
     dplyr::select(Death) %>%
     bind_cols(
          predict(model_rf_train, 
                  new_data = data_test[, c("Age", "LOS", "Category", "Organisation")])
     )

model_rf_test %>% slice(1:5)

model_rf_test %>% metrics(truth = Death, estimate = .pred_class) 

(gg_model_a4 <- model_rf_test %>% 
     ggplot(aes(x = .pred_class, y = Death, colour = factor(Death))) + 
          geom_abline(col = "#B22222", lty = 2) + 
          geom_jitter(alpha = 0.7, width = 0.2, height = 0.2) + 
          scale_color_brewer(palette = "Set1") +
          labs(color = "Deaths\n[1 = died]",
          x = "Predicted death",
          y = "Actual death",
          subtitle = "Model: rf (Death ~ Age + LOS + Category + Organisation)"))

# Save plot ---------------------------------------------------------------
CairoPNG(filename = "Models.png", width = 1600, height = 800)
gg_model_a2 + gg_model_a3 + gg_model_a4
dev.off()
     