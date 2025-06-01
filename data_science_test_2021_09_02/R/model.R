# load data ---------------------------------------------------------------
# load cleaned data
(data_model <- fread("data/"))
setDF(data_model)

cli_alert_info("Starting the model.R script, please wait...")

# split -------------------------------------------------------------------
# set seed to make the analysis reproducible
set.seed(062021)
data_split <- initial_split(data_model, strata = "price", prop = 0.75)
data_train <- training(data_split)
data_test  <- testing(data_split)