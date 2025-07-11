if (!requireNamespace("renv", quietly = TRUE)) {
     install.packages("renv")
}

if (!requireNamespace("pacman", quietly = TRUE)) {
     install.packages("pacman")
}

if (!requireNamespace("cli", quietly = TRUE)) {
     install.packages("cli")
}

cli::cli_alert_danger("Some packages may ask for the permission to install from source. Please select 'No'.")
cli::cli_alert_info("Loading/installing packages...")

# initiate or restore virtual environment with renv
# renv::init()
# renv::snapshot()
# renv::restore()

# Basic packages for cleaning, version control, and modelling
pacman::p_load(renv, data.table, cli, lubridate, rmarkdown, bigrquery, httpuv, magrittr,
               jsonlite, googleCloudStorageR, checkmate, ggplot2, tidymodels)
# Modelling packages
pacman::p_load(tidymodels, glmnet, ranger, xgboost)

cli::cli_alert_success("setup.R finished")


# create dirs -------------------------------------------------------------
if (!dir.exists("data/")) {
     dir.create("data/")
}

if (!dir.exists("output/")) {
     dir.create("output/")
}
