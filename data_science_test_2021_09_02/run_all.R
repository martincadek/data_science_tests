source("R/setup.R")
source("R/query.R")
source("R/clean.R")
source("R/model.R")
rmarkdown::render(input = "R/report.Rmd")