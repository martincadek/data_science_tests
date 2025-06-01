source("R/set_project.R", echo = FALSE)
source("R/set_functions.R", echo = FALSE)
source("R/make_synthetic_data.R", echo = FALSE)
source("R/make_model.R", echo = FALSE)
rmarkdown::render(input = "R/Report_Measuring_experience_with_the_NCMP_letters.Rmd",
                  output_dir = "output/")
