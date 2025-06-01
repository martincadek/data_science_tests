# Setup -------------------------------------------------------------------
if (!checkmate::test_file_exists("input/synthetic_data.rds")) {
     # Data and checks ---------------------------------------------------------
     # This file is ignored in github.
     message("You will be asked to interactively locate the real data.")
     real_data <- read_rds(file.choose()) %>%
          select(-c(StartDate, ResponseId, Parent_Mail, Parent_Phone, Districts,
                    Parent_Postcode_CAT, Child_School_CAT, MDY_NCMP))
     
     set_rows <- 139
     set_cols <- 63
     set_mean_attractivness <- 0.411271
     
     # Synthesise --------------------------------------------------------------
     # Synthesise a data set using default methods of syn()
     my_seed <- 2020
     synthetic_object <- syn(real_data, seed = my_seed)
     synthetic_data <- as_tibble(synthetic_object$syn)
     
     # Run checks --------------------------------------------------------------
     stopifnot(nrow(synthetic_data) == set_rows)
     stopifnot(ncol(synthetic_data) == set_cols)
     mean(synthetic_data$Attractiveness) - set_mean_attractivness # won't be exactly the same
     
     # Save - OFF --------------------------------------------------------------
     saveRDS(synthetic_data, here("input/synthetic_data.rds"))
} else {
     message("Synthetic data were already generated:")
     synthetic_data <- read_rds(path = "input/synthetic_data.rds")
     str(synthetic_data)
     }


