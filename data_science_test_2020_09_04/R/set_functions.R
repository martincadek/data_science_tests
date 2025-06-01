# GET GEO DATA ------------------------------------------------------------
# Interesting but trivial error occured. The function I wrote using postcode_lookup doesn't
# work well if there are duplicit postcodes. What happens is that the duplicit postcode is not
# included in the for loop, i.e.: if the postcode occurs a second time, it's skipped.
# This was resulting in a vector of different size than my input dataframe, as indicated by mutate.
# To simmulate the error. Keep duplicate postcodes in...
# function.
# postcode_lookup("IP27 0NJ")
# postcode_lookup("IP14 1QF")

# The function does not work on University DNS
# https://github.com/jeroen/curl/issues/72
# Change DNS

# Get longitude
get_longitude <- function(data, postcode) { # requires data and column name in quotes
     n <- list()
     for (i in data[[postcode]]) {
          if (httr::status_code(httr::GET(paste0("https://api.postcodes.io/postcodes/", i))) == 400) {
               n[i] <- NA
          }
          else {
               n[i] <- postcode_lookup(i)$longitude
          }
     }
     return(unname(unlist(n)))
}

# get_longitude(OneLifeServices, postcode = "Postcode")

# Get latitude
get_latitude <- function(data, postcode) { # reuires data and column name in quotes
     n <- list()
     for (i in data[[postcode]]) {
          if (httr::status_code(httr::GET(paste0("https://api.postcodes.io/postcodes/", i))) == 404) {
               n[i] <- NA
          }
          else {
               n[i] <- postcode_lookup(i)$latitude
          }
     }
     return(unname(unlist(n)))
}

# get_latitude(OneLifeServices, postcode = "Postcode")
get_admin_district_code <- function(data, postcode) { # reuires data and column name in quotes
     n <- list()
     for (i in data[[postcode]]) {
          if (httr::status_code(httr::GET(paste0("https://api.postcodes.io/postcodes/", i))) == 404) {
               n[i] <- NA
          }
          else {
               n[i] <- postcode_lookup(i)$admin_district_code
          }
     }
     return(unname(unlist(n)))
}
# get_admin_district_code(OneLifeServices, postcode = "Postcode")


get_lsoa <- function(data, postcode) { # reuires data and column name in quotes
     n <- list()
     for (i in data[[postcode]]) {
          if (httr::status_code(httr::GET(paste0("https://api.postcodes.io/postcodes/", i))) == 404) {
               n[i] <- NA
          }
          else {
               n[i] <- postcode_lookup(i)$lsoa
          }
     }
     return(unname(unlist(n)))
}

# get_lsoa(OneLifeServices, postcode = "Postcode")


# NEAREST_OUTCODE ---------------------------------------------------------
# This is implementation to compute nearest admin district, e.g. Leeds, from Lon, Lats
# https://stackoverflow.com/questions/37117472/loop-for-reverse-geocoding-in-r

# Uncode as needed, anonymous function

# lon_lat_admin_districts <- mapply(function(LocationLongitude, LocationLatitude) { 
#         nearest_outcode_lonlat(longitude = LocationLongitude, latitude = LocationLatitude)[[1]]$admin_district[[1]]
# }, 
# all_data$LocationLongitude, all_data$LocationLatitude
# )

replace_list_NULL <- function(x, replacement = "Missing") {
        if (length(x) == 0 || length(x[[1]]) == 0) {
                replacement
        } else {
                x
        }
}

# FIND ME DUPLICATES ------------------------------------------------------

# Find duplicates
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
find_me_duplicates <- function(data, col_name) {
     require(broom)
     require(tidyverse)
     col_name <- enquo(col_name)
     
     data %>%
          select(!!col_name) %>%
          table() %>%
          tidy() %>%
          arrange(desc(n)) %>%
          filter(n >= 2) %>%
          rename(frequency = "n",
                 column = ".")
}




# REVERSE SCALE -----------------------------------------------------------

reverse_simple <- function(x) {
        ifelse(x != 0, x*-1, x)
        # if (x != 0) {
        #         x <- x * (-1)        
        #         return(x)
        # } else {
        #         return(x)
        # }
}

reverse_advanced <- function(x) {
        ifelse(x == 7, 3,
               ifelse(x == 6, 2,
                      ifelse(x == 5, 1,
                             ifelse(x == 4, 0,
                                    ifelse(x == 3, -1,
                                           ifelse(x == 2, -2,
                                                  ifelse(x == 1, -3, NA)
                                           )
                                    )
                             )
                      )
               )
        )
}

# INSERT WHITE SPACE ------------------------------------------------------
# Replace _ from variables with " " to plot the data, or change the default setting.
# https://dplyr.tidyverse.org/articles/programming.html
# https://stackoverflow.com/questions/57004055/programming-with-tidyeval-the-mutate-function-after-tidyrunitecol-col
insert_whitespace <- function(data, which_variable, which_pattern = "_", which_replacement = " ") {
        which_variable <- enquo(which_variable) # Ensure quotation
        which_variable_name <- quo_name(which_variable) # Careful with mutate, needs to be set up slightly differently
        mutate(.data = data, !! which_variable_name := str_replace_all( # Notice :=, function would throw error otherwise
                string = !! which_variable, # Column with strings, no default
                pattern = which_pattern, # What patern, defualt "_" 
                replacement = which_replacement)) # What replacement, default " "
}



# REMOVE PATTERN ----------------------------------------------------------
# Remove 01_ and _01 from data to plot them. Or change the pattern
remove_pattern <- function(data, which_variable, which_pattern = "0[1-9]_|1[0-2]_|_0[1-9]|_0[1-9]") {
        which_variable <- enquo(which_variable) # Ensure quotation
        which_variable_name <- quo_name(which_variable) # Careful with mutate, needs to be set up slightly differently
        mutate(.data = data, !! which_variable_name := str_remove_all( # Notice :=, function would throw error otherwise
                string = !! which_variable, # Column with strings, no default
                pattern = which_pattern # What patern, defualt "_[:digit:][:digit:]" 
        )) # What replacement, default " "
}

# GET_CONSENT -------------------------------------------------------------
get_consent_number <- function(data) {
        data %>%
                filter_at(vars(starts_with("Consent_")), any_vars(!is.na(.))) %>%
                mutate_at(vars(starts_with("Consent_")), list(~ str_detect(., "Yes"))) %>%
                summarise_at(vars(starts_with("Consent_")), list(~ sum(.)))
}


# GENERETA FREQ DIST TBL -----------------------------------
generate_freq_dist_table <- function(data, variable, variable_rename = "X") {
        var <- enquo(variable) # Variable to generate the distribution from
        variable_rename <- enquo(variable_rename)  # Rename variable
        data %>%
                mutate(
                        !! var := replace_na(!! var, "Missing")
                        ) %>% # label all NAs as missing, turn to factor
                count(!! var, 
                      sort = TRUE, # Arrange into descentind
                      name = "Frequency") %>%
                # arrange(!! var) %>% # Arrange into descending
                mutate(
                        "Cumulative frequency" = cumsum(Frequency), # Cumulative freq
                        "Relative frequency" = round(prop.table(Frequency), 2), # Rel freq
                        "Relative percentage" = paste(round((prop.table(Frequency)) * 100), "%"), # Relative Perc
                        "Cumulative percentage" = paste(round((cumsum(Frequency)/sum(Frequency)) * 100), "%") # Cum perc
                )  %>%
                rename(!!variable_rename := !! var) # Rename variable
}

generate_freq_dist_table_groups <- function(data, variable, variable_rename = "X", ...) {
        vars_group <- enquos(...)
        var <- enquo(variable) # Variable to generate the distribution from
        variable_rename <- enquo(variable_rename)  # Rename variable
        message("Only frequency is computed by groups!")
        data %>%
                mutate(
                        !! var := replace_na(!! var, "Missing")
                ) %>% # label all NAs as missing, turn to factor
                group_by(!!! vars_group) %>%
                count(!! var, 
                      sort = TRUE, 
                      name = "Frequency") %>%
                ungroup() %>%
                arrange(!! var) %>% # Arrange into descending after ungroup()
                mutate(
                        "Cumulative frequency" = cumsum(Frequency), # Cumulative freq
                        "Relative frequency" = round(prop.table(Frequency), 2), # Rel freq
                        "Relative percentage" = paste(round((prop.table(Frequency)) * 100), "%"), # Relative Perc
                        "Cumulative percentage" = paste(round((cumsum(Frequency)/sum(Frequency)) * 100), "%") # Cum perc
                )  %>%
                rename(!!variable_rename := !! var) # Rename variable
}

# UEQ_SUMMARY_TABLE -------------------------------------------------------
# Can be applied after the UEQ is in long format, it's just to shorten the space...
summarise_ueq <- function(data, to_summarise, ...) {
        group_by_what <- enquos(...) # This can accept multiple arguments
        expr <- enquo(to_summarise)
        # This groups, sumamrise various psychometric statistics, then ungroups
        data %>%
        group_by(!!! group_by_what) %>%
          summarise(
                  UEQ_Count = n(),
                  UEQ_Min = min(!! expr),
                  UEQ_Max = max(!! expr),
                  UEQ_Sum = round(sum(!! expr), 2),
                  UEQ_IQR = round(IQR(!! expr), 2),
                  UEQ_Mean = round(mean(!! expr), 2),
                  UEQ_Median = round(median(!! expr), 2),
                  UEQ_SD = round(sd(!! expr), 2),
                  UEQ_Variance = round(var(!! expr), 2),
                  UEQ_SE = round(UEQ_SD / sqrt(UEQ_Count), 2),
                  UEQ_Lower_CI = round(lower_ci(UEQ_Mean, UEQ_SE, UEQ_Count), 2),
                  UEQ_Upper_CI = round(upper_ci(UEQ_Mean, UEQ_SE, UEQ_Count), 2),
                  UEQ_Skew = round(psych::skew(!! expr), 2), # skew tail to left +; right -
                  UEQ_Kurtosis = round(psych::kurtosi(!! expr), 2) #,
                  # UEQ_z_Skew = round((sqrt(abs(UEQ_Skew)/UEQ_Count)), 2),
                  # UEQ_z_Kurtosis = round((sqrt(abs(UEQ_Kurtosis)/UEQ_Count)), 2)) %>%
          ) %>%
          ungroup()
}

lower_ci <- function(mean, se, n, conf_level = 0.95){
        lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
        upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# STRATIFY FUNCTION -------------------------------------------------------
# Source from: https://gist.github.com/mrdwab/6424112

# Function for stratify sampling
stratified <- function(df, group, size, select = NULL,
                       replace = FALSE, bothSets = FALSE) {
        if (is.null(select)) {
                df <- df
        } else {
                if (is.null(names(select))) stop("'select' must be a named list")
                if (!all(names(select) %in% names(df))) {
                        stop("Please verify your 'select' argument")
                }
                temp <- sapply(
                        names(select),
                        function(x) df[[x]] %in% select[[x]]
                )
                df <- df[rowSums(temp) == length(select), ]
        }
        df.interaction <- interaction(df[group], drop = TRUE)
        df.table <- table(df.interaction)
        df.split <- split(df, df.interaction)
        if (length(size) > 1) {
                if (length(size) != length(df.split)) {
                        stop(
                                "Number of groups is ", length(df.split),
                                " but number of sizes supplied is ", length(size)
                        )
                }
                if (is.null(names(size))) {
                        n <- setNames(size, names(df.split))
                        message(
                                sQuote("size"), " vector entered as:\n\nsize = structure(c(",
                                paste(n, collapse = ", "), "),\n.Names = c(",
                                paste(shQuote(names(n)), collapse = ", "), ")) \n\n"
                        )
                } else {
                        ifelse(all(names(size) %in% names(df.split)),
                               n <- size[names(df.split)],
                               stop(
                                       "Named vector supplied with names ",
                                       paste(names(size), collapse = ", "),
                                       "\n but the names for the group levels are ",
                                       paste(names(df.split), collapse = ", ")
                               )
                        )
                }
        } else if (size < 1) {
                n <- round(df.table * size, digits = 0)
        } else if (size >= 1) {
                if (all(df.table >= size) || isTRUE(replace)) {
                        n <- setNames(
                                rep(size, length.out = length(df.split)),
                                names(df.split)
                        )
                } else {
                        message(
                                "Some groups\n---",
                                paste(names(df.table[df.table < size]), collapse = ", "),
                                "---\ncontain fewer observations",
                                " than desired number of samples.\n",
                                "All observations have been returned from those groups."
                        )
                        n <- c(
                                sapply(df.table[df.table >= size], function(x) x <- size),
                                df.table[df.table < size]
                        )
                }
        }
        temp <- lapply(
                names(df.split),
                function(x) df.split[[x]][sample(df.table[x],
                                                 n[x],
                                                 replace = replace
                ), ]
        )
        set1 <- do.call("rbind", temp)
        
        if (isTRUE(bothSets)) {
                set2 <- df[!rownames(df) %in% rownames(set1), ]
                list(SET1 = set1, SET2 = set2)
        } else {
                set1
        }
}


# RANDOMIZE LEWISHAM ------------------------------------------------------
randomize_lewisham <- function(dat) {
        set.seed(01032019)
        res <- dat %>% # Randomization occurs here, clustered with prob. = 0.5
                mutate(Randomization = cluster_ra(clusters = School_Nesting, conditions=c("Control", "Experimental"), 
                                                  prob = 0.5)) # Result as data.frame with new column
        
        res_sum <- summary(res$Randomization) # Result sumamrized
        
        res_plot <- plot(res$Randomization) # Result plotted
        
        res_tab <- table(res$School_Nesting, res$Randomization) # Result tabulated
        
        res_declare <- declare_ra(clusters = res$School_Nesting,
                                  conditions = c("Control", "Experimental"),
                                  prob = 0.5)
        
        res_prob_mat <- res_declare$probabilities_matrix
        
        output <- list(res, res_sum, res_tab, res_declare, res_prob_mat, res_plot) # Results saved in list
        
        return(output) # List printed
        
        rm(.Random.seed, envir=.GlobalEnv)
}



# UEQ REVERSE -------------------------------------------------------------
# WIP, not function yet:
# HW_Control %>%
#         mutate_at(vars(starts_with("UEQ_")), as.numeric) %>%
#         mutate_at(vars(ends_with("_REV")), reverse_simple) %>%
#         select(UEQ_5_valuable_inferior_REV, UEQ_4_easy_learn_difficult_REV)


# IMPORT DATA SUFFOLK -----------------------------------------------------
Suffolk_District_Codes <- c(
        "E07000200", "E07000201", "E07000202", "E07000203",
        "E07000204", "E07000205", "E07000206"
)

IMPORT_DATA_SUFFOLK <- function(DATA) {
        DATA %>%
                slice(3:n()) %>%
                select(-c(
                        EndDate, Status, IPAddress, RecordedDate, RecipientLastName,
                        RecipientFirstName, RecipientEmail, ExternalReference, DistributionChannel,
                        UserLanguage
                )) %>%
                rename(Duration = `Duration (in seconds)`) %>%
                select(Consent_ = num_range(prefix = "Q", range = 3:9), everything()) %>%
                rename(
                        Use_BMI_Calc_YN = Q11, Use_C4L_YN = Q12, Contact_Service_YN = Q13,
                        Contact_GP_YN = Q14, Contact_School_Nurse_YN = Q15, Shared_Result_Child_YN = Q16,
                        Letter_Changed_Opinion_YN = Q17
                ) %>%
                select(UEQ_ = num_range(prefix = "Q19_", range = 1:26), everything()) %>%
                # Items are randomized to start randomly as positive or negative;
                # for scaling purpose, it's needed to trasnform items to be - 3 to + 3
                # from negative to positive (always)
                rename(
                        UEQ_1_annoying_enjoyable = UEQ_1, 
                        UEQ_2_not_understandable_understandable = UEQ_2,
                        UEQ_3_creative_dull_REV = UEQ_3, # Needs to be reversed to -3 / +3
                        UEQ_4_easy_learn_difficult_REV = UEQ_4, # Needs to be rev
                        UEQ_5_valuable_inferior_REV = UEQ_5, # Needs to be rev
                        UEQ_6_boring_exciting = UEQ_6, 
                        UEQ_7_not_interesting_interesting = UEQ_7,
                        UEQ_8_unpredictable_predictable = UEQ_8,
                        UEQ_9_fast_slow_REV = UEQ_9, # Needs to be rev
                        UEQ_10_inventive_conventional_REV = UEQ_10, # Needs to be rev
                        UEQ_11_obstructive_supportive = UEQ_11,
                        UEQ_12_good_bad_REV = UEQ_12, # Needs to be rev
                        UEQ_13_complicated_easy = UEQ_13, 
                        UEQ_14_unlikable_pleasing = UEQ_14,
                        UEQ_15_usual_leading_edge = UEQ_15, 
                        UEQ_16_unpleasant_pleasant = UEQ_16,
                        UEQ_17_secure_not_secure_REV = UEQ_17, # Needs to be rev
                        UEQ_18_motivating_demotivating_REV = UEQ_18, # Needs to be rev
                        UEQ_19_meets_expectations_does_not_REV = UEQ_19, # Needs to be rev
                        UEQ_20_inefficient_efficient = UEQ_20,
                        UEQ_21_clear_confusing_REV = UEQ_21, # Needs to be rev
                        UEQ_22_impractical_practical = UEQ_22,
                        UEQ_23_organized_cluttered_REV = UEQ_23, # Needs to be rev
                        UEQ_24_attractive_unattractive_REV = UEQ_24, # Needs to be rev
                        UEQ_25_friendly_unfriendly_REV = UEQ_25, # Needs to be rev
                        UEQ_26_conservative_innovative = UEQ_26
                ) %>% 
                rename(
                        Parent_Role_CAT = Q20,
                        Child_Gender_CAT = Q21,
                        Child_Age_CAT = Q22,
                        Parent_Other_Children_CAT = Q23,
                        Child_School_CAT = Q24,
                        Parent_Car_CAT = Q25,
                        Parent_Ethnicity_CAT = Q26,
                        Parent_Qualification_CAT = Q27,
                        Parent_Weight_Stat_CAT = Q28,
                        Parent_Phone = Q30,
                        Parent_Mail = Q31
                ) %>%
                mutate(Parent_English_CAT = "Not asked", # Originally missing
                       Parent_Employment_CAT = "Not asked",  # Originally missing
                       MDY_NCMP = NA_character_, # Same
                       Parent_Postcode_CAT = "Not asked" # Same
                       ) %>%
                select(Consent_1:LocationLongitude, everything())
}

EXTRACT_EMAIL_SUFFOLK <- function(DATA) {
        DATA %>% 
                filter(Consent_6 == "Yes") %>% # Approval to be contacted
                filter(!is.na(Parent_Mail)) %>% # Provided email
                select(Survey_Group, ResponseId:LocationLongitude, 
                       Parent_Role_CAT:Parent_Weight_Stat_CAT, Parent_Mail)
}

# IMPORT DATA LEWISHAM -------------------------------------------------------------

# Load Functions ----------------------------------------------------------

IMPORT_DATA_LEWISHAM <- function(DATA) {
        DATA %>%
                slice(3:n()) %>%
                select(-c(
                        EndDate, Status, IPAddress, RecordedDate, RecipientLastName,
                        RecipientFirstName, RecipientEmail, ExternalReference, DistributionChannel,
                        UserLanguage
                )) %>%
                rename(Duration = `Duration (in seconds)`) %>%
                select(Consent_ = num_range(prefix = "Q", range = 3:8), everything()) %>%
                rename(
                        Use_BMI_Calc_YN = Q10, Use_C4L_YN = Q11, Contact_Service_YN = Q12,
                        Contact_GP_YN = Q13, Contact_School_Nurse_YN = Q14, 
                        Shared_Result_Child_YN = Q15, Letter_Changed_Opinion_YN = Q16
                ) %>%
                select(UEQ_ = num_range(prefix = "Q18_", range = 1:26), everything()) %>%
                # Items are randomized to start randomly as positive or negative;
                # for scaling purpose, it's needed to trasnform items to be - 3 to + 3
                # from negative to positive (always)
                rename(
                        UEQ_1_annoying_enjoyable = UEQ_1, 
                        UEQ_2_not_understandable_understandable = UEQ_2,
                        UEQ_3_creative_dull_REV = UEQ_3, # Needs to be reversed to -3 / +3
                        UEQ_4_easy_learn_difficult_REV = UEQ_4, # Needs to be rev
                        UEQ_5_valuable_inferior_REV = UEQ_5, # Needs to be rev
                        UEQ_6_boring_exciting = UEQ_6, 
                        UEQ_7_not_interesting_interesting = UEQ_7,
                        UEQ_8_unpredictable_predictable = UEQ_8,
                        UEQ_9_fast_slow_REV = UEQ_9, # Needs to be rev
                        UEQ_10_inventive_conventional_REV = UEQ_10, # Needs to be rev
                        UEQ_11_obstructive_supportive = UEQ_11,
                        UEQ_12_good_bad_REV = UEQ_12, # Needs to be rev
                        UEQ_13_complicated_easy = UEQ_13, 
                        UEQ_14_unlikable_pleasing = UEQ_14,
                        UEQ_15_usual_leading_edge = UEQ_15, 
                        UEQ_16_unpleasant_pleasant = UEQ_16,
                        UEQ_17_secure_not_secure_REV = UEQ_17, # Needs to be rev
                        UEQ_18_motivating_demotivating_REV = UEQ_18, # Needs to be rev
                        UEQ_19_meets_expectations_does_not_REV = UEQ_19, # Needs to be rev
                        UEQ_20_inefficient_efficient = UEQ_20,
                        UEQ_21_clear_confusing_REV = UEQ_21, # Needs to be rev
                        UEQ_22_impractical_practical = UEQ_22,
                        UEQ_23_organized_cluttered_REV = UEQ_23, # Needs to be rev
                        UEQ_24_attractive_unattractive_REV = UEQ_24, # Needs to be rev
                        UEQ_25_friendly_unfriendly_REV = UEQ_25, # Needs to be rev
                        UEQ_26_conservative_innovative = UEQ_26
                ) %>%
                rename(
                        Parent_Role_CAT = Q19,
                        Child_Gender_CAT = Q20,
                        Child_Age_CAT = Q21,
                        Parent_Other_Children_CAT = Q22,
                        Child_School_CAT = Q23,
                        Parent_English_CAT = Q24,
                        Parent_Marital_CAT = Q25,
                        Parent_Disability_CAT = Q26,
                        Parent_Employment_CAT = Q27,
                        Parent_Ethnicity_CAT = Q28,
                        Parent_Qualification_CAT = Q29,
                        Parent_Weight_Stat_CAT = Q30,
                        Parent_Phone = Q32,
                        Parent_Mail = Q33
                ) %>%
                mutate(
                        Consent_7 = NA_real_, # Orig missing
                        MDY_NCMP = NA_character_, # Same
                        Parent_Postcode_CAT = "Not asked" # Same
                       ) %>%
                select(Consent_1:Consent_6, Consent_7, 
                       StartDate:LocationLongitude, everything())
}

EXTRACT_EMAIL_LEWISHAM <- function(DATA) {
        DATA %>% 
                # filter(Consent_6 == "Yes") %>% # Approval to be contacted # was not asked in consent
                filter(!is.na(Parent_Mail)) %>% # Provided email
                select(Survey_Group, ResponseId:LocationLongitude, 
                       Parent_Role_CAT:Parent_Weight_Stat_CAT, Parent_Mail)
}


# IMPORT DATA NATIONAL ----------------------------------------------------

IMPORT_DATA_NATIONAL <- function(DATA) {
        DATA %>%
                slice(3:n()) %>%
                select(-c(
                        EndDate, Status, IPAddress, RecordedDate, RecipientLastName,
                        RecipientFirstName, RecipientEmail, ExternalReference, DistributionChannel,
                        UserLanguage, Q2, `Q39 - Topics`
                )) %>%
                rename(Duration = `Duration (in seconds)`) %>%
                select(Consent_ = num_range(prefix = "Q", range = 4:10), everything()) %>%
                rename(
                        # Removed Use_BMI_Calc_YN = Q11, Use_C4L_YN = Q12, Letter_Changed_Opinion_YN = Q17
                        NCMP_Recieved_YN = Q11, MonthNCMP = `Q12#1_1`, DayNCMP = `Q12#2_1`,
                        YearNCMP = `Q12#3_1`, Contact_Service_YN = Q15, Contact_GP_YN = Q16, 
                        Contact_School_Nurse_YN = Q17, Shared_Result_Child_YN = Q18
                ) %>%
                select(UEQ_ = num_range(prefix = "Q20_", range = 1:26), everything()) %>%
                # Items are randomized to start randomly as positive or negative;
                # for scaling purpose, it's needed to trasnform items to be - 3 to + 3
                # from negative to positive (always)
                rename(
                        UEQ_1_annoying_enjoyable = UEQ_1, 
                        UEQ_2_not_understandable_understandable = UEQ_2,
                        UEQ_3_creative_dull_REV = UEQ_3, # Needs to be reversed to -3 / +3
                        UEQ_4_easy_learn_difficult_REV = UEQ_4, # Needs to be rev
                        UEQ_5_valuable_inferior_REV = UEQ_5, # Needs to be rev
                        UEQ_6_boring_exciting = UEQ_6, 
                        UEQ_7_not_interesting_interesting = UEQ_7,
                        UEQ_8_unpredictable_predictable = UEQ_8,
                        UEQ_9_fast_slow_REV = UEQ_9, # Needs to be rev
                        UEQ_10_inventive_conventional_REV = UEQ_10, # Needs to be rev
                        UEQ_11_obstructive_supportive = UEQ_11,
                        UEQ_12_good_bad_REV = UEQ_12, # Needs to be rev
                        UEQ_13_complicated_easy = UEQ_13, 
                        UEQ_14_unlikable_pleasing = UEQ_14,
                        UEQ_15_usual_leading_edge = UEQ_15, 
                        UEQ_16_unpleasant_pleasant = UEQ_16,
                        UEQ_17_secure_not_secure_REV = UEQ_17, # Needs to be rev
                        UEQ_18_motivating_demotivating_REV = UEQ_18, # Needs to be rev
                        UEQ_19_meets_expectations_does_not_REV = UEQ_19, # Needs to be rev
                        UEQ_20_inefficient_efficient = UEQ_20,
                        UEQ_21_clear_confusing_REV = UEQ_21, # Needs to be rev
                        UEQ_22_impractical_practical = UEQ_22,
                        UEQ_23_organized_cluttered_REV = UEQ_23, # Needs to be rev
                        UEQ_24_attractive_unattractive_REV = UEQ_24, # Needs to be rev
                        UEQ_25_friendly_unfriendly_REV = UEQ_25, # Needs to be rev
                        UEQ_26_conservative_innovative = UEQ_26
                ) %>%
                rename(
                        Parent_Role_CAT = Q21,
                        Child_Gender_CAT = Q22,
                        Child_Age_CAT = Q23,
                        Child_Height_Ft_NUM = Q24_1_1,
                        Child_Height_Inch_NUM = Q24_1_2,
                        Child_Height_CM_NUM = Q24_1_3,
                        Child_Weight_Pound_NUM = Q25_1_1,
                        Child_Weight_KG_NUM = Q25_1_2,
                        Child_WeightCatLetter_CAT = Q26,
                        Parent_Other_Children_CAT = Q27,
                        Parent_Other_Children_Duplicate_Answer_CAT = Q28,
                        # Child_School_CAT = Q24,
                        # Parent_Car_CAT = Q25,
                        # Parent_Disability_CAT
                        Parent_English_CAT = Q29,
                        Parent_Employment_CAT = Q30,
                        Parent_Ethnicity_CAT = Q31,
                        Parent_Qualification_CAT = Q32,
                        Parent_Weight_Stat_CAT = Q33,
                        Parent_Postcode_CAT = Q34,
                        Parent_School_Lew_FILTER = Q35,
                        Parent_School_Lew_CAT = Q36,
                        Parent_ContactMe_FILTER = Q37,
                        Parent_Mail = Q38,
                        Parent_Phone = Q39
                ) %>%
                unite("MDY_NCMP", c(MonthNCMP, DayNCMP, YearNCMP), sep = "-", na.rm = FALSE, remove = TRUE) %>%
                mutate(
                        MDY_NCMP = na_if(MDY_NCMP, "NA-NA-NA"),
                        MDY_NCMP = lubridate::mdy(MDY_NCMP),
                        Child_WeightCatLetter_CAT = case_when(
                                Child_WeightCatLetter_CAT == "Healthy weight" ~ "HW",
                                Child_WeightCatLetter_CAT == "Underweight" ~ "UW",
                                Child_WeightCatLetter_CAT == "Overweight" ~ "OW",
                                Child_WeightCatLetter_CAT == "Very overweight" ~ "VOW",
                                Child_WeightCatLetter_CAT == "Refuse to say" ~ "Refuse to say",
                                TRUE ~ NA_character_
                        ),
                        Use_C4L_YN = "Not asked", # Originally missing, must be added
                        Use_BMI_Calc_YN = "Not asked", # Same
                        Letter_Changed_Opinion_YN = "Not asked", # Same
                        Child_School_CAT = "Not asked" # Same
                        ) %>%
                select(Consent_1:LocationLongitude, everything())
}

EXTRACT_EMAIL_NATIONAL <- function(DATA) {
        DATA %>% 
                filter(Consent_6 == "Yes") %>% # Approval to be contacted
                filter(!is.na(Parent_Mail)) %>% # Provided email
                select(ResponseId:LocationLongitude, 
                       Parent_Role_CAT:Parent_Weight_Stat_CAT, Parent_Mail)
}

# Child_Height_CM_NUM
# Child_Height_Ft_NUM
# Child_Height_Inch_NUM