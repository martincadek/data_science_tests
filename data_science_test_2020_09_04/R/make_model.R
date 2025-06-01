# README ------------------------------------------------------------------
# The following code is part of a script that was utilised to run analyses on 
# data from my PhD project.

# Library -----------------------------------------------------------------
library(here) # to run from different working directory


# LOAD --------------------------------------------------------------------
all_data <- read_rds(here("input", "synthetic_data.rds"))

# Plotting function -------------------------------------------------------
ggplot_lm <- function(data, outcome, explanatory, y_lab = NULL,
                      x_lab = NULL, 
                      title_lab = NULL,
                      guide_color = "colorbar") {
  outcome<- enquo(outcome)
  explanatory <- enquo(explanatory)
  ggplot(data = data, aes(y = !! outcome, x = !! explanatory, color = !! outcome)) +
    geom_point(alpha = 0.65,
               size = 2,
               position = position_jitter(width = 0.05, height = 0)) +
    geom_smooth(se = FALSE, method = "lm") +
    scale_y_continuous(limits = c(-3, 3)) +
    scale_x_continuous(n.breaks = 2) +
    scale_color_viridis_c(option = "E", end = 0.5, begin = 0) +
    project_theme(base_text_axis_x_angle = 0, base_text_axis_x_hjust = 0.5, 
                  base_text_size = 10) +
    theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dotted")) + 
    default_labs(title_lab = title_lab,
                 y_lab = y_lab,
                 x_lab = x_lab) +
    guides(colour = guide_color)
}


# Clean function ----------------------------------------------------
clean_gg_objects <- function(nm = c(gg1, gg2, gg3, gg4, gg5, 
                         gg6, gg7, gg8,
                         patchwork_label_y, 
                         patchwork_plots)) {
     rm(list = as.character(substitute(nm)), envir = .GlobalEnv )
}


# Collapse categories -----------------------------------------------------
# Coerce variables
# Collapse levels of factors
# Filter out any variables as missing
# Prepare data for binary logistic regression.

coerce_data <- function(data) {
  data %>%
       # Binary recoding
       mutate(is_Contact_Service_10 = ifelse(Contact_Service_YN == "Yes", 1, 0),
              is_Contact_GP_10 = ifelse(Contact_GP_YN == "Yes", 1, 0),
              is_Contact_School_Nurse_10 = ifelse(Contact_School_Nurse_YN == "Yes", 1, 0),
              is_Shared_Result_Child_10 = ifelse(Shared_Result_Child_YN == "Yes", 1, 0)
              ) %>%
       # Filtering missing variables; do before recoding, it will otherwise create empty levels
       filter_at(vars(Child_WeightCatLetter_CAT, Design_Version, Parent_Role_CAT,
                      Child_Gender_CAT, Child_Age_CAT, Parent_Other_Children_CAT,
                      Parent_Qualification_CAT, Parent_Weight_Stat_CAT
       ), all_vars(. != "Missing" & . != "Refuse to say")) %>%
      mutate(
          Child_WeightCatLetter_CAT_short = ifelse(Child_WeightCatLetter_CAT == "HW", "HW", "non_HW"),
          Parent_Qualification_CAT_short = ifelse(Parent_Qualification_CAT == "A-Levels or equivalent", "non_University", 
                                                  ifelse(Parent_Qualification_CAT == "GCSEs or equivalent", "non_University", "University")),
          Parent_Weight_Stat_CAT_short = ifelse(Parent_Weight_Stat_CAT == "HW", "HW", "non_HW"),
          Parent_Other_Children_CAT_short = ifelse(Parent_Other_Children_CAT == "Yes, and they've received the NCMP", "Yes", "No"),
          Parent_Other_Children_CAT_short = ifelse(Parent_Other_Children_CAT == "Yes, and they've received the NCMP", "Yes", "No")
             ) %>%
       # Ensuring all is factor; but only for the key variables, leave unanalysed as chars
       mutate_at(vars(is_Contact_GP_10,
                      is_Contact_School_Nurse_10,
                      is_Contact_Service_10, 
                      is_Shared_Result_Child_10,
                      Design_Version,
                      Child_WeightCatLetter_CAT_short,
                      Child_Gender_CAT,
                      Child_Age_CAT,
                      Parent_Qualification_CAT_short,
                      Parent_Role_CAT,
                      Parent_Weight_Stat_CAT_short,
                      Parent_Other_Children_CAT_short
                      
       ), factor)
}

all_data <- coerce_data(data = all_data)

# Create dummy variables - can be visualised in the model
dummy_recipe <- recipe( ~ ., data = all_data) %>%
     step_dummy(is_Contact_GP_10,
                is_Contact_School_Nurse_10,
                is_Contact_Service_10, 
                is_Shared_Result_Child_10,
                Design_Version,
                Child_WeightCatLetter_CAT_short,
                Child_Gender_CAT,
                Child_Age_CAT,
                Parent_Qualification_CAT_short,
                Parent_Role_CAT,
                Parent_Weight_Stat_CAT_short,
                Parent_Other_Children_CAT_short)

all_data_dummy <- dummy_recipe %>%
     prep(retain = TRUE) %>% 
     juice()    

# Correlation --------------------------------------------
# Assess how DV and IV correlate and remove low correlation variables.
# I am using heterogeneous correlation function which automatically computes corr for
# Pearson (numeric-numeric),
# Polyserial (numeric-ordinal),
# and Polychoric (ordinal-ordinal)

# Use correlation matrix to identify low performing variables.
het2_cor_data <- all_data %>%
  # Select variables that will be relevant for further analysis.
  select(is_Contact_GP_10,
         is_Contact_School_Nurse_10,
         is_Contact_Service_10, 
         is_Shared_Result_Child_10,
         Attractiveness,
         Perspicuity,
         Efficiency,
         Dependability,
         Stimulation,
         Novelty,
         Design_Version,
         Child_WeightCatLetter_CAT_short,
         Child_Gender_CAT,
         Child_Age_CAT,
         Parent_Qualification_CAT_short,
         Parent_Role_CAT,
         Parent_Weight_Stat_CAT_short,
         Parent_Other_Children_CAT_short) %>%
  rename_all(list(~str_remove_all(., "is_"))) %>%
  rename_all(list(~str_remove_all(., "_10"))) %>%
  rename_all(list(~str_remove_all(., "_CAT|_short"))) %>%
  rename_all(list(~str_replace_all(., "_", " "))) %>%
  polycor::hetcor()

CairoPNG(file = here("output", "all_plots", "correlation_categorical.png"),
    height = 1000,
    width = 1400)
ggcorrplot::ggcorrplot(ifelse(het2_cor_data$correlations >= 0.20 | het2_cor_data$correlations <= -0.20, het2_cor_data$correlations, 0),
                       type = c("upper"),
                       colors = viridis::cividis(n = 3, begin = 0, end = 1),
                       ggtheme = project_theme(),
                       lab = TRUE, 
                       legend.title = "Heterogeneous\ncorrelation matrix",
                       title = "Correlation between all explanatory and outcome variables"
) + 
  # I am using element markdown to highlight text.
  theme(axis.text.x = element_markdown(color = rep(c(viridis::cividis(n = 2, begin = 0.2, end = 0.6)), times = c(10, 7))),
        axis.text.y = element_markdown(color = rep(c(viridis::cividis(n = 2, begin = 0.2, end = 0.6)), times = c(9, 8))))

dev.off()


# Compare collapsed -------------------------------------------------------
# The visualisation compares collapsed and non-collapsed factors
# pre-collapse
pre <- all_data %>% 
  select(
    Child_WeightCatLetter_CAT,
    Design_Version,
    Parent_Weight_Stat_CAT,
    Parent_Other_Children_CAT,
    Child_Age_CAT,
    Parent_Qualification_CAT,
    Parent_Role_CAT,
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "Variables", 
               values_to = "Levels") %>%
  group_by(Variables, Levels) %>%
  count(name = "Total") %>%
  ungroup() %>%
  mutate(Variables = str_remove_all(Variables, "_CAT|_short"),
         Variables = str_replace_all(Variables, "_", " "),
         Levels = str_replace_all(Levels, "_", " "))


# post-collapse
post <- all_data %>% 
  select(
    Child_WeightCatLetter_CAT_short,
    Design_Version,
    Parent_Weight_Stat_CAT_short,
    Parent_Other_Children_CAT_short,
    Child_Age_CAT,
    Parent_Qualification_CAT_short,
    Parent_Role_CAT,
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "Variables", 
               values_to = "Levels") %>%
  group_by(Variables, Levels) %>%
  count(name = "Total") %>%
  ungroup() %>%
  mutate(Variables = str_remove_all(Variables, "_CAT|_short"),
         Variables = str_replace_all(Variables, "_", " "),
         Levels = str_replace_all(Levels, "_", " "))

ggpre <- ggplot(data = pre, aes(x = Levels, y = Total)) +
  geom_col(fill = color_b) +
  facet_wrap(~ Variables, scales = "free_x") +
  project_theme(base_text_axis_x_angle = 0, base_text_axis_x_hjust = 0.5, base_text_axis_x_vjust = 0.5) +
  scale_insert_newline(set_width = 10) +
  default_labs(title_lab = "Before the levels were merged")

ggpost <- ggplot(data = post, aes(x = Levels, y = Total)) +
  geom_col(fill = color_e) +
  facet_wrap(~ Variables, scales = "free_x") +
  project_theme(base_text_axis_x_angle = 0, base_text_axis_x_hjust = 0.5, base_text_axis_x_vjust = 0.5) +
  scale_insert_newline(set_width = 10) +
  default_labs(title_lab = "After the levels were merged")


CairoPNG(here("output","all_plots", "pre_post_levels_merged.png"), width = 1400, height = 900)
ggpre / ggpost
dev.off()


# UEQ Attractiveness -------------------------------------------------------
# Visualise predictors ----------------------------------------------------
# To fit a regression, I need to coerce the values to a numeric vector lying 
# between 0 and 1. So I have to use _dummy data.

gg1 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Child_WeightCatLetter_CAT_short_non_HW, 
          x_lab = "Child: HW (0)\nnon HW (1)",
          title_lab = "A")

gg2 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Design_Version_Experimental, 
          x_lab = "Observetional & Control (0)\nExperimental (1)",
          title_lab = "B")

gg3 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Design_Version_Observational, 
          x_lab = "Experimental & Control (0)\nObservational design (1)",
          title_lab = "C")

gg4 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Parent_Weight_Stat_CAT_short_non_HW, 
          x_lab = "Parent: HW (0)\nnon HW (1)", 
          title_lab = "D")

gg5 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Parent_Other_Children_CAT_short_Yes, 
          x_lab = "No previous NCMP experience (0)\nPrevious experience (1)", 
          title_lab = "E")

gg6 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Child_Age_CAT_Year.6..aged.10...11., 
          x_lab = "Reception year (0)\nYear 6 (1)", 
          title_lab = "F")

gg7 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Parent_Qualification_CAT_short_University, 
          x_lab = "Not university (0)\nUniversity (1)", 
          title_lab = "G")

gg8 <- ggplot_lm(data = all_data_dummy, outcome = Attractiveness, 
          explanatory = Parent_Role_CAT_Mother, 
          x_lab = "Father (0)\nMother (1)", 
          title_lab = "H")

# Create graph to show the association without the model
patchwork_label_y <- wrap_elements(panel = grid::textGrob("UEQ: Attractiveness", rot = 90))
patchwork_plots <- patchwork_label_y | (gg1 / gg2 / gg3 / gg4) | (gg5 / gg6 / gg7 / gg8)

# Save the graph
CairoPNG(filename = here("output", "all_plots", "predictors_UEQ_Attractiveness_vars.png"), width = 700, height = 750)
patchwork_plots + 
  plot_annotation(
    title = "Attractiveness (outcome) ~ explanatory variables",
    subtitle = "The outcome variable is plotted against explanatory variables with random horizontal jitter"
  ) +
  plot_layout(guides = "collect", 
              ncol = 3, nrow = 1,
              widths = c(1, 9, 9))
dev.off()

clean_gg_objects() 
# The following function will throw warning if gg object is not found.
# It also throws error "c" not found.

# Model -------------------------------------------------------------------
# Define models that will be tested.
iv_m1 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short)
iv_m2 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version)
iv_m3 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version + Parent_Weight_Stat_CAT_short)
iv_m4 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version + Parent_Weight_Stat_CAT_short + Parent_Other_Children_CAT_short)
iv_m5 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version + Parent_Weight_Stat_CAT_short + Parent_Other_Children_CAT_short + Child_Age_CAT)
iv_m6 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version + Parent_Weight_Stat_CAT_short + Parent_Other_Children_CAT_short + Child_Age_CAT + Parent_Qualification_CAT_short)
iv_m7 <- expr(Attractiveness ~ Child_WeightCatLetter_CAT_short + Design_Version + Parent_Weight_Stat_CAT_short + Parent_Other_Children_CAT_short + Child_Age_CAT + Parent_Qualification_CAT_short + Parent_Role_CAT)

ueq_att_mod_1 <- lm(data = all_data,  iv_m1)
ueq_att_mod_2 <- lm(data = all_data,  iv_m2)
ueq_att_mod_3 <- lm(data = all_data,  iv_m3)
ueq_att_mod_4 <- lm(data = all_data,  iv_m4)
ueq_att_mod_5 <- lm(data = all_data,  iv_m5)
ueq_att_mod_6 <- lm(data = all_data,  iv_m6)
ueq_att_mod_7 <- lm(data = all_data,  iv_m7)

# Report this:
compareCoefs(ueq_att_mod_1, 
             ueq_att_mod_2, 
             ueq_att_mod_3, 
             ueq_att_mod_4, 
             ueq_att_mod_5, 
             ueq_att_mod_6, 
             ueq_att_mod_7, pvals = TRUE) %>%
  as_tibble(rownames = "Coefficients") %>%
  drop_na(Coefficients, `Model 7`) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(Coefficients = str_replace_all(Coefficients, "_CAT|_short", " "),
         Coefficients = str_replace_all(Coefficients, "_", " ")) %>%
  mutate_all(replace_na, "*") %>%
  qflextable()

# Report this:
bind_rows(
  glance(ueq_att_mod_1),
  glance(ueq_att_mod_2),
  glance(ueq_att_mod_3),
  glance(ueq_att_mod_4),
  glance(ueq_att_mod_5),
  glance(ueq_att_mod_6),
  glance(ueq_att_mod_7), .id = "model") %>%
  mutate_if(is.numeric, round, 2) %>%
  qflextable()

# Compare -----------------------------------------------------------------
# Where significant, the inclusion of additional variable makes difference
model_comparison <- anova(
  ueq_att_mod_1, 
  ueq_att_mod_2, 
  ueq_att_mod_3, 
  ueq_att_mod_4, 
  ueq_att_mod_5, 
  ueq_att_mod_6, 
  ueq_att_mod_7, test = "Chisq")

# Report this:
tidy(model_comparison) %>%
  rownames_to_column(var = "model") %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_all(replace_na, "*") %>%
  qflextable()

aic_temp <- AIC(ueq_att_mod_1, 
    ueq_att_mod_2, 
    ueq_att_mod_3, 
    ueq_att_mod_4, 
    ueq_att_mod_5, 
    ueq_att_mod_6, 
    ueq_att_mod_7) %>%
  rownames_to_column()

bic_temp <- BIC(ueq_att_mod_1, 
    ueq_att_mod_2, 
    ueq_att_mod_3, 
    ueq_att_mod_4, 
    ueq_att_mod_5, 
    ueq_att_mod_6, 
    ueq_att_mod_7) %>%
  rownames_to_column()

aic_bic_df <- full_join(aic_temp, bic_temp, by = c("rowname", "df")) %>%
  pivot_longer(-c(rowname, df), names_to = "label", values_to = "bic_aic")

rm(aic_temp, bic_temp)

# Report this:
CairoPNG(filename = here("all_plots", "aicbic_UEQ_Attractiveness_mod.png"), width = 500, height = 400)
ggplot(data = aic_bic_df) +
  geom_line(aes(x = df, y = bic_aic, colour = label), size = 1.5) +
  geom_point(aes(x = df, y = bic_aic), color = color_c, size = 3) +
  scale_y_continuous(limits = NULL) +
  scale_x_continuous(limits = c(3, 11), n.breaks = 7) +
  scale_color_viridis_d(option = "E", begin = 0.2, end = 0.7) +
  default_labs(x_lab = "Degree of freedom (DF)", 
               y_lab = "Criterion value", 
               colour_lab = "AIC/BIC", 
               title_lab = "AIC and BIC change across models") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor.x = element_blank())
dev.off()

# DECISION: USE MODEL 2

# Report this:
CairoPNG(filename = here("output", "all_plots", "diagnostic_1_UEQ_Attractiveness.png"), width = 700, height = 600)
autoplot(ueq_att_mod_2, label.repel = 1) +
  theme_minimal()
dev.off()

# Report this:
CairoPNG(filename = here("output", "all_plots", "diagnostic_2_UEQ_Attractiveness.png"), width = 700, height = 600)
influenceIndexPlot(ueq_att_mod_2, id = list(n=3))
dev.off()

# Remove outlier(s), Index: 33
ueq_att_outlier_33 <- update(ueq_att_mod_2, subset = rownames(all_data) != 33)
compareCoefs(ueq_att_mod_2, ueq_att_outlier_33)

# Final model -------------------------------------------------------------

# FINAL DECISION: USE MODEL 2 WITH REMOVED OUTLIER
ueq_att_final <- ueq_att_outlier_33

# Report this:
ueq_att_final %>% tidy() %>%
  mutate(conf_low = as_tibble(confint(ueq_att_final))$`2.5 %`,
         conf_upper = as_tibble(confint(ueq_att_final))$`97.5 %`) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(term = str_replace_all(term, "_CAT|_short", " "),
            term = str_replace_all(term, "_", " ")) %>%
  qflextable()

# Report this:
glance(ueq_att_final) %>%
  mutate_if(is.numeric, round, 2) %>%
  qflextable()

# Report this & Visualise:
predicted <- ggpredict(ueq_att_final)
gg1 <- plot(predicted, 
     colors = "bw", 
     rawdata = TRUE)$Child_WeightCatLetter_CAT_short +
  labs(
    x = "Child: HW (0)\nnon HW (1)", 
    y = NULL, 
    title = "Predicted values of attractiveness per letter")

gg2 <- plot(predicted, 
     colors = "bw", 
     rawdata = TRUE)$Design_Version +
  labs(
    x = "Design version", 
    y = NULL, 
    title = "Predicted values of attractiveness per design")

patchwork_label_y <- wrap_elements(panel = grid::textGrob("UEQ: Predicted attractiveness", rot = 90))
patchwork_plots <- patchwork_label_y | (gg1 / gg2)

# Report this:
CairoPNG(filename = here("all_plots", "final_vis_UEQ_Attractiveness.png"), width = 730, height = 620)
patchwork_plots + 
  plot_annotation(
    title = "Attractiveness (predicted) ~ explanatory variables",
    subtitle = "Marginal effects plots of predicted values layered on the raw data with random jitter"
  ) +
  plot_layout(guides = "collect", 
              ncol = 2, nrow = 1,
              widths = c(1, 9))
dev.off()

clean_gg_objects()


# DAG ----------------------------------------------------------------
# #A9A9A9 = Gray, Excluded variable
# #6B9D59 = Green, Outcome
# #AC8C4E = Bronze, Exposure
# #00A396 = Turquoise, Competing exposure
# #C87A8A = Red, Confounder

dag_anatomy_colour <- c(
  "Outcome"               = "#6B9D59",
  "Exposure"              = "#AC8C4E",
  "Competing exposure"    = "#00A396",
  "Confounder"            = "#C87A8A",
  "Excluded"              = "#A9A9A9"
)

dag_anatomy_label <- c(
  "Outcome"               = "Outcome",
  "Exposure"              = "Exposure",
  "Competing exposure"    = "Competing exposure",
  "Confounder"            = "Confounder",
  "Excluded"              = "Excluded"   
)

labels_dag_UEQ <- c(
  "UEQ"                                = "User Experience Questionnaire",
  "Letter_received"                    = "Letter received",
  "IMD_Home_Derived"                   = "Derived household IMD",
  "Child_Weight_Status"                = "Child's weight",
  # "Child_Age"                          = "Child's age",
  # "Child_Gender"                       = "Child's gender",
  # "Employment_Status"                  = "Parent's employment status",
  "Parent_Disability"                  = "Parent's disability",
  "Parent_Education"                   = "Parent's education",
  "Parent_English_Speaking"            = "Parent's English fluency",
  "Parent_Ethnicity"                   = "Parent's ethnicity",
  "Parent_Gender"                      = "Parent's gender",
  "Parent_Previous_NCMP_Experience"    = "Parent's previous NCMP experience",
  "Parent_Self_Reported_Weight"        = "Parent's self-reported weight",
  # "Parent_Single"                      = "Parent's marital status",
  "School_IMD"                         = "School IMD")

legend_colors_UEQ <- c(
  "User Experience Questionnaire"      = "#6B9D59",
  "Letter received"                       = "#AC8C4E",
  "Derived household IMD"                 = "#A9A9A9",
  "School IMD"                            = "#A9A9A9",
  "Child's weight"                        = "#C87A8A",
  # "Child's age"                           = "#00A396",
  # "Child's gender"                        = "#A9A9A9",
  # "Parent's employment status"            = "#A9A9A9",
  "Parent's disability"                   = "#A9A9A9",
  "Parent's English fluency"              = "#A9A9A9",
  "Parent's ethnicity"                    = "#A9A9A9",
  "Parent's education"                    = "#00A396",
  "Parent's gender"                       = "#00A396",
  "Parent's previous NCMP experience"     = "#00A396",
  "Parent's self-reported weight"         = "#00A396"
  # "Parent's marital status"               = "#A9A9A9"
)

UEQ_dag_data <- dagify(
  UEQ ~ Letter_received + IMD_Home_Derived + Child_Weight_Status + Parent_Disability + Parent_Education +
    Parent_English_Speaking + Parent_Ethnicity + Parent_Gender + Parent_Previous_NCMP_Experience +
    Parent_Self_Reported_Weight,
  Letter_received ~ Child_Weight_Status + IMD_Home_Derived,
  IMD_Home_Derived ~ School_IMD,
  exposure = "Letter_received",
  outcome = "UEQ",
  labels = labels_dag_UEQ) %>% 
  tidy_dagitty() %>% 
  mutate(dag_anatomy = case_when(
    label == "User Experience Questionnaire"     ~ "Outcome",
    label == "Letter received"                   ~ "Exposure",
    label == "Derived household IMD"             ~ "Excluded",
    label == "Child's weight"                    ~ "Confounder",
    label == "School IMD"                        ~ "Excluded",
    # label == "Child's age"                       ~ "Competing exposure",
    # label == "Child's gender"                    ~ "Excluded",
    # label == "Parent's employment status"        ~ "Excluded",
    label == "Parent's disability"               ~ "Excluded",
    label == "Parent's education"                ~ "Competing exposure",
    label == "Parent's English fluency"          ~ "Excluded",
    label == "Parent's ethnicity"                ~ "Excluded",
    label == "Parent's gender"                   ~ "Competing exposure",
    label == "Parent's previous NCMP experience" ~ "Competing exposure",
    label == "Parent's self-reported weight"     ~ "Competing exposure"
    # label == "Parent's marital status"           ~ "Excluded"
  ))

dag_UEQ <- ggplot(UEQ_dag_data, aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_edges() +
  geom_dag_point(aes(color = dag_anatomy)) +
  geom_dag_label_repel(aes(label = label, fill = label), seed = 2020,
                       direction = "y", fontface = "bold", color = "white") +
  # scale_color_discrete_qualitative(palette = "Dark 2") +
  # scale_fill_discrete_qualitative(palette = "Dark 2") +
  scale_color_manual(values = dag_anatomy_colour, labels = dag_anatomy_label) +
  scale_fill_manual(values = legend_colors_UEQ) +
  scale_adjusted() +
  guides(
    color = guide_legend(title = "Anatomy of DAG"), 
    fill = FALSE) +
  expand_plot(expand_y = expansion(c(0.2, 0.2))) +
  #guides(color = FALSE, fill = FALSE) +
  labs(title = "Model 1: User Experience Questionnaire (updated)") +
  theme_dag(base_size = 15, base_family = "Arial")

CairoPNG(filename = here("output", "all_plots", "dag_updated_m1_UEQ.png"), width = 1000, height = 1000)
dag_UEQ
dev.off()


