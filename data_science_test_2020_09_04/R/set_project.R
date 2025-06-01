# Virtual environment -----------------------------------------------------
# renv::init()
# renv::restore()
# renv::snapshot()

# Libraries --------------------------------------------------------
pkgs_cran <- c("renv", "remotes", "here", "tidyverse", "readxl", "httr", 
               "colorspace","ids", "randomizr", "tmaptools", "tmap", "sf", 
               "ggsignif", "showtext", "janitor", "gghighlight", "ggcorrplot",
               "patchwork", "flextable", "GPArotation", "broom", "synthpop",
               "viridis", "recipes", "car", "effects", "polycor", "ggtext",
               "Cairo", "ggfortify", "ggeffects", "conflicted", "ggdag", "checkmate"
               # MORE PKGS HERE
               ) # install and load last

# Loads packages, if they can't be load, it installs them.
for (package in pkgs_cran) {
     if (!require(package, character.only = TRUE, quietly = TRUE)) {
          install.packages(package)
          library(package, character.only = TRUE)
     }
}

# Other pkgs (installed externally or individaully)
# install.packages("funModeling", dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))
# install.packages("latticeExtra")
# install.packages("psych")
# remotes::install_github("rOpenSci/fingertipsR",
#                         build_vignettes = TRUE,
#                         dependencies = "suggests")
# Test libraries ----------------------------------------------------------
pkgs_test <- pkgs_cran

lapply(pkgs_test, require, character.only = TRUE)
lapply(pkgs_test, packageVersion)
conflict_scout() # Functions surrounded by [] have been chosen using one of the built-in rules.
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("chisq.test", "stats")
conflict_prefer("fisher.test", "stats")
conflict_prefer("tidy", "broom")
conflict_prefer("update", "stats")
conflict_prefer("select", "dplyr")

# Themes, fonts, labels ---------------------------------------------------
font_paths() # Check font paths
font_add("arial", "Arial.ttf") # Add windows font Arial
# font_add("times", "Times New Roman.ttf") # Add windows font Times
showtext_auto() # Use showtext globally

# Theme -------------------------------------------------------------------
project_theme <- function(base_family = "arial",
                          base_text_size = 15,
                          base_text_axis_x_size = 12, 
                          base_text_axis_x_angle = 90, 
                          base_text_axis_x_hjust = 1,
                          base_text_axis_x_vjust = 0.5
) {
  theme(axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        text = element_text(family = base_family, 
                            size = base_text_size),
        axis.text.x = element_text(size = base_text_axis_x_size, 
                                   angle = base_text_axis_x_angle, 
                                   hjust = base_text_axis_x_hjust, 
                                   vjust = base_text_axis_x_vjust))
}

# Labels ------------------------------------------------------------------
default_labs <- function(x_lab = NULL, 
                         y_lab = NULL, 
                         title_lab = NULL, 
                         colour_lab = NULL,
                         fill_lab = NULL,
                         subtitle_lab = NULL, 
                         caption_lab = NULL, 
                         tag_lab = NULL, 
                         label_lab = NULL) {
  labs(x = x_lab, 
       y = y_lab, 
       title = title_lab,
       colour = colour_lab,
       fill = fill_lab,
       subtitle = subtitle_lab, 
       caption = caption_lab, 
       tag = tag_lab, 
       label = label_lab)
}


# Labels breaker ----------------------------------------------------------
# Tiny function to allow labels in plots to break line after the first word
# This makes the labels more condensed.

scale_insert_newline <- function(set_width = 20) { # 20 as default seems okay
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "), width = set_width))
}


# Palette ----------------------------------------------------------------
palette_gray_mc <- colorspace::sequential_hcl(n = 5, h = c(-360, -360), 
                                              c = c(0, 0, 0), l = c(20, 85), 
                                              power = c(0, 1), fixup = FALSE, 
                                              register = "palette_gray_mc")

# Color from pallette above pal_gray_mc[3], "#7D7D7D"
color_e <- "#D4D4D4"
color_d <- "#A8A8A8"
color_c <- "#7D7D7D"
color_b <- "#555555"
color_a <- "#303030"

# Register the palette.
colorspace::sequential_hcl(n = 5, palette = "palette_gray_mc")
