# Load --------------------------------------------------------------------
dat_feedback <- data.table::fread("data/data_for_task.csv")
setnames(dat_feedback, names(dat_feedback), snakecase::to_snake_case(names(dat_feedback)))
# Drop url as this is really useless column, checked and there's nothing
dat_feedback <- dat_feedback[, -"url"]

data_report <- as.data.table(funModeling::df_status(dat_feedback))[order(-unique)]
data_report
# Coerce ------------------------------------------------------------------
dat_feedback[, creation_date_ymdhms := lubridate::dmy_hm(creation_date)]

# Prepare ID column
dat_feedback[, id_column := .I]
split_sequence <- max(lengths(strsplit(dat_feedback$emotion_rating_checkbox_74056, ",")))
dat_feedback[, paste0("emotion_rating_checkbox_", 1:split_sequence) := tstrsplit(emotion_rating_checkbox_74056, ",", fixed = TRUE, fill = "no_response")]
grabber_intersct <- grep("emotion_rating_checkbox_\\d{1}$", colnames(dat_feedback), value = TRUE)

# Prepare dummy columns
dummy_columns <- dat_feedback[, c("id_column", ..grabber_intersct)] %>% 
     melt("id_column") %>% 
     dcast(id_column ~ value, fun.agg = length) %>% 
     .[, id_column := NULL]
dummy_columns <- dummy_columns[, -"no_response"]

dat_feedback[, names(dummy_columns) := dummy_columns][]



# Visualise emotions + other scales ------------------------------------------------------
grabber_emotion_rating_all <- colnames(dat_feedback[, Annoyed:Worried])
grabber_emotion_rating_positive <- c("Confident", "Delighted", "Excited", "Impressed", "Interested", "Relaxed")
grabber_emotion_rating_negative <- c("Annoyed", "Bored", "Disappointed", "Frustrated", "Tense", "Worried")

upset(dat_feedback, intersect = grabber_emotion_rating_all, 
      name = "Emotions", wrap = TRUE, set_sizes = FALSE,
      min_size = 2,
      width_ratio = 0.1
) + ggtitle("Upset plot of all emotions (positive and negative, minimum at 2)")
     

upset(dat_feedback, intersect = grabber_emotion_rating_positive, 
      name = "Emotions", wrap = TRUE, set_sizes = FALSE,
      min_size = 3,
      width_ratio = 0.1, 
      min_degree = 1
) + ggtitle("Upset plot of positive emotions")

upset(dat_feedback, intersect = grabber_emotion_rating_negative, 
      name = "Emotions", wrap = TRUE, set_sizes = FALSE,
      min_size = 2,
      width_ratio = 0.1,
      min_degree = 1
) + ggtitle("Upset plot of negative emotions")


upset(dat_feedback, intersect = grabber_emotion_rating_positive, 
      name = "Emotions", wrap = TRUE, set_sizes = FALSE,
      min_size = 3,
      width_ratio = 0.1, 
      min_degree = 1,
      annotations = list(
           # if not specified, the mode will follow the mode set in `upset()` call (here: `inclusive_intersection`)
           "CES"=(
                ggplot(mapping=aes(y=ces_rating_scale_1_7))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Attractiveness"=(
                ggplot(mapping=aes(y=attractiveness_rating_grading_14772_scale_1_5 ))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Comprehension"=(
                ggplot(mapping=aes(y=comprehension_rating_grading_scale_1_5))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Satisfaction"=(
                ggplot(mapping=aes(y=satisfaction))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Trustworthiness"=(
                ggplot(mapping=aes(y=trustworthiness_rating_grading_96093_scale_1_5))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           )
      )
      ) + ggtitle("Upset plot of positive emotions (other scales are in exclusive intersection, minimum at 3)")

upset(dat_feedback, intersect = grabber_emotion_rating_negative, 
      name = "Emotions", wrap = TRUE, set_sizes = FALSE,
      min_size = 1,
      width_ratio = 0.1, 
      min_degree = 1,
      annotations = list(
           # if not specified, the mode will follow the mode set in `upset()` call (here: `inclusive_intersection`)
           "CES"=(
                ggplot(mapping=aes(y=ces_rating_scale_1_7))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Attractiveness"=(
                ggplot(mapping=aes(y=attractiveness_rating_grading_14772_scale_1_5 ))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Comprehension"=(
                ggplot(mapping=aes(y=comprehension_rating_grading_scale_1_5))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Satisfaction"=(
                ggplot(mapping=aes(y=satisfaction))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           ),
           "Trustworthiness"=(
                ggplot(mapping=aes(y=trustworthiness_rating_grading_96093_scale_1_5))
                + geom_jitter(alpha=0.2, na.rm=TRUE)
                + upset_mode('exclusive_intersection')
           )
      )
) + ggtitle("Upset plot of negative emotions (other scales are in exclusive intersection, minimum at 1)")


# K - means ---------------------------------------------------------------
library(tidymodels)

data_k_emotions <- tibble(dat_feedback[, ..grabber_emotion_rating_all])


set.seed(1234)
multi_kmeans <- tibble(k = 1:10) %>%
     mutate(
          model = purrr::map(k, ~ kmeans(data_k_emotions, centers = .x, nstart = 20)),
          tot.withinss = purrr::map_dbl(model, ~ glance(.x)$tot.withinss)
     )

multi_kmeans

multi_kmeans %>%
     ggplot(aes(k, tot.withinss)) +
     geom_point() +
     geom_line() +
     theme_minimal() + 
     ggtitle("Elbow plot") +
     labs(x = "K", y = "Total withinness")

final_kmeans <- multi_kmeans %>%
     dplyr::filter(k == 5) %>%
     dplyr::pull(model) %>%
     pluck(1)

kmeanspca_raw <- augment(final_kmeans, data = data_k_emotions) %>%
     select(-.cluster) %>%
     prcomp(scale = TRUE)

kmeanspca <- bind_cols(
     augment(kmeanspca_raw),
     augment(final_kmeans, data = data_k_emotions) %>%
          select(.cluster)
)

kmeanspca %>%
     ggplot(aes(.fittedPC1, .fittedPC2, color = .cluster)) +
     geom_point() +
     theme_minimal() +
     labs(x = "PC1 (Stronger on negative emotions)", y = "PC2 (Stronger on neutral emotions)") +
     ggtitle("Exploratory k-means with PCA")
