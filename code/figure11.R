library(tidyverse)

duchcov <- read_csv(here::here("data", "duchcov.csv"))

cluster <- read_csv(here::here("data", "clusters.csv")) %>%
  mutate(kmeans = factor(kmeans))

isotopes <- duchcov %>%
  select(id, starts_with("Pb2"))

isotopes <- bind_cols(isotopes, cluster)

sources <- read_csv(here::here("data", "ore_sources.csv")) %>%
  mutate(Region = factor(Region,
                         levels = c("Valais", "Rheinland", "Saarland",
                                    "Rammelsberg & Harz",
                                    "E Alps", "SE Alps", "Erzgebirge", 
                                    "Slovakia", "E Carpathians")),
         Region2 = factor(Region2,
                          levels = c("Valais", "Rheinland", "Saarland",
                                     "Harz", "Rammelsberg",
                                     "E Alps", "SE Alps", "Erzgebirge", 
                                     "Slovakia", "E Carpathians")))

sources <- sources %>% filter(Outlier == FALSE)

sources_lm <- lm(as.matrix(sources[, c("Pb207_206", "Pb208_206",
                                       "Pb206_204", "Pb207_204", "Pb208_204")]) ~ sources$Region2)

n_levels_sources <- length(levels(sources$Region2))

sources_lda <- MASS::lda(Region2 ~ ., sources[, c("Region2",
                                                  "Pb207_206", "Pb208_206",
                                                  "Pb206_204", "Pb207_204", "Pb208_204")],
                         prior = as.double(
                           paste(
                             rep(1/n_levels_sources,
                                 n_levels_sources), sep = ",")))

sources_pred <- predict(sources_lda)
prediction <- with(sources_pred, data.frame(Region = sources$Region2,
                                            Predict = class, round(posterior, 2)))

duchcov_prediction <- predict(object = sources_lda, newdata = isotopes[, -7])
duchcov_prediction_df <- with(duchcov_prediction,
                              data.frame(id = isotopes[, "id"],
                                         kmeans = isotopes[, "kmeans"],
                                         predicted_region = class,
                                         round(posterior, 2))) %>% 
  as_tibble() %>% 
  mutate(kmeans = str_c("K-means cluster ", kmeans))

fig_out <- duchcov_prediction_df %>%
  select(-predicted_region) %>%
  gather(key = "region", value = "probability", -id, -kmeans) %>%
  group_by(kmeans, region) %>%
  dplyr::summarize(mean_prob = mean(probability, na.rm = TRUE)) %>%
  mutate(region = str_replace(region, "\\.", " ")) %>% 
  ggplot(aes(x = region, y = mean_prob)) +
  geom_col(fill = "white", col = "black") +
  facet_wrap(~kmeans, ncol = 1) +
  labs(x = "Region", y = "Mean posterior probability") +
  theme(axis.text.x = element_text(size = 8, angle = -30, hjust = 0))

ggsave(here::here("plots", "FIG11.pdf"), fig_out, "pdf", width = 140, height = 140, units = "mm", scale = 1)
