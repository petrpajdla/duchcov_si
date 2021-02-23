library(tidyverse)


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(panel.border = element_rect(color = "gray90", 
                                                   fill = NA))

# data --------------------------------------------------------------------

predictions <- read_rds(here::here("data/temp", "predictions.RDS"))


# plot --------------------------------------------------------------------

predictions %>%
  select(-predicted_region) %>%
  gather(key = "region", value = "probability", -kmeans) %>%
  group_by(kmeans, region) %>%
  dplyr::summarize(mean_prob = mean(probability, na.rm = TRUE)) %>%
  mutate(kmeans = fct_relabel(kmeans, ~ paste("K-means cluster", .)),
         region = str_replace(region, "\\.\\.", ", "),
         region = str_replace(region, "\\.", " ")
         ) %>% 
  ggplot(aes(x = region, y = mean_prob)) +
  geom_col(fill = "white", col = "black") +
  facet_wrap(~kmeans, ncol = 1) +
  labs(x = "Region", y = "Mean posterior probability") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  theme_borders


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_E.pdf"), 
       width = 140, height = 120, units = "mm", scale = 1)

ggsave(here::here("plots", "FIG_E.png"), 
       width = 140, height = 120, units = "mm", scale = 1)
