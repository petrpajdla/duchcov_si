library(tidyverse)
library(gridExtra)


# theme & data ------------------------------------------------------------

theme_set(theme_minimal())

distances <- read_rds(here::here("data/temp", "distances.RDS"))


# plots -------------------------------------------------------------------

# Figure: Euclidian (A) and Mahalanobis (B) distances between Duchcov K-means clusters and ore deposits in wider central Europe.
f8A <- ggplot(distances$euclidean, aes(y = region, x = value, fill = region)) +
  ggridges::geom_density_ridges(alpha = 0.4, show.legend = FALSE, scale = 1.1) +
  scale_fill_viridis_d(direction = -1, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  facet_wrap(~kmeans, scales = "fixed") +
  labs(y = "Region", x = "ED (x axis limited to interval 0 - 1)",
       title = "(A)")

mh_max <- max(distances$mahalanobis$value)

f8B <- distances$mahalanobis %>% 
  mutate(value = value / mh_max) %>% 
  ggplot(aes(y = region, x = value, fill = region)) +
  ggridges::geom_density_ridges(alpha = 0.4, show.legend = FALSE, scale = 1.1) +
  scale_fill_viridis_d(direction = -1, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  facet_wrap(~kmeans, scales = "fixed") +
  labs(y = "Region", x = "MD (x axis limited to interval 0 - 1)",
       title = "(B)")

# pdf("./plots/fig8.pdf", width = 14, height = 7)
f8 <- grid.arrange(f8A, f8B, ncol = 1)
# dev.off()


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_D.pdf"), 
       plot = f8,
       width = 140, height = 200, 
       units = "mm", scale = 1)

ggsave(here::here("plots", "FIG_D.png"), 
       plot = f8,
       width = 140, height = 200, 
       units = "mm", scale = 1)
