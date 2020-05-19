library(gridExtra)
library(ggforce)
library(tidyverse)
library(cowplot)

duchcov <- read_csv("./data/duchcov.csv")

cluster <- read_csv("./data/clusters.csv") %>%
  mutate(kmeans = factor(kmeans))

isotopes <- duchcov %>%
  select(id, starts_with("Pb2"))

isotopes <- bind_cols(isotopes, cluster)

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")))
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")))
lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^206, "Pb")))

sources <- read_csv("./data/ore_sources.csv") %>%
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

gg_206_207 <- ggplot(sources, aes(Pb206_204, Pb207_204)) +
  lab_206_207

gg_206_208 <- ggplot(sources, aes(Pb206_204, Pb208_204)) +
  lab_206_208

gg_207_208 <- ggplot(sources, aes(Pb207_206, Pb208_206)) +
  lab_207_208

# lab_A <- expression(paste("(A) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^207, "Pb/", ""^204, "Pb"))
# lab_B <- expression(paste("(B) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^208, "Pb/", ""^204, "Pb"))
# lab_C <- expression(paste("(C) Isospace "^207, "Pb/", ""^206, "Pb to ", ""^208, "Pb/", ""^206, "Pb"))
lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))
lab_C <- expression(paste("(C)"))

# euclidean and mahalanobis distances
nest_matrix <- function(df, gr) {
  output <- df %>% group_by(!!sym(gr)) %>%
    nest() %>% 
    mutate(mx = map(data, as.matrix))
  return(output)
}

# function to get distance between point clouds
point_cloud_distance <- function(origin, goal, group_origin, group_goal, method) {
  # create nested matrices
  x <- nest_matrix(origin, group_origin)
  y <- nest_matrix(goal, group_goal)
  lx <- nrow(x)
  ly <- nrow(y)
  # create empty list for results
  distance <- vector(mode = "list", length = ly)
  for (k in seq_along(distance)) {
    distance[[k]] <- vector(mode = "list", length = lx)
    names(distance[[k]]) <- x %>% pull(!!sym(group_origin))
  }
  names(distance) <- y %>% pull(!!sym(group_goal)) %>% str_c("K-means cluster ", .)
  # count euclidean distances
  if (method == "euclidean") {
    for (i in 1:ly) {
      for (j in 1:lx) {
        distance[[i]][[j]] <- pdist::pdist(X = y$mx[[i]], Y = x$mx[[j]])@dist
      }
      distance[[i]] <- bind_rows(lapply(distance[[i]], as_tibble), .id = "region")
    }
    # mahalanobis distance
  } else if (method == "mahalanobis") {
    for (i in 1:ly) {
      for (j in 1:lx) {
        distance[[i]][[j]] <- StatMatch::mahalanobis.dist(data.x = y$mx[[i]],
                                                          data.y = x$mx[[j]])
      }
      distance[[i]] <- bind_rows(
        lapply(lapply(distance[[i]], as.vector), as_tibble), .id = "region")
    }
  }
  distance <- bind_rows(distance, .id = "kmeans")
  return(distance)
}

src <- sources %>% select(Region2, starts_with("Pb"))
iso <- isotopes %>% select(kmeans, starts_with("Pb")) %>% na.omit()

euclidean_distance <- point_cloud_distance(origin = src, goal = iso, 
                                           group_origin = "Region2", group_goal = "kmeans",
                                           method = "euclidean") %>% 
  mutate(region = fct_relevel(region, levels(sources$Region2)))

mahalanobis_distance <- point_cloud_distance(origin = src, goal = iso, 
                                             group_origin = "Region2", group_goal = "kmeans",
                                             method = "mahalanobis") %>% 
  mutate(region = fct_relevel(region, levels(sources$Region2)))

ed_sum <- euclidean_distance %>% group_by(kmeans, region) %>% dplyr::summarize(m = mean(value))
md_sum <- mahalanobis_distance %>% group_by(kmeans, region) %>% dplyr::summarize(m = mean(value))

# plot =========================================================================
# Figure 8: Euclidian (A) and Mahalanobis (B) distances between Duchcov K-means clusters and ore deposits in wider central Europe.
f8A <- ggplot(euclidean_distance, aes(y = region, x = value, fill = region)) +
  ggridges::geom_density_ridges(alpha = 0.4, show.legend = FALSE, scale = 1.1) +
  scale_fill_viridis_d(direction = -1, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  facet_wrap(~kmeans, scales = "fixed") +
  labs(y = "Region", x = "ED (x axis limited to interval 0 - 1)",
       title = "(A) Euclidean distance") +
  geom_point(data = ed_sum, aes(y = region, x = m), shape = 4, size = 1,
             show.legend = FALSE)

f8B <- ggplot(mahalanobis_distance, aes(y = region, x = value, fill = region)) +
  ggridges::geom_density_ridges(alpha = 0.4, show.legend = FALSE, scale = 1.1) +
  scale_fill_viridis_d(direction = -1, end = 0.9) +
  coord_cartesian(xlim = c(0, 10)) +
  facet_wrap(~kmeans, scales = "fixed") +
  labs(y = "Region", x = "MD (x axis limited to interval 0 - 10)",
       title = "(B) Mahalanobis distance") +
  geom_point(data = md_sum, aes(y = region, x = m), shape = 4, size = 1,
             show.legend = FALSE)

# f8A <- ggplot(distances_km, aes(reorder(region, value, mean), value, color = region)) +
#   geom_boxplot(show.legend = FALSE) +
#   scale_color_viridis_d(direction = -1, end = 0.9) +
#   coord_flip(ylim = c(0, 1)) +
#   facet_wrap(~kmeans, scales = "fixed") +
#   labs(x = "Region", y = "Euclidean distance (x axis limited to interval 0 - 1.0)", title = "(A) Euclidean distance")
# 
# f8B <- ggplot(mahal_km, aes(reorder(region, value, median), value/max(value), color = region)) +
#   geom_boxplot(show.legend = FALSE) +
#   scale_color_viridis_d(direction = -1, end = 0.9) +
#   coord_flip(ylim = c(0, 0.1)) +
#   facet_wrap(~kmeans, scales = "fixed") +
#   labs(x = "Region", y = "Mahalanobis distance (x axis limited to interval 0 - 0.1)", title = "(B) Mahalanobis distance")

# pdf("./plots/final/fig8.pdf", width = 14, height = 7)
f8 <- grid.arrange(f8A, f8B, ncol = 1)
# dev.off()

ggsave("./plots/FIG8.pdf", plot = f8, device = "pdf", width = 140, height = 240, units = "mm", scale = 1)

