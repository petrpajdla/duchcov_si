library(tidyverse)

# data on Duchcov hoard
# elements selected for further analysis
elements <- c("Co", "Ni", "Zn", "As", "Ag", "Sb", "Pb")

duchcov <- read_csv(here::here("data", "duchcov.csv"))

cluster_d <- read_csv(here::here("data", "clusters.csv")) %>%
  mutate(kmeans = factor(kmeans))

isotopes <- duchcov %>%
  select(id, starts_with("Pb2"))
isotopes <- bind_cols(isotopes, cluster_d)

# data on burials
burials <- read_csv(here::here("data", "burial_grounds.csv"))

composition_burials <- read_csv(here::here("data", "burial_grounds_compositions.csv")) %>% 
  filter(site != "Duchcov")

# pca, scaling, kmeans
composition_burials <- scale(composition_burials[, elements])
pc_comp <- prcomp(composition_burials)

cluster_b <- tibble(kmeans = kmeans(pc_comp$x[, 1:5], centers = 4, nstart = 50)$cluster) %>% 
  mutate(kmeans = as_factor(kmeans))

# labs
lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")))
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")))
lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^206, "Pb")))

# plot
rename_clusters <- function(cluster_b) {
  cluster_b_tab <- cluster_b %>% count(kmeans) %>% 
    mutate(group = if_else(n == 43, "B",
                           if_else(n == 44, "A", NA_character_))) %>% 
    select(kmeans, group)
  cluster_b_lookup <- cluster_b_tab$group
  names(cluster_b_lookup) <- cluster_b_tab$kmeans
  cluster_b_out <- cluster_b %>% 
    filter(kmeans %in% cluster_b_tab$kmeans) %>% 
    mutate(kmeans = unname(cluster_b_lookup[as.character(kmeans)]))
  return(cluster_b_out)
}

burials_plot <- bind_cols(burials, rename_clusters(cluster_b)) %>% 
  filter(!is.na(kmeans))

bur1 <- burials_plot %>% 
  ggplot(aes(Pb206_204, Pb207_204)) +
  stat_density2d(aes(color = kmeans, alpha = ..level..), 
                 size = 0.6, show.legend = FALSE) +
  geom_point(aes(color = kmeans), alpha = 0.6, size = 1.8) +
  scale_color_brewer(aesthetics = c("color", "fill"), palette = "Set1", name = "Burials\ngroup") +
  geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.8, size = 1.8, show.legend = FALSE) + 
  labs(shape = "Duchcov\nK-means\ncluster", title = "(A)") + lab_206_207 +
  theme_gray()

# burials_plot %>% 
#   ggplot(aes(Pb206_204, Pb207_204)) +
#   stat_ellipse(aes(color = kmeans), level = 0.65) +
#   geom_point(aes(color = kmeans), alpha = 0.6, size = 2) +
#   scale_color_brewer(aesthetics = c("color", "fill"), palette = "Set1", name = "Burials\ngroup") +
#   geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.8, size = 2) + 
#   labs(shape = "Duchcov\nK-means\ncluster") + lab_206_207

bur2 <- burials_plot %>% 
  ggplot(aes(Pb206_204, Pb208_204)) +
  stat_density2d(aes(color = kmeans, alpha = ..level..), 
                 size = 0.6, show.legend = FALSE) +
  geom_point(aes(color = kmeans), alpha = 0.6, size = 1.8, show.legend = FALSE) +
  scale_color_brewer(aesthetics = c("color", "fill"), palette = "Set1", name = "Burials\ngroup") +
  geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.8, size = 1.8) + 
  labs(shape = "Duchcov\nK-means\ncluster", title = "(B)") + lab_206_208 +
  theme_gray()

# tweaking the legend position
# cowplot
legend1 <- cowplot::get_legend(bur1)
legend2 <- cowplot::get_legend(bur2)
bur1 <- bur1 + theme(legend.position="none")
bur2 <- bur2 + theme(legend.position="none")
# 4. Create a blank plot
# blankPlot <- ggplot() +
#   geom_blank(aes(1,1)) + 
#   cowplot::theme_nothing()

fX <- gridExtra::grid.arrange(bur1, legend1, bur2, legend2, 
                              nrow = 2, ncol = 2, 
                              widths = c(0.8, 0.2))
  
ggsave(here::here("plots", "FIG10.pdf"), plot = fX, width = 190, height = 140, units = "mm")
