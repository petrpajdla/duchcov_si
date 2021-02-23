library(tidyverse)
library(ggforce)
library(gridExtra)


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(panel.border = element_rect(fill = NA, color = "gray90"))


# data --------------------------------------------------------------------

pc_comp <- read_rds(here::here("data/temp", "pc_comp.RDS"))
cluster <- read_csv(here::here("data/temp", "clusters.csv")) %>% 
  mutate(kmeans = factor(kmeans))


# plot --------------------------------------------------------------------

# Figure: Biplot of the PC1 and PC2 showing significant elements (A); K-means clusters (1-4) in PCA space.  
f5A <- ggbiplot::ggbiplot(pc_comp, scale = 0, alpha = .1, varname.adjust = 4, varname.size = 3) + 
  labs(title = "(A)") + # Biplot of the PC1 and PC2
  theme_borders

f5B <- ggplot(as_tibble(pc_comp$x), aes(PC1, PC2, fill = cluster$kmeans)) +
  geom_mark_hull(aes(color = cluster$kmeans), expand = unit(2.4, "mm")) +
  geom_point(aes(shape = cluster$kmeans, color = cluster$kmeans)) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  labs(fill = "K-means\ncluster", color = "K-means\ncluster", shape = "K-means\ncluster",
       title = "(B)") +  #  K-means clusters in PC1 to PC2 space
  theme_borders
  # theme(legend.position = c(0.1, 0.24))

# pdf("./plots/fig5.pdf", width = 10, height = 5)
f5 <- grid.arrange(f5A, f5B, ncol = 2)
# dev.off()


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_A.pdf"), 
       plot = f5, width = 190, height = 80, units = "mm", scale = 1)

ggsave(here::here("plots", "FIG_A.png"), 
       plot = f5, width = 190, height = 80, units = "mm", scale = 1)
