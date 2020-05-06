library(gridExtra)
library(ggforce)
library(tidyverse)
library(cowplot)

duchcov <- read_csv("./data/duchcov.csv")

composition <- duchcov %>%
  select(Li:Bi) %>%
  as.matrix()
rownames(composition) <- duchcov$id

elements <- c("Co", "Ni", "Zn", "As", "Ag", "Sb", "Pb")

cluster <- read_csv("./data/clusters.csv") %>%
  mutate(kmeans = factor(kmeans))

# values are scaled to mean 0 and sd 1
composition_biplot <- scale(composition[, elements])
# solve overplotting in variable names on a biplot
colnames(composition_biplot)[4] <- "\nAs"
colnames(composition_biplot)[2] <- "\nNi"

pc_comp <- prcomp(composition_biplot)

# Figure 5: Biplot of the PC1 and PC2 showing significant elements (A); K-means clusters (1-4) in PCA space.  
f5A <- ggbiplot::ggbiplot(pc_comp, scale = 0, alpha = .1, varname.adjust = 4, varname.size = 3) + 
  labs(title = "(A)")

f5B <- ggplot(as_tibble(pc_comp$x), aes(PC1, PC2, fill = cluster$kmeans)) +
  geom_mark_hull(aes(color = cluster$kmeans), expand = unit(2.4, "mm")) +
  geom_point(aes(shape = cluster$kmeans, color = cluster$kmeans)) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  labs(fill = "k-means\ncluster", color = "k-means\ncluster", shape = "k-means\ncluster",
       title = "(B)")
  # theme(legend.position = c(0.1, 0.24))

f5 <- grid.arrange(f5A, f5B, ncol = 2)

ggsave("./plots/FIG5.pdf", plot = f5, device = "pdf", width = 190, height = 80, units = "mm", scale = 1)
