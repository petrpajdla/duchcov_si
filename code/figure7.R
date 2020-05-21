library(gridExtra)
library(ggforce)
library(tidyverse)
library(cowplot)

duchcov <- read_csv(here::here("data", "duchcov.csv"))

cluster <- read_csv(here::here("data", "clusters.csv")) %>%
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

# frame margins of further plots
dist_duchcov <- vector(mode = "list")
dist_duchcov$Pb206x$min <-  min(isotopes$Pb206_204, na.rm = TRUE)
dist_duchcov$Pb206x$max <-  max(isotopes$Pb206_204, na.rm = TRUE)
dist_duchcov$Pb207y$min <-  min(isotopes$Pb207_204, na.rm = TRUE)
dist_duchcov$Pb207y$max <-  max(isotopes$Pb207_204, na.rm = TRUE)
dist_duchcov$Pb208_204y$min <-  min(isotopes$Pb208_204, na.rm = TRUE)
dist_duchcov$Pb208_204y$max <-  max(isotopes$Pb208_204, na.rm = TRUE)

dist_duchcov$Pb208y$min <-  min(isotopes$Pb208_206, na.rm = TRUE)
dist_duchcov$Pb208y$max <-  max(isotopes$Pb208_206, na.rm = TRUE)
dist_duchcov$Pb207x$min <-  min(isotopes$Pb207_206, na.rm = TRUE)
dist_duchcov$Pb207x$max <-  max(isotopes$Pb207_206, na.rm = TRUE)

sources <- read_csv(here::here("data", "ore_sources.csv")) %>%
  mutate(Region = factor(Region,
                         levels = c("Valais", "Rheinland", "Saarland",
                                    "Rammelsberg & Harz",
                                    "E Alps", "SE Alps", "Erzgebirge", 
                                    "Slovakia", "E Carpathians")),
         Region2 = factor(Region2,
                         levels = c("Valais", "Rheinland", "Saarland",
                                    "Rammelsberg", "Harz",
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

# Figure 7: Detail of ore sources highlighted using convex hulls overlaid with Duchcov data, 
# highlighting clusters according to K-means. Source data (i.e. ore deposits) according to:

gg_206_207 <- gg_206_207 + 
  coord_cartesian(xlim = c(dist_duchcov$Pb206x$min, dist_duchcov$Pb206x$max),
                  ylim = c(dist_duchcov$Pb207y$min, dist_duchcov$Pb207y$max)) +
  # theme(legend.position = c(0.8, 0.1)) +
  guides(shape = guide_legend(ncol=1), color = guide_legend(ncol = 1)) +
  labs(shape = "K-means cluster", title = lab_A, color = "Region", fill = "Region")

gg_206_208 <- gg_206_208 + 
  coord_cartesian(xlim = c(dist_duchcov$Pb206x$min, dist_duchcov$Pb206x$max),
                  ylim = c(dist_duchcov$Pb208_204y$min, dist_duchcov$Pb208_204y$max)) +
  # theme(legend.position = c(0.8, 0.1)) +
  guides(shape = guide_legend(ncol=1), color = guide_legend(ncol = 1)) +
  labs(shape = "K-means cluster", title = lab_B, color = "Region", fill = "Region")

gg_207_208 <- gg_207_208 + 
  coord_cartesian(xlim = c(dist_duchcov$Pb207x$min, dist_duchcov$Pb207x$max),
                  ylim = c(dist_duchcov$Pb208y$min, dist_duchcov$Pb208y$max)) +
  # theme(legend.position = c(0.8, 0.1)) +
  guides(shape = guide_legend(ncol=1), color = guide_legend(ncol = 1)) +
  labs(shape = "K-means cluster", title = lab_C, color = "Region", fill = "Region")

# plots
f7A <- gg_206_207 +
  geom_point(alpha = 0.2, aes(color = Region2), show.legend = TRUE) +
  geom_mark_hull(aes(fill = Region2, color = Region2), expand = unit(2.4, "mm"),
                 alpha = 0.1, show.legend = FALSE) +
  scale_color_viridis_d(direction = -1, end = 0.9) + scale_fill_viridis_d(direction = -1) +
  geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.6) +
  facet_wrap(~Region, ncol = 3)

f7B <- gg_206_208 +
  geom_point(alpha = 0.2, aes(color = Region2), show.legend = TRUE) +
  geom_mark_hull(aes(fill = Region2, color = Region2), expand = unit(2.4, "mm"),
                 alpha = 0.1, show.legend = FALSE) +
  scale_color_viridis_d(direction = -1, end = 0.9) + scale_fill_viridis_d(direction = -1) +
  geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.6) +
  facet_wrap(~Region, ncol = 3)

f7C <- gg_207_208 +
  geom_point(alpha = 0.2, aes(color = Region2), show.legend = TRUE) +
  geom_mark_hull(aes(fill = Region2, color = Region2), expand = unit(2.4, "mm"),
                 alpha = 0.1, show.legend = FALSE) +
  scale_color_viridis_d(direction = -1, end = 0.9) + scale_fill_viridis_d(direction = -1) +
  geom_point(data = isotopes, aes(shape = kmeans), alpha = 0.6) +
  facet_wrap(~Region, ncol = 3)

f7ab <- grid.arrange(f7A, f7B,nrow = 2)

ggsave(here::here("plots", "FIG7_1.pdf"), plot = f7ab, device = "pdf", width = 190, height = 240, units = "mm", scale = 1)
ggsave(here::here("plots", "FIG7_2.pdf"), plot = f7C, device = "pdf", width = 190, height = 120, units = "mm", scale = 1)
