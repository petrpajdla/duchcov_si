library(gridExtra)
library(ggforce)
library(tidyverse)
library(cowplot)

duchcov <- read_csv(here::here("data", "duchcov.csv"))

burials <- read_csv(here::here("data", "burial_grounds.csv"))

duchcov$cluster_km <- read_csv(here::here("data", "clusters.csv")) %>%
  mutate(kmeans = factor(kmeans)) %>%
  dplyr::pull()

duchcov <- duchcov %>% mutate(brooch = if_else(str_detect(type_eng, "^brooch_"),
                                               "brooch", "not brooch"),
                              brooch = factor(brooch, levels = c("brooch", "not brooch")))

burials <- burials %>% mutate(brooch = if_else(str_detect(type_eng, "brooch_"),
                                               "brooch", "not brooch"),
                              brooch = factor(brooch, levels = c("brooch", "not brooch")))

# Figure 9: KDE of data from different burials overlaid with Duchcov hoard (K-means clusters highlighted).

# lab_A <- expression(paste("(A) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^207, "Pb/", ""^204, "Pb"))
# lab_B <- expression(paste("(B) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^208, "Pb/", ""^204, "Pb"))
# lab_C <- expression(paste("(C) Isospace "^207, "Pb/", ""^206, "Pb to ", ""^208, "Pb/", ""^206, "Pb"))
lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))
lab_C <- expression(paste("(C)"))

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")), title = lab_A)
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")), title = lab_B)
lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^206, "Pb")), title = lab_C)

g_bur_206_207 <- ggplot(burials, aes(Pb206_204, Pb207_204)) +
  labs(color = "Burials", shape = "Duchcov k-means\ncluster") +
  lab_206_207 +
  theme_light() +
  theme(legend.position = "none") +
  guides(shape = guide_legend(ncol=2), color = guide_legend(ncol = 2))

g_bur_206_208 <- ggplot(burials, aes(Pb206_204, Pb208_204)) +
  labs(color = "Burials", shape = "Duchcov k-means\ncluster") +
  lab_206_208 +
  theme_light() +
  theme(legend.position = c(0.84, 0.2)) +
  guides(shape = guide_legend(ncol=2), color = guide_legend(ncol = 2))

# g_bur_207_208 <- ggplot(burials, aes(Pb207_206, Pb208_206)) +
#   labs(color = "Burials", shape = "Duchcov k-means\ncluster") +
#   lab_207_208 +
#   theme_light() +
#   theme(legend.position = c(0.84, 0.2)) +
#   guides(shape = guide_legend(ncol=2), color = guide_legend(ncol = 2))

f9A <- g_bur_206_207 +
  stat_density2d(aes(fill = stat(nlevel)), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_color_brewer(palette = "Set1") +
  geom_point(aes(color = site), alpha = 0.8, show.legend = FALSE) +
  geom_point(data = duchcov, aes(shape = cluster_km), size = 1.4, alpha = 0.4) +
  facet_wrap(~site)

f9B <- g_bur_206_208 +
  stat_density2d(aes(fill = stat(nlevel)), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_color_brewer(palette = "Set1") +
  geom_point(aes(color = site), alpha = 0.8, show.legend = FALSE) +
  geom_point(data = duchcov, aes(shape = cluster_km), size = 1.4, alpha = 0.4) +
  facet_wrap(~site)

# f9C <- g_bur_207_208 +
#   stat_density2d(aes(fill = stat(nlevel)), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
#   scale_fill_gradient(low = "white", high = "black") +
#   scale_color_brewer(palette = "Set1") +
#   geom_point(aes(color = site), alpha = 0.8, show.legend = FALSE) +
#   geom_point(data = duchcov, aes(shape = cluster_km), size = 1.4, alpha = 0.4) +
#   facet_wrap(~site)

# pdf("./plots/final/fig9.pdf", width = 7, height = 14)
# f9 <- grid.arrange(f9A, f9B, f9C, nrow = 3)
f9 <- grid.arrange(f9A, f9B, nrow = 2)
# dev.off()

# ggsave(here::here("plots", "FIG9.pdf"), plot = f9, device = "pdf", width = 190, height = 140, units = "mm", scale = 1)
ggsave(here::here("plots", "FIG9.pdf"), plot = f9, device = "pdf", width = 190, height = 240, units = "mm", scale = 1)
