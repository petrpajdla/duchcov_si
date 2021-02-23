library(tidyverse)
library(gridExtra)


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(panel.border = element_rect(fill = NA, color = "gray90"))


# data --------------------------------------------------------------------

burials <- read_rds(here::here("data/temp", "burials.RDS"))

duchcov <- read_rds(here::here("data/temp", "isotopes.RDS"))


# labels ------------------------------------------------------------------

lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))
# lab_C <- expression(paste("(C)"))

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")), title = lab_A)
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")), title = lab_B)
# lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
#                     y = expression(paste(""^208, "Pb/", ""^206, "Pb")), title = lab_C)


# plots -------------------------------------------------------------------

g_bur_206_207 <- ggplot(burials, aes(Pb206_204, Pb207_204)) +
  labs(color = "Burials", shape = "Duchcov K-means\ncluster") +
  lab_206_207 +
  theme(legend.position = c(0.98, -0.2), legend.justification = c("right", "bottom")) +
  guides(shape = guide_legend(ncol = 2), color = guide_legend(ncol = 2)) +
  theme_borders

g_bur_206_208 <- ggplot(burials, aes(Pb206_204, Pb208_204)) +
  labs(color = "Burials", shape = "Duchcov\nK-means\ncluster") +
  lab_206_208 +
  theme(legend.position = c(0.98, -0.2), legend.justification = c("right", "bottom")) +
  guides(shape = guide_legend(ncol = 2), color = guide_legend(ncol = 2)) +
  theme_borders

# g_bur_207_208 <- ggplot(burials, aes(Pb207_206, Pb208_206)) +
#   labs(color = "Burials", shape = "Duchcov K-means\ncluster") +
#   lab_207_208 +
#   theme(legend.position = c(0.84, 0.2)) +
#   guides(shape = guide_legend(ncol = 2), color = guide_legend(ncol = 2)) +
#   theme_borders

f9A <- g_bur_206_207 +
  # stat_density2d(aes(fill = stat(nlevel)), geom = "polygon", alpha = 0.2,
  #                h = c(0.3, 0.075), show.legend = FALSE) +
  # scale_fill_gradient(low = "white", high = "gray10") +
  stat_density2d(aes(color = site, alpha = ..level..), h = c(0.3, 0.075), 
                 show.legend = FALSE) + 
  scale_color_brewer(palette = "Set1") +
  geom_point(aes(color = site), shape = 4, size = 0.6,
             alpha = 0.6, show.legend = FALSE) +
  geom_point(data = duchcov, aes(shape = kmeans), size = 0.8, fill = "white", 
             show.legend = FALSE) +
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  facet_wrap(~site)

f9B <- g_bur_206_208 +
  # stat_density2d(aes(fill = stat(nlevel)), geom = "polygon", alpha = 0.2, 
  #                h = c(0.4, 0.4), show.legend = FALSE) +
  # scale_fill_gradient(low = "white", high = "gray10") +
  stat_density2d(aes(color = site, alpha = ..level..), h = c(0.4, 0.4), 
                 show.legend = FALSE) + 
  scale_color_brewer(palette = "Set1") +
  geom_point(aes(color = site), shape = 4, size = 0.6,
             alpha = 0.6, show.legend = FALSE) +
  geom_point(data = duchcov, aes(shape = kmeans), fill = "white", size = 0.8) +
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  facet_wrap(~site)

# f9C <- g_bur_207_208 +
#   stat_density_2d_filled(aes(fill = stat(nlevel)), geom = "polygon",
#                  alpha = 0.2, show.legend = FALSE) +
#   scale_fill_gradient(low = "white", high = "black") +
#   scale_color_brewer(palette = "Set1") +
#   geom_point(aes(color = site), alpha = 0.8, show.legend = FALSE) +
#   geom_point(data = duchcov, aes(shape = kmeans), size = 1.4, alpha = 0.4) +
#   facet_wrap(~site)


# pdf("./plots/fig9.pdf", width = 7, height = 14)
f9 <- grid.arrange(f9A, f9B, nrow = 2)
# dev.off()


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_F.pdf"), 
       plot = f9,
       width = 140, height = 160, units = "mm", scale = 1)

ggsave(here::here("plots", "FIG_F.png"), 
       plot = f9,
       width = 140, height = 160, units = "mm", scale = 1)

