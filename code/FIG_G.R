library(gridExtra)
library(tidyverse)


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(legend.background = element_rect(fill = "white", color = "gray90"),
                       panel.border = element_rect(fill = NA, color = "gray90"))


# data --------------------------------------------------------------------

burials_plot <- read_rds(here::here("data/temp", "burials_clusters.RDS"))

duchcov <- read_rds(here::here("data/temp", "isotopes.RDS"))


# labels ------------------------------------------------------------------

lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")), title = lab_A)
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")), title = lab_B)


# plots -------------------------------------------------------------------

b1 <- burials_plot %>% 
  ggplot(aes(Pb206_204, Pb207_204)) +
  stat_density2d(aes(color = kmeans, alpha = ..level..), 
                 size = 0.6, h = c(0.18, 0.05), show.legend = FALSE) +
  geom_point(aes(color = kmeans), alpha = 0.6, size = 1.8) +
  scale_color_brewer(aesthetics = c("color", "fill"), palette = "Set1", 
                     name = "Burials\ngroup") +
  geom_point(data = duchcov, aes(shape = kmeans), fill = "white", size = 1.8, 
             show.legend = FALSE) + 
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  labs(shape = "Duchcov\nK-means\ncluster", title = lab_A) + 
  lab_206_207 +
  theme_borders +
  theme(legend.position = c(0.98, 0.02), legend.justification = c("right", "bottom"))

b2 <- burials_plot %>% 
  ggplot(aes(Pb206_204, Pb208_204)) +
  stat_density2d(aes(color = kmeans, alpha = ..level..), 
                 size = 0.6, h = c(0.14, 0.2), show.legend = FALSE) +
  geom_point(aes(color = kmeans), alpha = 0.6, size = 1.8, show.legend = FALSE) +
  scale_color_brewer(aesthetics = c("color", "fill"), palette = "Set1", 
                     name = "Burials\ngroup") +
  geom_point(data = duchcov, aes(shape = kmeans), fill = "white", size = 1.8) + 
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  labs(shape = "Duchcov\nK-means\ncluster", title = lab_B) + 
  lab_206_208 +
  theme_borders +
  theme(legend.position = c(0.98, 0.02), legend.justification = c("right", "bottom")) +
  guides(shape = guide_legend(ncol = 2))

f11 <- grid.arrange(b1, b2, nrow = 2)


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_G.pdf"), 
       plot = f11, width = 140, height = 140, units = "mm")

ggsave(here::here("plots", "FIG_G.png"), 
       plot = f11, width = 140, height = 160, units = "mm")
