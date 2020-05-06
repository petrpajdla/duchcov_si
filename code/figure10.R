library(gridExtra)
library(ggforce)
library(tidyverse)
library(cowplot)

duchcov <- read_csv("./data/duchcov.csv")

burials <- read_csv("./data/burial_grounds.csv")

duchcov$cluster_km <- read_csv("./data/clusters.csv") %>%
  mutate(kmeans = factor(kmeans)) %>%
  dplyr::pull()

duchcov <- duchcov %>% mutate(brooch = if_else(str_detect(type_eng, "^brooch_"),
                                               "brooch", "not brooch"),
                              brooch = factor(brooch, levels = c("brooch", "not brooch")))

burials <- burials %>% mutate(brooch = if_else(str_detect(type_eng, "brooch_"),
                                               "brooch", "not brooch"),
                              brooch = factor(brooch, levels = c("brooch", "not brooch")))

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

# Figure 10: Density distribution of burial finds and the Duchcov hoard. 
origin <- bind_rows(duchcov = duchcov[, c("Pb207_204", "Pb206_204", 
                                          "Pb208_204", "Pb208_206", 
                                          "Pb207_206", "brooch")],
                    burials = burials[, c("Pb207_204", "Pb206_204", 
                                          "Pb208_204", "Pb208_206", 
                                          "Pb207_206", "brooch")],
                    .id = "origin")

# 1

pmain206_207 <- ggplot(origin, aes(Pb206_204, Pb207_204, color = origin)) +
  stat_density2d(show.legend = FALSE, alpha = 0.4) +
  geom_point(aes(shape = origin), size = 2, alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Origin", shape = "Origin") + lab_206_207 + theme(legend.position = "none")

ydens206_207 <- axis_canvas(pmain206_207, axis = "y", coord_flip = TRUE) +
  geom_density(data = origin, aes(x = Pb207_204, fill = origin),
               alpha = 0.2, size = 0.2)+
  coord_flip() +
  scale_fill_brewer(palette = "Set1")

xdens206_207 <- axis_canvas(pmain206_207, axis = "x", coord_flip = FALSE) +
  geom_density(data = origin, aes(x = Pb206_204, fill = origin),
               alpha = 0.2, size = 0.2) +
  scale_fill_brewer(palette = "Set1")

pfinal206_207 <- insert_yaxis_grob(pmain206_207, ydens206_207, 
                                   grid::unit(.2, "null"), position = "right")
pfinal206_207 <- insert_xaxis_grob(pfinal206_207, xdens206_207, 
                                   grid::unit(.2, "null"), position = "top")

f10A <- ggdraw(pfinal206_207)

# 2
pmain206_208 <- ggplot(origin, aes(Pb206_204, Pb208_204, color = origin)) +
  stat_density2d(show.legend = FALSE, alpha = 0.4) +
  geom_point(aes(shape = origin), size = 2, alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Origin", shape = "Origin") + lab_206_208  + theme(legend.position = "none")

ydens206_208 <- axis_canvas(pmain206_208, axis = "y", coord_flip = TRUE) +
  geom_density(data = origin, aes(x = Pb208_204, fill = origin),
               alpha = 0.2, size = 0.2)+
  coord_flip() +
  scale_fill_brewer(palette = "Set1")

xdens206_208 <- axis_canvas(pmain206_208, axis = "x", coord_flip = FALSE) +
  geom_density(data = origin, aes(x = Pb206_204, fill = origin),
               alpha = 0.2, size = 0.2) +
  scale_fill_brewer(palette = "Set1")

pfinal206_208 <- insert_yaxis_grob(pmain206_208, ydens206_208, 
                                   grid::unit(.2, "null"), position = "right")
pfinal206_208 <- insert_xaxis_grob(pfinal206_208, xdens206_208, 
                                   grid::unit(.2, "null"), position = "top")

f10B <- ggdraw(pfinal206_208)

# 3
pmain207_208 <- ggplot(origin, aes(Pb207_206, Pb208_206, color = origin)) +
  stat_density2d(show.legend = FALSE, alpha = 0.4) +
  geom_point(aes(shape = origin), size = 2, alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Origin", shape = "Origin") + lab_207_208 + theme(legend.position = c(0.8, 0.24))

ydens207_208 <- axis_canvas(pmain207_208, axis = "y", coord_flip = TRUE) +
  geom_density(data = origin, aes(x = Pb208_206, fill = origin),
               alpha = 0.2, size = 0.2)+
  coord_flip() +
  scale_fill_brewer(palette = "Set1")

xdens207_208 <- axis_canvas(pmain207_208, axis = "x", coord_flip = FALSE) +
  geom_density(data = origin, aes(x = Pb207_206, fill = origin),
               alpha = 0.2, size = 0.2) +
  scale_fill_brewer(palette = "Set1")

pfinal207_208 <- insert_yaxis_grob(pmain207_208, ydens207_208, 
                                   grid::unit(.2, "null"), position = "right")
pfinal207_208 <- insert_xaxis_grob(pfinal207_208, xdens207_208, 
                                   grid::unit(.2, "null"), position = "top")

f10C <- ggdraw(pfinal207_208)

# pdf("./plots/final/fig10.pdf", width = 15, height = 4)
f10 <- grid.arrange(f10A, f10B, f10C, ncol = 1)
# dev.off()

ggsave("./plots/FIG10.pdf", plot = f10, device = "pdf", width = 90, height = 240, units = "mm")
