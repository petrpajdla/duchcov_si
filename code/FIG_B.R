library(tidyverse)
library(gridExtra)
library(ggforce)


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(legend.background = element_rect(color = "gray90"), 
                       panel.border = element_rect(fill = NA, color = "gray90"))

isotopes <- read_rds(here::here("data/temp", "isotopes.RDS")) %>%
  bind_cols(select(read_rds(here::here("data/analytical", "duchcov.RDS"))$meta, type))


# labels ------------------------------------------------------------------

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")))
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")))
lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^206, "Pb")))

# Figure: Overview of LI ratios of the assemblage, highlighting individual clusters (1 – 4) 
# according to K-means. Error margins are about the size of the symbols in the plots with 204Pb in the denominator.

lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))
lab_C <- expression(paste("(C)"))


# plots -------------------------------------------------------------------

f6A <- ggplot(isotopes, aes(Pb206_204, Pb207_204)) +
  geom_mark_hull(aes(label = type, 
                     filter = type == "cauldron" | type == "rivet"), 
                 label.fill = NA, label.fontsize = 8, con.cap = 0, 
                 expand = unit(2.2, "mm")) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_A) + lab_206_207 +
  theme(legend.position = "none") +
  theme_borders

f6B <- ggplot(isotopes, aes(Pb206_204, Pb208_204)) +
  geom_mark_hull(aes(label = type, 
                     filter = type == "cauldron" | type == "rivet"),
                 label.fill = NA, label.fontsize = 8, con.cap = 0, 
                 expand = unit(2.2, "mm"), radius = unit(2.2, "mm")) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_B) + lab_206_208 +
  theme(legend.position = "none") +
  theme_borders

f6C <- ggplot(isotopes, aes(Pb207_206, Pb208_206)) +
  geom_mark_hull(aes(label = type, 
                     filter = type == "cauldron" | type == "rivet"),
                 label.fill = NA, label.fontsize = 8, con.cap = 0, 
                 expand = unit(2.4, "mm"), radius = unit(2.2, "mm")) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_C) + lab_207_208 +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(0.8, 0.24)) +
  theme_borders

# pdf("./plots/fig6.pdf", width = 15, height = 5)
f6 <- grid.arrange(f6A, f6B, f6C, ncol = 1)
# dev.off()


# output ------------------------------------------------------------------

ggsave(here::here("plots", "FIG_B.pdf"), plot = f6, device = "pdf", 
       width = 90, height = 240, units = "mm")

ggsave(here::here("plots", "FIG_B.png"), plot = f6,
       width = 90, height = 240, units = "mm")

