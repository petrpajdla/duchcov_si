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

# Figure 6: Overview of LI ratios of the assemblage, highlighting individual clusters (1 – 4) 
# according to K-means. Error margins are about the size of the symbols in the plots with 204Pb in the denominator.

# lab_A <- expression(paste("(A) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^207, "Pb/", ""^204, "Pb"))
# lab_B <- expression(paste("(B) Isospace "^206, "Pb/", ""^204, "Pb to ", ""^208, "Pb/", ""^204, "Pb"))
# lab_C <- expression(paste("(C) Isospace "^207, "Pb/", ""^206, "Pb to ", ""^208, "Pb/", ""^206, "Pb"))
lab_A <- expression(paste("(A)"))
lab_B <- expression(paste("(B)"))
lab_C <- expression(paste("(C)"))

f6A <- ggplot(isotopes, aes(Pb206_204, Pb207_204)) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_A) + lab_206_207 +
  theme_gray() +
  theme(legend.position = "none")

f6B <- ggplot(isotopes, aes(Pb206_204, Pb208_204)) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_B) + lab_206_208 +
  theme_gray() +
  theme(legend.position = "none")

f6C <- ggplot(isotopes, aes(Pb207_206, Pb208_206)) +
  geom_point(aes(color = kmeans, shape = kmeans), size = 1.6) +
  scale_color_brewer(palette = "Set1", name = "K-means\ncluster") +
  labs(shape = "K-means\ncluster", title = lab_C) + lab_207_208 +
  guides(color = guide_legend(ncol = 2)) +
  theme_gray() +
  theme(legend.position = c(0.8, 0.24))

f6 <- grid.arrange(f6A, f6B, f6C, ncol = 1)

ggsave(here::here("plots", "FIG6.pdf"), plot = f6, device = "pdf", width = 90, height = 240, units = "mm")

