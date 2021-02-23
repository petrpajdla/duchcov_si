library(tidyverse)
library(gridExtra)


# read data ---------------------------------------------------------------

isotopes <- read_rds(here::here("data/temp", "isotopes.RDS"))

sources_in <- read_rds(here::here("data/temp", "sources.RDS"))

sources <- sources_in$sources_full


# labels ------------------------------------------------------------------

lab_uni <- labs(shape = "K-means cluster",
                linetype = "K-means cluster\nlinear model",
                color = "Region", 
                fill = "Region")

lab_206_207 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^207, "Pb/", ""^204, "Pb")),
                    title = expression(paste("(A)")))
lab_206_208 <- labs(x = expression(paste(""^206, "Pb/", ""^204, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^204, "Pb")),
                    title = expression(paste("(B)")))
lab_207_208 <- labs(x = expression(paste(""^207, "Pb/", ""^206, "Pb")),
                    y = expression(paste(""^208, "Pb/", ""^206, "Pb")),
                    title = expression(paste("(C)")))


# theme -------------------------------------------------------------------

theme_set(theme_minimal())
theme_borders <- theme(panel.border = element_rect(fill = NA, color = "gray90"),
                       legend.position = "bottom", #c(0.9, 0), 
                       # legend.justification = c("right", "bottom")
                       axis.text = element_text(size = 6))


# region in-plot label ----------------------------------------------------

region_labels <- tribble(
  ~region, ~region2, ~Pb206_204, ~Pb207_204, ~Pb208_204, ~Pb207_206, ~Pb208_206, ~label,
  "Inn Valley & Mitterberg", "Inn Valley", 19, 15.75, 38.25, 0.82, 2, "Inn v.",
  "Inn Valley & Mitterberg", "Mitterberg", 19.5, 15.675, 40.5, 0.78, 2.05, "Mitt.",
  "CG, Harz & Rammelsberg", "CG, Harz", 18.64, 15.6, 38.3, 0.84, 2.085, "CG, Harz",
  "CG, Harz & Rammelsberg", "Rammelsberg", 18.35, 15.67, 38.05, 0.855, 2.08, "Ram.",
  "Apuseni & Baia Mare", "Apuseni", 18.85, 15.615, 38.4, 0.84, 2.06, "Ap.",
  "Apuseni & Baia Mare", "Baia Mare", 18.6, 15.685, 38.95, 0.83, 2.07, "B.M."
  ) %>% 
  mutate(region = factor(region, levels = levels(sources$region)),
         region2 = factor(region2, levels = levels(sources$region2)))


# plots -------------------------------------------------------------------
plot_models <- function(x) {
  x + 
    # sources
    geom_point(aes(color = region2, fill = region2),
               size = 1, shape = 21, alpha = 0.4, 
               show.legend = FALSE) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
    stat_smooth(aes(color = region2), 
                size = 0.6,
                method = "lm", se = FALSE, fullrange = FALSE,
                show.legend = FALSE) +
    scale_color_viridis_d(direction = -1, end = 0.9) + 
    # duchcov points
    geom_point(data = isotopes,
               aes(shape = kmeans),
               color = "black", fill = "white", size = 0.8) +
    scale_shape_manual(values = c(21, 24, 22, 3)
                       # guide = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    # source detailed name
    geom_text(data = region_labels, 
              aes(label = label, color = region2),
              size = 2.4,
              show.legend = FALSE) +
    stat_smooth(data = filter(isotopes, kmeans %in% c("1", "2")), 
                aes(linetype = kmeans), 
                size = 0.6, color = "black",
                method = "lm", se = FALSE, fullrange = TRUE) +
    scale_linetype_manual(values = c(2, 3) 
                          # guide = guide_legend(nrow = 1)
                          ) +
    # facetting
    facet_wrap(vars(region), scales = "free", ncol = 4) +
    lab_uni +
    theme_borders
}

f7A <- sources %>% ggplot(aes(Pb206_204, Pb207_204)) %>% 
  plot_models() +
  lab_206_207 +
  theme(legend.position = "none")

f7B <- sources %>% ggplot(aes(Pb206_204, Pb208_204)) %>% 
  plot_models() +
  lab_206_208

f7C <- sources %>% ggplot(aes(Pb207_206, Pb208_206)) %>% 
  plot_models() +
  lab_207_208
  

# output ------------------------------------------------------------------

f7ab <- grid.arrange(f7A, f7B, nrow = 2)

ggsave(here::here("plots", "FIG_C_1.pdf"), plot = f7ab, 
       width = 190, height = 200, units = "mm", scale = 1)
ggsave(here::here("plots", "FIG_C_2.pdf"), plot = f7C, 
       width = 190, height = 120, units = "mm", scale = 1)

ggsave(here::here("plots", "FIG_C_1.png"), plot = f7ab, 
       width = 190, height = 200, units = "mm", scale = 1)
ggsave(here::here("plots", "FIG_C_2.png"), plot = f7C, 
       width = 190, height = 120, units = "mm", scale = 1)
