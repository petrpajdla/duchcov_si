figure_scripts <- list.files("./code/", pattern = "^figure", full.names = TRUE)

for (i in seq_along(figure_scripts)) {
  source(figure_scripts[i])
}