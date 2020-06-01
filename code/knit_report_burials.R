# knit report
# knitr::knit("./si/duchcov_support.Rmd", output = "./si/duchcov_support.md")
rmarkdown::render(input = here::here("si", "duchcov_support_burials.Rmd"))

# # create in-text figures
# # MOVED TO MAKEFILE
# figure_scripts <- list.files("./code/", pattern = "^figure", full.names = TRUE)
# 
# for (i in seq_along(figure_scripts)) {
#   source(figure_scripts[i])
# }
