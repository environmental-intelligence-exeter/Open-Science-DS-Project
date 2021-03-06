## ---------------------------
##
## Script name: set-up.r
##
## Purpose of script: Install packages needed for project and set up helper functions.
##
## Author: Nathaneal Sheehan
##
## Date Created: 2021-03-20
##
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------


#################################################################
##                           Library                           ##
#################################################################
pkgs = c("tidyverse",# data cleaning
         "ddplot", # barchar plot
         "countrycode",# geocode continents
         "showtext", # use custom fonts
         "ggpubr",
         "cluster",
         "factoextra",
         "plotly") # stick plots together
installed_pkgs = pkgs %in% rownames(installed.packages())
if (any(installed_pkgs == FALSE)) {
  install.packages[!installed_pkgs]
}
invisible(lapply(pkgs, library, character.only = TRUE))
rm(installed_pkgs, pkgs)

font_add_google("Ubuntu", "ub")
##################################################################
##                       Helper Functions                       ## thanx for helpin
##################################################################
fetch_data = function(url,path) {
  url = url
  path = path
  download.file(url,path)
  read.csv(path)
} # Fetch data
dark_theme = function() {
  theme(
    # add border 1)
    panel.border = element_rect(
      colour = "slategrey",
      fill = NA,
      linetype = 2
    ),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x = element_line(
      colour = "#cf2e2e",
      linetype = 3,
      size = 0.5
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(
      colour = "white",
      face = "italic",
      family = "ub"
    ),
    axis.title = element_text(colour = "white", family = "ub"),
    axis.ticks = element_line(colour = "white"),
    plot.background = element_rect(fill = "#cf2e2e"),
    plot.title = element_text(family = "ub", hjust = .5, size = 16),
    plot.subtitle = element_text(family = "ub", hjust = .5, size = 12),
    legend.background = element_rect(fill = "#cf2e2e"),
    legend.text  = element_text(color = "white", family = "ub", size = 10),
    legend.key = element_rect(fill = "#cf2e2e"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
} # Dark theme
options(scipen=999) # Turn off scientific notation
