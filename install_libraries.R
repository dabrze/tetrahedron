# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# -------------------------------------------------------------------------------------
# SYSTEM DEPENDENCIES:
# Cairo requires installed libcairo2-dev (https://www.cairographics.org).
# rgl requires installed libgl1-mesa-dev (https://www.mesa3d.org) and libglu1-mesa-dev.
# rgl dependencies:
#   - curl: requires libcurl4-openssl-dev
#   - magick: requires libmagick++-dev
# -------------------------------------------------------------------------------------

packages<-c("Cairo", "svglite", "ggplot2", "plot3D", "shiny", "rgl", "htmlwidgets", "jsonlite", "geometry", "RColorBrewer", "dplyr", "colourpicker")
check.packages(packages)