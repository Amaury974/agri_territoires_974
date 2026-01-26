

packages <- c('shiny','leaflet','sf', 'ggplot2','shadowtext','dplyr','tidyr','stringr', 'devtools', 'colorspaces')
packages_manquants <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(packages_manquants) > 0) {
  install.packages(packages_manquants)
}
devtools::install_github("Amaury974/pack.figures")

