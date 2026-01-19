



### ________________________________________________________________________ ###
####                          > Packages                                    ####
cat('>> GLOBAL > Packages\n')


library(shiny)


library(leaflet)    # cartographie dynamique
library(sf)     # outils SIG

library(ggplot2); theme_set(theme_bw(base_size=20)) # graphiques
library(shadowtext)
library(dplyr); options(dplyr.summarise.inform = FALSE)
library(tidyr) # pivot table
library(stringr)



### ________________________________________________________________________ ###
####                          > couches SIG                                 ####

# ~~~~{    SIG    }~~~~ #
cat('>> GLOBAL > local SIG\n')


sf_communes <- st_read('data/SIG/communes/communesPolygon.shp')
sf_communes <- st_transform(sf_communes, 4326)
sf_communes$selected <- FALSE

# ~~~~{    parcelles page 2    }~~~~ #
sf_parcelles <- read_sf('data/parcelles demo/sf_parcelles/sf_parcelles.shp')
df_exploit <- read.csv2('data/parcelles demo/expl demo.csv')


### ________________________________________________________________________ ###
####                          > Palettes                                    ####
cat('>> GLOBAL > local SIG\n')

# ~~~~{    Communes insee    }~~~~ #
df_communes <- read.csv2('data/Communes_974.csv')


# ~~~~{    RGA    }~~~~
N_SAU_com <- read.csv2('data/RGA/N_SAU_com.csv')
df_culture <- read.csv2('data/RGA/vegetal.csv')
df_cheptel <- read.csv2('data/RGA/animal.csv')


### ________________________________________________________________________ ###
####                          > Palettes                                    ####


# ~~~~{    RGA page 1    }~~~~ #
# les palettes des graphiques de comparaison géographiques sont généré à chaque changement de commune

# Couleurs des graphiques par cultures uniquement
palette_culture = c('Canne à sucre' = '#D95F02',
                    'Canne' = '#D95F02',
                    'Arboriculture' = '#E7298A',
                    'Elevage' = '#7570B3',
                    'STH et Fourrage' = '#7570B3',
                    'STH & fourrages' = '#7570B3',
                    'Bovins Viande' = '#4e489b',
                    'Ovins Ou Caprins'= '#7b77b1',
                    'Porcins' = '#aaa5ea',
                    'Poulet' = '#39356d',
                    'Maraîchage' = '#1B9E77',
                    'Maraîchage & tubercules' = '#1B9E77',
                    'PAPAM' = '#1B9E77',
                    'Horticulture'= '#409078',
                    'Autre' = '#1B9E77',
                    'Polyculture/Polyélevage' = '#868686')

# ~~~~{    initialisation vide    }~~~~ #

df_resume_commune <- NULL
df_resume_label <- NULL
df_resume_culture <- NULL
df_resume_cheptel <- NULL

selected_interco_lib <- NULL
selected_interco_com <- NULL
selected_interco_num <- NULL

N_expl_comm <- NULL


# ~~~~{    parcelles page 2    }~~~~ #

palette_exploitations <- 
  ajorant.figures::mega_Palette(length(unique(sf_parcelles$expl)))           # une palette suffisamment grande pour toutes les exploitations

names(palette_exploitations) <- unique(sf_parcelles$expl)                    # chaque couleur correspond à une exploitation


list_icons <- ajorant.figures::leaf_colored_markers(palette_exploitations)      # liste de marqueurs colorés basés sur la palette de couleurs


# black_dot <- makeIcon(
#   iconUrl='data/SIG/dot-svgrepo-com.svg',
#   iconWidth = 20,                                                   
#   iconHeight = 20,
#   iconAnchorX = 10,
#   iconAnchorY = 24,
# )

path_ombre <- system.file(                                                      # chemin d'accès vers l'ombre du marqueur par défaut de Leaflet,
  "htmlwidgets/lib/leaflet/images/marker-shadow.png",                           #  dont la forme est suffisamment proche de celle du svg qu'on utilise
  package = "leaflet"                                                           #  (on ne peut pas utiliser le dit marqueur par défaut parce qu'il est en .png et donc que la colorisation serait beaucoup plus complexe qu'avec un .svg)
)
black_icon <- makeIcon(
  iconUrl='data/SIG/map-marker-plein.svg',
  iconWidth = 28,                                                   
  iconHeight = 28,
  iconAnchorX = 14,
  iconAnchorY = 26,
  
  shadowUrl = path_ombre,
  shadowWidth = 48,
  shadowHeight = 48,
  shadowAnchorX = 14,
  shadowAnchorY = 48
)




