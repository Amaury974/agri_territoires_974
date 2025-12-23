




library(shiny)


library(leaflet)    # cartographie dynamique
library(sf)     # outils SIG

library(ggplot2); theme_set(theme_bw(base_size=20)) # graphiques
library(shadowtext)
library(dplyr); options(dplyr.summarise.inform = FALSE)
library(stringr)




# ~~~~{    SIG    }~~~~
sf_communes <- st_read('data/SIG/communes/communesPolygon.shp')
sf_communes <- st_transform(sf_communes, 4326)
sf_communes$selected <- FALSE


# ~~~~{    Communes insee    }~~~~
df_communes <- read.csv('data/COMMUNE.csv')


# ~~~~{    RGA    }~~~~
N_SAU_com <- read.csv2('data/RGA/N_SAU_com.csv')
df_culture <- read.csv2('data/RGA/cultures.csv')
df_cheptel <- read.csv2('data/RGA/cheptel.csv')

# ~~~~{    Palettes    }~~~~

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

