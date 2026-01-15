




library(leaflet)    # cartographie dynamique
library(sf)     # outils SIG

library(dplyr); options(dplyr.summarise.inform = FALSE)
library(stringr)

library(ajorant.divers)



### ________________________________________________________________________ ###
####                            > Couches SIG                               ####

# ~~~~{    Importation    }~~~~ #
id_ftp <- scan('data/id_ftp.txt', what = character())

sf_cadastre <- st_read_ftp(                                                     # lecture couche des parcelles cadastrales réunionnaises depuis serveur ftp chambre
  url_dossier = "ftp://172.23.0.179/BDD%20SIG/Administratif/Cadastre",
  identification = id_ftp
)


sf_cadastre <-  select(sf_cadastre,                                             # on ne garde que les colonnes utiles, en renomant le code insee de la commune
                       insee = commune,
                       section, 
                       numero)

# ~~~~{    Modification de la projection    }~~~~ #
sf_cadastre <- st_transform(sf_cadastre, 4326)



### ________________________________________________________________________ ###
####                             > Parcelles                                ####

# ~~~~{    Importation    }~~~~ #
# setwd('C:/Users/delagarde/Documents/Applications/agri_territoires_974/data/parcelles demo')
df_parcelles <- read.csv2('data/parcelles demo/parcelles demo.csv')

conv_com <- read.csv2("data/Communes_974.csv")                                # tableau de conversion des noms de commune


# ~~~~{    Standardisation    }~~~~ #
df_parcelles2 <- mutate(df_parcelles,
                        numero = str_extract(cadastre, '\\d+') %>% as.numeric() %>% as.character(),
                        section = str_extract(cadastre, '[:alpha:]+'),
                        id_prcl = paste(NOM_COM, cadastre, '-', expl)) %>%
  rename(Commune = NOM_COM) %>%
  left_join(conv_com) %>%
  mutate(insee = as.character(insee))

# ~~~~{    Jointure    }~~~~ #
sf_parcelles <- right_join(sf_cadastre, df_parcelles2)


# ~~~~{    Sauvegarde    }~~~~ #
# il abrège les intitulés de colonnes !!!
# write_sf(sf_parcelles, 'data/parcelles demo/sf_parcelles/sf_parcelles.shp')


### ________________________________________________________________________ ###
####                          > Exploitations                               ####

df_exploit <- read.csv2('data/parcelles demo/expl demo.csv')



### ________________________________________________________________________ ###
####                              > Carte                                   ####

palette_exploitations <- 
  ajorant.figures::mega_Palette(length(unique(sf_parcelles$expl)))           # une palette suffisamment grande pour toutes les exploitations

names(palette_exploitations) <- unique(sf_parcelles$expl)                    # chaque couleur correspond à une exploitation


list_icons <- ajorant.figures::leaf_colored_markers(palette_exploitations)      # liste de marqueurs colorés basés sur la palette de couleurs


black_dot <- makeIcon(
  iconUrl='data/SIG/dot-svgrepo-com.svg',
  iconWidth = 20,                                                   
  iconHeight = 20,
  iconAnchorX = 10,
  iconAnchorY = 24,
)


leaflet() %>%
  addTiles() %>%
  # addProviderTiles('Esri.WorldTerrain') %>%
  setView(lng = 55.71,
          lat = -21.08,
          zoom = 13) %>%
  
  addMapPane("dessus", zIndex = 630) %>% 
  addMapPane("dessous", zIndex = 620) %>%
  

  addPolygons(
    data = sf_parcelles,
    fillColor = ~as.vector(palette_exploitations[expl]),
    fillOpacity = 0.5,
    # les petits polygones disparaissent quand la carte est dézoomée
    # ne leur mettant une bordure, on les force à apparaitre
    color = ~as.vector(palette_exploitations[expl]),     # Couleur des bordures
    weight = 2,           # Épaisseur des bordures
    opacity = 0.5,
    layerId = ~id_parcelle,
    
    highlight = highlightOptions(
      weight = 2,
      fillOpacity = 1,
      opacity = 1,
      color = 'black',
      # fillColor = "white",
      #
    ),
    
    popup = ~paste(section,	numero),
    
  ) %>%
  addMarkers(
    data=df_exploit,
    icon = ~list_icons[expl],
    layerId = ~expl,
    options = pathOptions(pane = "dessus")
  ) %>%
  
  addMarkers(
    data=df_exploit,
    icon = black_dot,
    options = pathOptions(pane = "dessous")
    
  ) 









