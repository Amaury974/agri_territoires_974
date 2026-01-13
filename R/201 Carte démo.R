
# library(leaflet)    # cartographie dynamique
# library(sf)     # outils SIG
# 
# library(dplyr); options(dplyr.summarise.inform = FALSE)
# library(stringr)
# 
# library(ajorant.divers)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# sf_cadastre <- st_read_ftp(                                                     # lecture couche des parcelles cadastrales réunionnaises depuis serveur ftp chambre
#   url_dossier = "ftp://172.23.0.179/BDD%20SIG/Administratif/Cadastre", 
#   identification = id_ftp
# ) 
# 
# 
# # ~~~~{    Modification de la projection    }~~~~ #
# sf_cadastre <- st_transform(sf_cadastre, 4326)
# 
# sf_cadastre2 <- filter(sf_cadastre, NOM_COM == 'Saint-Benoît')
# 
# leaflet() %>%
#   addTiles() %>%
#   # addProviderTiles('Esri.WorldTerrain') %>%
#   setView(lng = 55.54,
#           lat = -21.11,
#           zoom = 9.3) %>%
#   #BOS
#   addPolygons(data = sf_cadastre2,
#             popup = ~paste(SECTION,	NUMERO)
# )
  



# setwd('C:/Users/delagarde/Documents/Applications/agri_territoires_974/data/parcelles demo')
# parcelles <- read.csv2('parcelles demo.csv')
# 
# parcelles <- mutate(parcelles,
#                     NUMERO = str_extract(cadastre, '\\d+'),
#                     SECTION = str_extract(cadastre, '[:alpha:]+'),
#                     id = paste(cadastre, '-', expl))
# 
# sf_parcelles <- right_join(sf_cadastre, parcelles)
# 
# 
# 
# write_sf(sf_parcelles, 'sf_parcelles/sf_parcelles.shp')
# 
# sf_parcelles <- read_sf(sf_parcelles/sf_parcelles.shp)






palette_exploitations <- colorFactor(
  palette = ajorant.figures::mega_Palette(length(unique(sf_parcelles$expl))),                     # fonction perso pour avoir une très grande palette de couleurs
  domain = unique(sf_parcelles$expl))


leaflet() %>%
    addTiles() %>%
    # addProviderTiles('Esri.WorldTerrain') %>%
    setView(lng = 55.71,
            lat = -21.08,
            zoom = 13) %>%
    #BOS
    addPolygons(data = sf_parcelles,
              fillColor = ~palette_exploitations(expl),
              fillOpacity = 0.7,
              # les petits polygones disparaissent quand la carte est dézoomée
              # ne leur mettant une bordure, on les force à apparaitre
              color = ~palette_exploitations(expl),      # Couleur des bordures
              weight = 2,           # Épaisseur des bordures
              opacity = 0.7,
              layerId = ~id,
              
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 1,
                opacity = 1,
                color = 'black',
                # fillColor = "white",
                #
              ),
              
              popup = ~paste(SECTION,	NUMERO),
              
    )


























