





library(leaflet)    # cartographie dynamique
library(sf)     # outils SIG

library(dplyr); options(dplyr.summarise.inform = FALSE)
library(stringr)

library(ajorant.divers)




url_dossier <- "ftp://172.23.0.179/BDD%20SIG/Environnement/BOS/"
id_ftp <- "sig_r:c43xQC6b3gBd7Y"                               # <- mots de passe lecture FTP




# ~~~~{    importation des couches    }~~~~ #

sf_BOS <- st_read_ftp("ftp://172.23.0.179/BDD%20SIG/Environnement/BOS", id_ftp) 
sf_communes <- st_read_ftp("ftp://172.23.0.179/BDD%20SIG/Administratif/Communes", id_ftp) 

sf_BOS <- st_transform(sf_BOS, 4326)
sf_communes <- st_transform(sf_communes, 4326)



palette_culture <- colorFactor(
  palette = 'Dark2',
  domain = unique(sf_BOS$culture))

leaflet() %>%
  addTiles() %>%
  # addProviderTiles('Esri.WorldTerrain') %>%
  setView(lng = 55.54,
          lat = -21.11,
          zoom = 9.3) %>%
  #BOS
  addPolygons(data = sf_BOS,
              fillColor = ~palette_culture(culture),
              fillOpacity = 1,
              # les petits polygones disparaissent quand la carte est dézoomée
              # ne leur mettant une bordure, on les force à apparaitre
              color = ~palette_culture(culture),      # Couleur des bordures
              weight = 0.5,           # Épaisseur des bordures
              opacity = 1,
  ) %>%
  #Communes
  addPolygons(data = sf_communes,
              fillColor = "transparent",
              color = "black",      # Couleur des bordures
              weight = 0.5,           # Épaisseur des bordures
              opacity = 1,
              
              layerId = ~code_insee,
              
              highlight = highlightOptions(
                weight = 2,
                # fillColor = "white",
                #
              )
  )








