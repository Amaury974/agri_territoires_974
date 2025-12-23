# encoding UTF-8
# développé sous R 4.4.2
# dernière édition : déc. 2025
# auteur / contact : amaury.jorant@reunion.chambagri.fr
# __________
# 
# Projet : visuablisation des données recensement agricole Agreste
# 
# 
# Scripts liés : RGA ANALYSE.R
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



# la direction du script est donnée en argument dans l'appel
wd <- commandArgs(trailingOnly = TRUE)
print(wd)
wd <- paste(wd, collapse = ' ')

setwd(wd)
# setwd('M:/D_PPP_U/Amaury JORANT/Chartes Agricoles/_Analyse de données/app_RGA')
shiny::runApp(launch.browser = TRUE)









