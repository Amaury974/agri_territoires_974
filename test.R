# encoding UTF-8
# développé sous R 4.4.2
# dernière édition : décembre 2025
# auteur / contact : amaury.jorant@reunion.chambagri.fr
# __________
# 
# Projet : Analyse des données des RGA, génération de figures.
# 
# 
# Scripts liés : RGA EXTRACTION.R
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
#                                                                              #
####                            ENVIRONNEMENT                               ####
#                                                                              #
### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###

setwd('M:/D_PPP_U/Amaury JORANT/Chartes Agricoles/_Analyse de données/app_RGA')

source('global.R')
for(script_i in list.files('R', pattern = '.+R$')) {
  print(script_i)
  source(paste0('R/',script_i))
}


### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
#                                                                              #
####                            A INITIALISER                               ####
#                                                                              #
### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###

selected_Com <- 'Saint-Pierre'
selected_Com <- 'Saint-Philippe'
# selected_Com <- 'Entre-Deux'
# selected_Com <- "L'Étang-Salé"
selected_Com <- 'Le Tampon'
selected_Com <- 'Saint-Paul'



### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
#                                                                              #
####                        CHARGEMENT DES DONNÉES                          ####
#                                                                              #
### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###



# interco
selected_interco_lib <- filter(df_communes, Commune == selected_Com)$EPCI
selected_interco_com <- filter(df_communes, EPCI == selected_interco_lib)$Commune
selected_interco_num <- filter(df_communes, EPCI == selected_interco_lib)%>%nrow()

f_palette(selected_Com, selected_interco_lib)

df_resume_commune <- f_resume_commune(N_SAU_com, 
                                   selected_Com, 
                                   selected_interco_com, 
                                   selected_interco_lib)

df_resume_label <- f_resume_commune_label(df_resume_commune, selected_Com)

df_resume_culture <- f_resume_culture(df_culture, 
                                   selected_Com, 
                                   selected_interco_com, 
                                   selected_interco_lib,
                                   selected_interco_num)

df_resume_cheptel <- f_resume_betiole(df_cheptel, 
                                selected_Com, 
                                selected_interco_com, 
                                selected_interco_lib,
                                selected_interco_num)

N_expl_comm <- filter(N_SAU_com, Commune == selected_Com, annee == 2020)$n_exploit




### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
#                                                                              #
####                                Figures                               ####
#                                                                              #
### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###

fg_global_sau_et_n(df_resume_commune, df_resume_label, selected_Com)

fg_veg_SAU(df_resume_culture, selected_Com)
fg_veg_N(df_resume_culture, selected_Com)

fg_anim_ugb(df_resume_cheptel, selected_Com)
fg_anim_N(df_resume_cheptel, selected_Com)
























