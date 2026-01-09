

# ### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
# #                                                                              #
# ####                            A INITIALISER                               ####
# #                                                                              #
# ### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
# 
# 
# # selected_Com <- 'Saint-Louis'
# # selected_Com <- 'Saint-Philippe'
# # selected_Com <- "L'Etang-Salé"
# selected_Com <- "Saint-Pierre"
# 

### ________________________________________________________________________ ###
####              graphique comparaison filières , zones UGB                ####


fg_anim_ugb <- function(resume_cheptel, selected_Com){
  
  bestiole_presentes <- unique(filter(resume_cheptel, Zone == selected_Com, ugb > 10)$Bestiole)
  
  graph_comparaison <- filter(resume_cheptel, !is.na(ugb)) %>%
    filter(Bestiole %in% bestiole_presentes) %>%
    ggplot(aes(x = Bestiole, y = ugb, fill = An_Zone)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = palette_zone) +
    
    # geom_shadowtext
    geom_text(data=filter(resume_cheptel, An == 2020, Zone == selected_Com, Bestiole %in% bestiole_presentes),
              aes(label = signif(tetes,2) |> 
                    round() |>
                    prettyNum(big.mark   = " ") |>
                    str_c(' têtes') |>
                    str_remove('NA têtes')
              ),
              angle = 90,
              nudge_x = -0.23,
              hjust=0,
              y = max(resume_cheptel$ugb, na.rm = T)/100,
              # color = 'black',
              # bg.colour = 'white',
              size = 3)+
    
    theme_minimal(base_size = 15) +
    theme(legend.position = c(1,1),
          legend.justification = c(1, 1),
          axis.text.x = element_text(size = 11, angle = 20, hjust = 0.9),
          plot.caption = element_text(size = 6)) +
    labs(fill=NULL,
         x=NULL,
         y='cheptel (UGB)',
         title = "Cheptel des différents animaux")
    # labs(caption = str_c(sep = '\n',
    #                      "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025",
    #                      "données : DAAF - RA2010 ; RA2020",
    #                      "UGB : unité gros bétail,",
    #                      "permettant de comparer des animaux de taille différentes"
    # ))
  
  graph_comparaison
}


### ______________________________________________________________________ ###
####               graphique comparaison filières , zones N               ####

fg_anim_N <- function(resume_cheptel, selected_Com){
  
  bestiole_presentes <- unique(filter(resume_cheptel, Zone == selected_Com, ugb > 10)$Bestiole)
  
  graph_comparaison_N <- filter(resume_cheptel, !is.na(ugb)) %>%
    filter(Bestiole %in% bestiole_presentes) %>%
    
    ggplot(aes(x = Bestiole, y = N, fill = An_Zone)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = palette_zone) +
    
    theme_minimal(base_size = 15) +
    theme(legend.position = c(1,1),
          legend.justification = c(1, 1),
          axis.text.x = element_text(size = 11, angle = 20, hjust = 0.9),
          plot.caption = element_text(size = 6)) +
    labs(fill=NULL,
         x=NULL,
         y="nombre d'exploitations",
         title="Nombre d'exploitations par animal")
    # labs(caption = str_c(sep = '\n',
    #                      "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025",
    #                      "données : DAAF - RA2010 ; RA2020"
    # ))
  
  graph_comparaison_N
}



# ### ______________________________________________________________________ ###
# ####                       graphiques par bestiole                        ####
# 
# best_i <- unique(resume_cheptel$Bestiole)[6]
# 
# # ~~~~{ N têtes }~~~~ #
# 
# # recencés depuis 1990 -> courbes
# best_i = bestiole_presentes[bestiole_presentes %in% c('Porcins', 'Bovins', 'Equins')][1]
# for(best_i in bestiole_presentes[bestiole_presentes %in% c('Porcins', 'Bovins', 'Equins')]){
#   
#   
#   tete_i <- filter(resume_cheptel, Bestiole == best_i) %>%
#     ggplot(aes(An, tetes, color = Zone)) +
#     geom_line(linewidth = 1) +
#     scale_y_continuous(limits = c(0, NA)) +
#     scale_color_manual(values = palette_zone) +
#     theme_minimal(base_size = 15) +
#     theme(
#       plot.caption = element_text(size = 9),
#       plot.title = element_text(size = 15)
#     ) +
#     theme(legend.position = c(0.97,0.3),
#           legend.justification = c(1, 1)) +
#     labs(x = NULL,
#          color = NULL,
#          y='cheptel (têtes)',
#          title = paste("Evolution du cheptel de", tolower(best_i))) +
#     labs(caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : DAAF - RA 1988 à 2020")
#   
#   
#   # setwd(dir_graph_elevage)
#   # png(paste(selected_Com,best_i,'.png'), 5, 5, units='in', res=200)
#   # print(tete_i)
#   # dev.off()
#   
#   setwd(dir_graph_elevage)
#   svg(paste0(selected_Com,' ',best_i,'.svg'), 5,5)
#   print(tete_i)
#   dev.off()
#   
# }
# 
# # recencés depuis 2010 seulement -> colonnes
# best_i = bestiole_presentes[bestiole_presentes %in% c('Volailles', 'Ovins Caprins', 'Lapins')][1]
# for(best_i in bestiole_presentes[bestiole_presentes %in% c('Volailles', 'Ovins Caprins', 'Lapins')]){
#   
#   
#   tete_i <- filter(resume_cheptel, Bestiole == best_i) %>%
#     ggplot(aes(An, tetes, fill = Zone)) +
#     geom_col(linewidth = 1, position = 'dodge') +
#     scale_fill_manual(values = palette_zone) +
#     
#     scale_y_continuous(limits = c(0, NA)) +
#     scale_x_continuous( breaks = c(2010,2020)) +
#     
#     
#     theme_minimal(base_size = 15) +
#     theme(
#       legend.position = 'bottom',
#       plot.caption = element_text(size = 9),
#       plot.title = element_text(size = 15)
#     ) +
#     
#     labs(x = NULL,
#          fill = NULL,
#          y='cheptel (têtes)',
#          title = paste("Evolution du cheptel de", tolower(best_i))) +
#     labs(caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : DAAF - RA 2010 à 2020")
#   
#   
#   # setwd(dir_graph_elevage)
#   # png(paste(selected_Com,best_i,'tetes.png'), 5.3, 5, units='in', res=200)
#   # print(tete_i)
#   # dev.off()
#   
#   setwd(dir_graph_elevage)
#   svg(paste(selected_Com,best_i,'tetes.svg'), 5.3,5)
#   print(tete_i)
#   dev.off()
#   
# }
# 
# 
# 
# # ~~~~{ N élevages }~~~~ #
# 
# for(best_i in bestiole_presentes){
#   
#   expl_i <-filter(resume_cheptel, Bestiole == best_i, !is.na(n)) %>%
#     ggplot(aes(An, N, fill = Zone)) +
#     geom_col(linewidth = 1, position = 'dodge') +
#     # geom_shadowtext(aes(x= An+(4.5*as.numeric(Zone)-5), label = round(N)), vjust = 1, angle = 90) +
#     # geom_text(data=df_label, aes(label = perc_n_exploit, x = as.numeric(dim)+0.45*( (Zone != Com)-0.5)))  +
#     
#     scale_y_continuous(limits = c(0, NA)) +
#     scale_x_continuous( breaks = c(2010,2020)) +
#     scale_fill_manual(values = palette_zone) +
#     theme_minimal(base_size = 15) +
#     theme(
#       legend.position = 'bottom',
#       plot.caption = element_text(size = 9),
#       plot.title = element_text(size = 15)
#     ) +
#     labs(x = NULL,
#          fill = NULL,
#          y= "Nombre d'élevages",
#          title = paste("Evolution du nombre d'élevage", tolower(best_i))) +
#     labs(caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : DAAF - RA 2010 à 2020")
#   
#   
#   # setwd(dir_graph_elevage)
#   # png(paste(selected_Com,best_i,'Nexpl.png'), 5, 5, units='in', res=200)
#   # print(expl_i)
#   # dev.off()
#   
#   setwd(dir_graph_elevage)
#   svg(paste(selected_Com,best_i,'Nexpl.svg'), 5,5)
#   print(expl_i)
#   dev.off()
#   
# }
# 
# 
# 
# 
# ### ________________________________________________________________________ ###
# ####                       tableau comparaison 2020                         ####
# 
# # ~~~~{ UGB }~~~~ #
# 
# tab <- resume_cheptel %>%
#   filter(An == 2020) %>%
#   # mutate(Zone = factor(Zone, c(selected_Com, selected_interco_lib, 'La Réunion'))) %>%
#   arrange(Zone) %>%
#   pivot_wider(id_cols = Bestiole, names_from = Zone, values_from = ugb) %>%
#   mutate(Bestiole = as.character(Bestiole)) %>%
#   rename(Catégorie = Bestiole)
# 
# tab[nrow(tab)+1,1] <- 'ugb totale'
# tab[nrow(tab),2:4] <- as.list(apply(tab[,2:4], 2, sum, na.rm=TRUE))
# 
# 
# tab[, selected_interco_lib] <- tab[,  str_c(selected_interco_lib, '(moy.)')] *selected_interco_num
# tab[,  'La Réunion'] <- tab[, 'La Réunion(moy.)'] * 26
# 
# tab <- select(tab, -c(str_c(selected_interco_lib, '(moy.)'), 'La Réunion(moy.)'))
# 
# tab[,paste('part', selected_interco_lib)] <- 
#   with(tab, paste(signif(100*get(selected_Com)/get(selected_interco_lib),2), '%'))
# 
# tab[,'part Réunion'] <- with(tab, paste(signif(100*get(selected_Com)/`La Réunion`, 2), '%'))
# 
# tab[,2:4] <- apply(tab[,2:4], 2, function(X)paste(format(round(signif(X,2)), big.mark = ' '), 'ugb'))
# 
# tab_ugb <- tab
# 
# 
# setwd(dir_graph_elevage)
# write.csv2(tab_ugb, paste(selected_Com, 'ugb 2020.csv'), row.names = FALSE)
# 
# 
# # ~~~~{ têtes }~~~~ #
# 
# tab <- resume_cheptel %>%
#   filter(An == 2020) %>%
#   # mutate(Zone = factor(Zone, c(selected_Com, selected_interco_lib, 'La Réunion'))) %>%
#   arrange(Zone) %>%
#   pivot_wider(id_cols = Bestiole, names_from = Zone, values_from = tetes) %>%
#   mutate(Bestiole = as.character(Bestiole)) %>%
#   rename(Catégorie = Bestiole)
# 
# tab[, selected_interco_lib] <- tab[,  str_c(selected_interco_lib, '(moy.)')] *selected_interco_num
# tab[,  'La Réunion'] <- tab[, 'La Réunion(moy.)'] * 26
# 
# tab <- select(tab, -c(str_c(selected_interco_lib, '(moy.)'), 'La Réunion(moy.)'))
# 
# tab[,paste('part', selected_interco_lib)] <- 
#   with(tab, paste(signif(100*get(selected_Com)/get(selected_interco_lib),2), '%'))
# 
# tab[,'part Réunion'] <- with(tab, paste(signif(100*get(selected_Com)/`La Réunion`, 2), '%'))
# 
# tab[,2:4] <- apply(tab[,2:4], 2, function(X)format(round(signif(X,2)), big.mark = ' '))
# 
# tab_tetes <- tab
# 
# 
# setwd(dir_graph_elevage)
# write.csv2(tab_tetes, paste(selected_Com, 'tetes 2020.csv'), row.names = FALSE)
# 
# # ~~~~{ elevage }~~~~ #
# 
# tab <- resume_cheptel %>%
#   filter(An == 2020) %>%
#   # mutate(Zone = factor(Zone, c(selected_Com, selected_interco_lib, 'La Réunion'))) %>%
#   arrange(Zone) %>%
#   pivot_wider(id_cols = Bestiole, names_from = Zone, values_from = N) %>%
#   mutate(Bestiole = as.character(Bestiole)) %>%
#   rename(Catégorie = Bestiole)
# 
# 
# tab[, selected_interco_lib] <- tab[,  str_c(selected_interco_lib, '(moy.)')] *selected_interco_num
# tab[,  'La Réunion'] <- tab[, 'La Réunion(moy.)'] * 26
# 
# tab <- select(tab,  -c(str_c(selected_interco_lib, '(moy.)'), 'La Réunion(moy.)'))
# 
# tab[,paste('part', selected_interco_lib)] <- 
#   with(tab, paste(signif(100*get(selected_Com)/get(selected_interco_lib),2), '%'))
# 
# tab[,'part Réunion'] <- with(tab, paste(signif(100*get(selected_Com)/`La Réunion`, 2), '%'))
# 
# tab[,2:4] <- apply(tab[,2:4], 2, function(X)format(round(signif(X,2)), big.mark = ' '))
# 
# tab_N <- tab
# 
# setwd(dir_graph_elevage)
# write.csv2(tab_N, paste(selected_Com, 'Nexpl 2020.csv'), row.names = FALSE)
# 
# 
# 
# 
# ### ________________________________________________________________________ ###
# ####                           tableau évolution                            ####
# 
# 
# 
# tab_evol <- resume_cheptel %>%
#   pivot_wider(id_cols=c(Zone, Bestiole), 
#               names_from = An, 
#               values_from = c(N, tetes)) %>%
#   mutate(Variation_n_2010_2020 = 100*(N_2020 - N_2010)/N_2010,
#          
#          Variation_tetes_2010_2020 = 100*(tetes_2020 - tetes_2010)/tetes_2010,
#          Variation_tetes_2000_2020 = 100*(tetes_2020 - tetes_2000)/tetes_2000)
# 
# tab_evol[,3:ncol(tab_evol)] <- apply(tab_evol[,3:ncol(tab_evol)], 2, function(X)signif(X,3))
# tab_evol[,9:ncol(tab_evol)] <- apply(tab_evol[,9:ncol(tab_evol)], 2, function(X)str_c(X,' %'))
# 
# names(tab_evol) <- str_replace(names(tab_evol), 'Bestiole', 'Cheptel') %>%
#   str_replace('(?<![:alpha:])n_', "nombre d'élevages ") %>%
#   str_replace('tetes_', "nombre d'animaux ") %>%
#   str_replace('_', " ")
# 
# 
# 
# setwd(dir_graph_elevage)
# write.csv2(tab_evol, paste(selected_Com, 'evolution.csv'), row.names = FALSE)












