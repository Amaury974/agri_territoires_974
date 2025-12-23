

### ________________________________________________________________________ ###
####                   graphique évolution SAU par cultures                 ####

fg_veg_SAU <-  function(df_resume_culture, selected_Com){
  
  print(selected_Com)
  print(df_resume_culture)
  
  graph_SAU <- df_resume_culture %>%
    filter(Culture %in% unique(filter(df_resume_culture, Zone == selected_Com, SAU > 0)$Culture)) %>% 
    ggplot(aes(x = Culture, y = SAU, fill = An_Zone)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = palette_zone) +
    theme_minimal(base_size = 15) +
    theme(panel.background = element_rect(fill='transparent', color = 'transparent'),
          axis.text.x = element_text(angle = 20, hjust = 0.9),
          legend.position = c(1,1),
          legend.justification = c(1, 1),
          plot.caption = element_text(size = 6)) +
    labs(fill=NULL,
         x=NULL,
         y='SAU (ha)',
         title = "Surfaces par production végétal")
  # caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : Agreste - RA 2000 à 2020")
  
  graph_SAU
}

### ________________________________________________________________________ ###
####                  graphique évolution Nexpl par cultures                ####

fg_veg_N <-  function(df_resume_culture, selected_Com){
  
  graph_N <- df_resume_culture %>%
    filter(Culture %in% unique(filter(df_resume_culture, Zone == selected_Com, SAU > 0)$Culture)) %>% 
    arrange(Culture == 'Autre', -N) %>%
    mutate(Culture = factor(Culture, unique(Culture))) %>%
    ggplot(aes(x = Culture, y = N, fill = An_Zone)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = palette_zone) +
    theme_minimal(base_size = 15) +
    
    theme(panel.background = element_rect(fill='transparent', color = 'transparent'),
          axis.text.x = element_text(angle = 20, hjust = 0.9),
          legend.position = c(1,1),
          legend.justification = c(1, 1),
          plot.caption = element_text(size = 6)) +
    
    labs(fill=NULL,
         x=NULL,
         y="nombre d'exploitaitons",
         title = "Nombre d'exploitations par production végétal")
  # caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : Agreste - RA 2000 à 2020")
  
  graph_N
}

# ### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
# #                                                                              #
# ####                         CHIFFRES PAR CULTURE                           ####
# #                                                                              #
# ### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
# 
# ### ________________________________________________________________________ ###
# ####                               % evolution                              ####
# 
# setwd(dir_graph_cultures)
# df_resume_culture %>%
#   pivot_wider(id_cols=c(Zone, Culture),
#               names_from = An,
#               names_prefix = 'SAU_',
#               values_from = SAU) %>%
#   mutate(Variation_2010 = 100*(SAU_2020 - SAU_2010)/SAU_2010,
#          Variation_2000 = 100*(SAU_2020 - SAU_2000)/SAU_2000) %>%
#   write.csv2('SAU_cultures - evolution.csv')
# 
# 
# ### ________________________________________________________________________ ###
# ####                  SAU_cultures - part des entités sup                   ####
# 
# # ~~~~{ part des cultures de la commune dans l'interco }~~~~ #
# 
# 
# tab <- df_resume_culture %>%
#   filter(An == 2020) %>%
#   # mutate(Zone = factor(Zone, c(selected_Com, selected_interco_lib, 'La Réunion'))) %>%
#   arrange(Zone) %>%
#   pivot_wider(id_cols = Culture, names_from = Zone, values_from = SAU) %>%
#   mutate(Culture = as.character(Culture)) %>%
#   rename(Catégorie = Culture)
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
# tab[,2:4] <- apply(tab[,2:4], 2, function(X)str_c(format(round(signif(X,2)), big.mark = ' '),' ha'))
# 
# tab_SAU <- tab
# 
# 
# 
# # ~~~~{ rang de la commune }~~~~ #
# 
# tab_SAU <- df_SAU %>%
#   filter(An == 2020) %>%
#   arrange(Culture, desc(SAU)) %>%
#   group_by(Culture) %>%
#   mutate(one = 1,
#          rang_974 = cumsum(one)) %>%
#   filter(Commune %in% selected_interco_com) %>%
#   mutate(rang_interco = cumsum(one)) %>%
#   filter(Commune == selected_Com) %>%
#   select(Culture, rang_974, rang_interco) %>%
#   right_join(tab_SAU, join_by(Culture == Catégorie))
# 
# 
# 
# # ~~~~{ part dans la commune }~~~~ #
# 
# tab_SAU <- filter(df_resume_culture, An == 2020, Zone == selected_Com) %>%
#   mutate(part_SAU =  str_c(round(100*SAU/sum(SAU)), ' %')) %>%
#   select(Culture, `part SAU communale` = part_SAU) %>%
#   right_join(tab_SAU)
# 
# setwd(dir_graph_cultures)
# write.csv2(tab_SAU, 'SAU_cultures - part des entités sup.csv', row.names = FALSE)
# 
# # 'SAU_cultures - chiffres.txt')
# 
# ### ________________________________________________________________________ ###
# ####                    N_cultures - part des entités sup                   ####
# 
# # ~~~~{ chiffre + part des cultures de la commune dans l'interco }~~~~ #
# 
# tab <- df_resume_culture %>%
#   filter(An == 2020) %>%
#   # mutate(Zone = factor(Zone, c(selected_Com, selected_interco_lib, 'La Réunion'))) %>%
#   arrange(Zone) %>%
#   pivot_wider(id_cols = Culture, names_from = Zone, values_from = N) %>%
#   mutate(Culture = as.character(Culture)) %>%
#   rename(Catégorie = Culture)
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
# tab[,2:4] <- apply(tab[,2:4], 2, function(X) format(round(signif(X,2)), big.mark = ' '))
# 
# tab_N <- tab
# 
# 
# 
# # ~~~~{ rang de la commune }~~~~ #
# 
# tab_N <- df_SAU %>%
#   filter(An == 2020) %>%
#   arrange(Culture, desc(N)) %>%
#   group_by(Culture) %>%
#   mutate(one =1,
#          rang_974 = cumsum(one)) %>%
#   filter(Commune %in% selected_interco_com) %>%
#   mutate(rang_interco = cumsum(one)) %>%
#   filter(Commune == selected_Com) %>%
#   select(Culture, rang_974, rang_interco) %>%
#   right_join(tab_N, join_by(Culture == Catégorie))
# 
# 
# 
# # ~~~~{ part dans la commune }~~~~ #
# 
# tab_N <- filter(df_resume_culture, An == 2020, Zone == selected_Com) %>%
#   mutate(part_N =  str_c(round(100*N/N_expl_comm), ' %')) %>%
#   select(Culture, `part expl. communales` = part_N) %>%
#   right_join(tab_N)
# 
# setwd(dir_graph_cultures)
# write.csv2(tab_N, 'N_cultures - part des entités sup.csv', row.names = FALSE)
# 
# 
# 
# 
# ### ________________________________________________________________________ ###
# ####                        évolution par filière                           ####
# 
# Cult_i = unique(df_resume_culture$Culture)[2]
# 
# for(Cult_i in unique(df_resume_culture$Culture)[1:5]){
# 
#   plot_i <-
#     filter(df_resume_culture, Culture == Cult_i) %>%
#     ggplot(aes(x = An, y = SAU, fill = Zone)) +
#     geom_col(position = 'dodge') +
#     scale_fill_manual(values = palette_zone) +
#     theme_minimal(base_size = 15) +
#     theme(
#       legend.position = 'bottom',
#       legend.text = element_text(size = 10),
#       # legend.position = c(.03, .97),
#       # legend.justification = c(0, 1),
#       plot.caption = element_text(size = 9),
#       plot.title = element_text(size = 15)
#     ) +
#     labs(fill=NULL,
#          x=NULL,
#          y='SAU (ha)',
#          title = paste('Evolution des Sufaces', ifelse(Cult_i == 'Elevage', 'fourragères et pâtures', Cult_i), '\nà', selected_Com),
#          caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : Agreste - RA2000 à 2020")
# 
#   setwd(dir_graph_cultures)
#   svg(str_c('SAU_cultures - ',selected_Com, ' ', Cult_i,  '.svg'), 5,5)
#   print(plot_i)
#   dev.off()
# 
#   #
#   # setwd(dir_graph_cultures)
#   # png(str_c('SAU_cultures - ',selected_Com, ' ', Cult_i, '.png'), 5,5, units='in', res=200)
#   # print(plot_i)
#   # dev.off()
# 
# 
# }
# 








