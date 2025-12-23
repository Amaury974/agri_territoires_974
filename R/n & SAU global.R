

### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###
#                                                                              #
####                               RESULTAT                               ####
#                                                                              #
### ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ###

### ________________________________________________________________________ ###
####                   evolution sau_tot_ha et n_exploit                    ####

fg_global_sau_et_n <- function(df_resume_commune, df_resume_label, selected_Com){
  
  y_ajust <- 3
  linetype_legend <- c('La Réunion(moy.)'='dotted')
  linetype_legend[selected_Com] <- 'solid'
  
  max_y <- filter(df_resume_commune, Zone %in% c(selected_Com, 'La Réunion(moy.)')) %>%
    summarize(max_y = max(sau_tot_ha, n_exploit*y_ajust)) %>%
    pull(max_y)
  
  # décallage des étiquettes:
  #       -par défaut décallage en haut
  #       -si les lignes sont trop serrées, décallage pour fuire l'autre ligne, 
  #         càd vers le haut pour la plus grande, vers le bas pour la plus petite
  
  df_resume_label$nudge_sens_forcage <- with(df_resume_label, ifelse(sau_tot_ha > n_exploit* y_ajust, 1, -1))
  # df_resume_label$nudge_n_exploit <- -df_resume_label$decallage_sau_tot_ha
  
  df_resume_label$nudge_besoin_forcage <- with(df_resume_label, min(abs(sau_tot_ha - n_exploit*y_ajust)/max_y) <  0.15)
  
  df_resume_label$nudge_sens_sau <- with(df_resume_label,1 + 2*pmin(0, nudge_besoin_forcage * nudge_sens_forcage))
  df_resume_label$nudge_sens_n <- with(df_resume_label,1 + 2*pmin(0, nudge_besoin_forcage * - nudge_sens_forcage))
  
  df_resume_label$nudge_sau_tot_ha <- with(df_resume_label, max_y/20* nudge_sens_sau)
  df_resume_label$nudge_n_exploit <- with(df_resume_label, max_y/20* nudge_sens_n)
  
  
  graph_SAU.n_exploit <- filter(df_resume_commune, Zone %in% c(selected_Com, 'La Réunion(moy.)')) %>%
    ggplot(aes(x = annee, linetype = Zone)) +
    
    # courbe sau_tot_ha
    geom_line(linewidth = 1, aes(y=sau_tot_ha), color='#4DD091') +
    geom_point(data=filter(df_resume_commune, Zone == selected_Com),
               aes(y=sau_tot_ha), color='#4DD091', shape = 4, size = 2) +
    
    # geom_segment(data=df_resume_label, aes(y=sau_tot_ha, yend = sau_tot_ha+50))+
    
    geom_shadowtext(
      data = df_resume_label,
      aes(y = pmax(0,sau_tot_ha + nudge_sau_tot_ha), 
          label = round(sau_tot_ha, -1), 
          hjust = c(0.2, 0.8)), 
      vjust=0.5, size =5,
      col="#185C3C", bg.color="white"
    ) +
    
    # courbe expl
    geom_line(linewidth = 1, aes(y = n_exploit * y_ajust), color = '#FF5C77') +
    geom_point(data = filter(df_resume_commune, Zone == selected_Com),
               aes(y = n_exploit * y_ajust), color = '#FF5C77', shape = 4, size = 2) +
    
    # with(df_resume_label, y_ajust + max_y / 20 + 2 * pmin(-decalage, 0))
    geom_shadowtext(
      data = df_resume_label,
      aes(y = pmax(0,n_exploit * y_ajust + nudge_n_exploit), 
          label = round(n_exploit, -1),
          hjust = c(0.2, 0.8)), 
      vjust = 0.5, 
      size = 5,
      col = "#8A0017", 
      bg.color = "white"
    ) +
    
    scale_linetype_manual(values = linetype_legend,
                          guide = guide_legend(override.aes = list(color = 'grey40'))) +
    
    # second axe
    scale_y_continuous(
      limits = c(0,NA),
      sec.axis = sec_axis(~./y_ajust, name="Nombre d'exploitations")
    ) + 
    
    theme_minimal(base_size = 17) +
    theme(panel.background = element_rect(fill='transparent', color = 'transparent'),
          legend.position = "bottom",
          # legend.position = c( 1, 1),
          # legend.justification = c(1, 1),
          plot.title = element_text(hjust = 0.5, vjust = 2),
          plot.caption = element_text(size = 9),
          axis.title.y = element_text(color = '#4DD091'),
          axis.title.y.right = element_text(color = '#FF5C77', angle = 90)
          
    ) +
    labs(linetype = NULL,
         y = 'SAU (ha)',  
         x = 'Année',
         title = str_c("Evolution de l'agriculture\nà ", selected_Com))
         # caption = "réalisation : Chambre d'Agriculture de la Réunion - D3P, 2025\ndonnées : Agreste - RA 1988 à 2020")
  
  graph_SAU.n_exploit
  
  # setwd(dir_graph)
  # svg('n_exploit & sau_tot_ha - sau_tot_ha.n_exploit.svg', 5.5,5)
  # graph_SAU.n_exploit
  # dev.off()
  
}

### ________________________________________________________________________ ###
####                                chiffres                                ####

# ~~~~{ % Com dans interco }~~~~ #
f_chiffre_global <- function(N_SAU_com, df_resume_commune, selected_Com, selected_interco_lib){
  
HTML(str_c(
  '<h3>',
  filter(df_communes, Commune == selected_Com)$insee,' ', selected_Com, 
'</h3>',

'<p><b>',
  round(100*filter(df_resume_commune, annee == 2020, Zone == selected_Com)$sau_tot_ha /
          (selected_interco_num*filter(df_resume_commune, annee == 2020, str_detect(Zone, selected_interco_lib))$sau_tot_ha)),

  '%</b> de la SAU de ', selected_interco_lib, '<br>',

  'et <b>',
  round(100*filter(df_resume_commune, annee == 2020, Zone == selected_Com)$n_exploit /
          (selected_interco_num*filter(df_resume_commune, annee == 2020, str_detect(Zone, selected_interco_lib))$n_exploit)),
  "%</b> du nombre d'exploitation de l'interco</p>",

"<p> <b>",
  filter(N_SAU_com, annee == max(annee)) %>%
    arrange(-sau_tot_ha) %>%
    mutate(classement_SAU = cumsum(!is.na(Commune))) %>%
    filter(Commune == selected_Com) %>%
    pull(classement_SAU),
  "<sup>ème</sup></b> commune reunionnaise en surface<br><b>",

  filter(N_SAU_com, annee == max(annee)) %>%
    arrange(-n_exploit) %>%
    mutate(classement_N = cumsum(!is.na(Commune))) %>%
    filter(Commune == selected_Com) %>%
    pull(classement_N),
  "<sup>ème</sup></b> commune reunionnaise en nombre d'agriculteurs"
))

}






