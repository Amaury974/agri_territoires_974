

### ________________________________________________________________________ ###
####                   graphique évolution SAU par cultures                 ####

fg_veg_SAU <-  function(df_resume_culture, selected_Com){
  cat('>> RGA > graph végétal SAU > déb\n')
  
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

  cat('>>                         > fin\n\n')
  graph_SAU
}

### ________________________________________________________________________ ###
####                  graphique évolution Nexpl par cultures                ####

fg_veg_N <-  function(df_resume_culture, selected_Com){
  cat('>> RGA > graph végétal N > déb\n')
  
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

  cat('>>                       > fin\n\n')
  graph_N
}