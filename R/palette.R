

f_palette <- function(selected_Com, selected_interco_lib){
  
  
  
  # selected_interco_lib <<- filter(df_communes, Commune == selected_Com)$EPCI
  # selected_interco_com <<- filter(df_communes, EPCI == selected_interco_lib)$Commune
  # selected_interco_num <<- filter(df_communes, EPCI == selected_interco_lib)%>%nrow()
  
  # Couleurs des graphiques par zone (le 1988 n'est pas toujours disponible)
  palette_zone <-c()
  
  # extrapolé du thème CA
  palette_zone[paste(c('1988', '2000', '2010', '2020'), selected_Com)] <- c('#00DA63', '#00C057', '#009C46', '#00863D')
  palette_zone[paste(c('1988', '2000', '2010', '2020'), selected_interco_lib)] <- c('#F4A06C', '#EF772D', '#ED6B1C', '#C3540F')
  palette_zone[paste(c('1988', '2000', '2010', '2020'), 'La Réunion')] <- c('#00D7E2', '#00BBC4', '#008E96', '#00676C')
  palette_zone[c(selected_Com, selected_interco_lib, 'La Réunion')] <- c('#009C46', '#ED6B1C', '#008E96')
  
  # doublure avec (moy)
  palette_zone2 <- palette_zone[!str_detect(names(palette_zone), selected_Com)]
  names(palette_zone2) <- str_c(names(palette_zone2),'(moy.)')
  palette_zone <<- c(palette_zone, palette_zone2)
  
  
}
