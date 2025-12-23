


f_resume_betiole <- function(df_cheptel, 
                             selected_Com, 
                             selected_interco_com, 
                             selected_interco_lib,
                             selected_interco_num){
  
  res_Com <- df_cheptel %>%
    filter(Commune == selected_Com) %>%
    group_by(An, Bestiole) %>%
    summarize(tetes = mean(tetes, na.rm = TRUE),
              N = mean(N, na.rm = TRUE),
              ugb = mean(ugb, na.rm = TRUE))
  res_Com$Zone <- selected_Com
  
  res_interco <- df_cheptel %>%
    filter(Commune %in% selected_interco_com) %>%
    group_by(An, Bestiole) %>%
    summarize(tetes = sum(tetes, na.rm = TRUE)/selected_interco_num,
              N = sum(N, na.rm = TRUE)/selected_interco_num,
              ugb = mean(ugb, na.rm = TRUE)/selected_interco_num)
  res_interco$Zone <- str_c(selected_interco_lib, '(moy.)')
  
  res_974 <- df_cheptel %>%
    group_by(An, Bestiole) %>%
    summarize(tetes = sum(tetes, na.rm = TRUE)/24,
              N = sum(N, na.rm = TRUE)/24,
              ugb = mean(ugb, na.rm = TRUE)/24)
  res_974$Zone <- 'La Réunion(moy.)'
  
  
  resume_cheptel <- bind_rows(res_Com, res_interco, res_974) %>%
    ungroup() %>%
    mutate(An_Zone = paste(An, Zone),
           An_Zone = factor(An_Zone, unique(An_Zone))) %>%
    # ordre des Bestiole décroissant dans l'ordre des facteurs
    arrange(-tetes) %>%
    mutate(Bestiole = factor(Bestiole, unique(Bestiole))) %>%
    
    #ordre des zones 
    mutate(Zone = factor(Zone, 
                         c(selected_Com, 
                           str_c(selected_interco_lib, '(moy.)'), 
                           'La Réunion(moy.)'))) %>%
    filter(!is.na(Bestiole))
}
