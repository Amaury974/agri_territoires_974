
f_resume_culture <- function(df_culture, 
                 selected_Com, 
                 selected_interco_com, 
                 selected_interco_lib,
                 selected_interco_num){
  
res_Com <- df_culture %>%
  filter(Commune == selected_Com) %>%
  group_by(An, Culture) %>%
  summarize(SAU = mean(SAU, na.rm = TRUE),
            N = mean(N, na.rm = TRUE))
res_Com$Zone <- selected_Com

res_interco <- df_culture %>%
  filter(Commune %in% selected_interco_com) %>%
  group_by(An, Culture) %>%
  summarize(SAU = sum(SAU, na.rm = TRUE)/selected_interco_num,
            N = sum(N, na.rm = TRUE)/selected_interco_num)
res_interco$Zone <- str_c(selected_interco_lib, '(moy.)')

res_974 <- df_culture %>%
  group_by(An, Culture) %>%
  summarize(SAU = sum(SAU, na.rm = TRUE)/24,
            N = sum(N, na.rm = TRUE)/24)
res_974$Zone <- 'La Réunion(moy.)'



resume_SAU <- bind_rows(res_Com, res_interco, res_974) %>%
  ungroup() %>%
  mutate(An_Zone = paste(An, Zone),
         An_Zone = factor(An_Zone, unique(An_Zone))) %>%
  # ordre des culture décroissant dans l'ordre des facteurs
  arrange(Culture == 'Autre', -SAU) %>%
  mutate(Culture = factor(Culture, unique(Culture))) %>%
  
  #ordre des zones 
  mutate(Zone = factor(Zone, 
                       c(selected_Com, 
                         str_c(selected_interco_lib, '(moy.)'), 
                         'La Réunion(moy.)'))) %>%
  filter(!is.na(Culture))

resume_SAU
}










