




f_resume_commune <- function(N_SAU_com, selected_Com, selected_interco_com, selected_interco_lib){
  
res_Com <- N_SAU_com %>%
  filter(Commune == selected_Com) %>%
  group_by(annee) %>%
  summarize(sau_tot_ha = mean(sau_tot_ha, na.rm = TRUE),
            n_exploit = mean(n_exploit, na.rm = TRUE)
            # ETP = mean(ETP, na.rm = TRUE)
  )
res_Com$Zone <- selected_Com


res_interco <- N_SAU_com %>%
  filter(Commune %in% selected_interco_com) %>%
  group_by(annee) %>%
  summarize(sau_tot_ha = mean(sau_tot_ha, na.rm = TRUE),
            n_exploit = mean(n_exploit, na.rm = TRUE)
            # ETP = mean(ETP, na.rm = TRUE)
  )
res_interco$Zone <- str_c(selected_interco_lib, '(moy.)')


res_974 <- N_SAU_com %>%
  group_by(annee) %>%
  summarize(sau_tot_ha = mean(sau_tot_ha, na.rm = TRUE),
            n_exploit = mean(n_exploit, na.rm = TRUE)
            # ETP = mean(ETP, na.rm = TRUE)
  )
res_974$Zone <- 'La Réunion(moy.)'


resume_commune <- bind_rows(res_Com, res_interco, res_974) %>%
  mutate(Zone = factor(Zone, c(selected_Com, str_c(selected_interco_lib, '(moy.)'), 'La Réunion(moy.)')),
         surf_moy = sau_tot_ha/n_exploit) %>%
  ungroup() 

resume_commune
}

f_resume_commune_label <- function(resume_commune, selected_Com){
  
  resume_label <- resume_commune %>%
  filter(annee %in% c(1988, 2020),
         Zone == selected_Com) %>%
  mutate(sau_tot_ha = round(sau_tot_ha),
         surf_moy2 = round(surf_moy,1)
         # ETP = round(ETP)
  )
  
  resume_label
}




