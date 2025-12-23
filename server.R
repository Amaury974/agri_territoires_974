


server <- function(input, output) {
  
  # Variable réactive pour stocker l'ID sélectionné
  selected_Com <- reactiveVal(NULL)
  
  df_resume_commune <- reactiveVal(NULL)
  df_resume_label <- reactiveVal(NULL)
  df_resume_culture <- reactiveVal(NULL)
  df_resume_cheptel <- reactiveVal(NULL)
  
  selected_interco_lib <- reactiveVal(NULL)
  selected_interco_com <- reactiveVal(NULL)
  selected_interco_num <- reactiveVal(NULL)
  
  N_expl_comm <- reactiveVal(NULL)
  
  
  ### ______________________________________________________________________ ###
  ####                                 CARTE                                ####
  
  # ~~~~{    Carte interactive Leaflet, onglet 1    }~~~~
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      # addProviderTiles('Esri.WorldTerrain') %>%
      setView(lng = 55.54,
              lat = -21.11,
              zoom = 9.5) %>%
      addPolygons(data = sf_communes,
                  fillColor = "transparent",
                  color = "black",      # Couleur des bordures
                  weight = 0.5,           # Épaisseur des bordures
                  opacity = 1,
                  
                  layerId = ~code_insee,
                  
                  highlight = highlightOptions(
                    weight = 2,
                    # fillColor = "white",
                    # 
                  )
      )
    
  })
  
  
  # ~~~~{    réaction au clic    }~~~~
  observeEvent(input$map_shape_click, {
    cat('>> data > click _ 1\n')
    
    click <- input$map_shape_click
    print(click)
    
    id_clique <- click$id
    
    if(id_clique != 'polygone_selectionne'){
      
      com_clic <- filter(df_communes, insee == id_clique)$Commune
      
      # Stocker l'ID
      selected_Com(com_clic)
      
      cat('>>              _ ', selected_Com(), '\n')
      
      # interco
      selected_interco_lib(filter(df_communes, Commune == selected_Com())$EPCI)
      selected_interco_com(filter(df_communes, EPCI == selected_interco_lib())$Commune)
      selected_interco_num(filter(df_communes, EPCI == selected_interco_lib())%>%nrow())
      
      f_palette(selected_Com(), selected_interco_lib())
      

      # tableaux de résumé
      df_resume_commune(f_resume_commune(N_SAU_com, 
                                         selected_Com(), 
                                         selected_interco_com(), 
                                         selected_interco_lib()))
      
      df_resume_culture(f_resume_culture(df_culture, 
                                         selected_Com(), 
                                         selected_interco_com(), 
                                         selected_interco_lib(),
                                         selected_interco_num()))
      
      df_resume_cheptel(f_resume_betiole(df_cheptel, 
                                      selected_Com(), 
                                      selected_interco_com(), 
                                      selected_interco_lib(),
                                      selected_interco_num()))
      
      N_expl_comm(filter(N_SAU_com, Commune == selected_Com(), annee == 2020)$n_exploit)
      
      df_resume_label(f_resume_commune_label(df_resume_commune(), selected_Com()))
      
      cat('>>              _ 2\n')
      
      # Mettre à jour la carte pour highlighter le polygone sélectionné
      leafletProxy("map") %>%
        removeShape("polygone_selectionne") %>%
        addPolygons(
          data = sf_communes[sf_communes$code_insee == id_clique, ],
          fillColor = "white",
          fillOpacity = 0.5,
          color = "black", 
          weight = 2,
          layerId = "polygone_selectionne"
        )
    }
    
    cat('              _ fin\n\n')
  })
  
  
  
  
  ### ______________________________________________________________________ ###
  ####                            infos commune                             ####
  
  # ~~~~{    Commune sélectionnée    }~~~~
  output$Commune <- renderUI({

    if(is.null(selected_Com())) return(NULL)
    
    p(selected_Com())
  })
  
  output$chiffre_global <- renderUI(
    f_chiffre_global(
      N_SAU_com,
      df_resume_commune(),
      selected_Com(),
      selected_interco_lib()
    ))
  
  output$g_global_sau_et_n  <- renderPlot(
    fg_global_sau_et_n(
      df_resume_commune(), 
      df_resume_label(), 
      selected_Com()
    ))
  
  ### ______________________________________________________________________ ###
  ####                      graphiques généraux végétal                     ####
  
  output$g_veg_SAU  <- renderPlot(fg_veg_SAU(df_resume_culture(), selected_Com()))
  output$g_veg_N  <- renderPlot(fg_veg_N(df_resume_culture(), selected_Com()))
  
  
  ### ______________________________________________________________________ ###
  ####                      graphiques généraux animal                      ####
  
  output$g_anim_ugb  <- renderPlot(fg_anim_ugb(df_resume_cheptel(), selected_Com()))
  output$g_anim_N  <- renderPlot(fg_anim_N(df_resume_cheptel(), selected_Com()))
  
  
  
  
  
  
  
  
  # output$info_clic <- renderPrint({
  #   click <- input$carte_shape_click
  #   
  #   if(is.null(click)) {
  #     return("Cliquez sur un polygone")
  #   }
  #   
  #   # L'ID du polygone cliqué
  #   id_clique <- click$id
  #   
  #   # # Vous pouvez récupérer toutes les infos du polygone
  #   # polygone_selectionne <- polygones[polygones$id_colonne == id_clique, ]
  #   
  #   # cat("ID du polygone cliqué :", id_clique, "\n")
  #   # cat("Coordonnées du clic :", click$lat, ",", click$lng, "\n")
  #   # print(st_drop_geometry(polygone_selectionne))
  # })
  
  
  
}




