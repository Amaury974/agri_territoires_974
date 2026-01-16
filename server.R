


server <- function(input, output) {
  
  RV <- reactiveValues(data=NULL)
  
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  #####                     ACCUEIL / SÉLECTION COMMUNE                    #####
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  
  ### ______________________________________________________________________ ###
  ####                  > sélection depuis select_Input                     ####
  
  observeEvent(input$choix_commune, {
    cat('>> accueil > choix_commune > déb\n')
    RV$selected_Com <- input$choix_commune
    cat('>>                         >', RV$selected_Com, '\n')
    cat('>>                         > fin\n\n')
  })
  
  
  ### ______________________________________________________________________ ###
  ####                     > sélection depuis carte                         ####
  
  observeEvent(input$carte_communes_shape_click, {
    cat('>> accueil > sélection depuis carte > déb\n')
    
    click <- input$carte_communes_shape_click
    
    id_clique <- click$id
    
    if(id_clique != 'polygone_selectionne'){
      RV$selected_Com <- filter(df_communes, insee == id_clique)$Commune
      
      updateSelectInput(inputId = 'choix_commune', selected = RV$selected_Com)
    }
    cat('>>                                  >', RV$selected_Com, '\n')
    cat('>>                                  > fin\n\n')
    
  })
  
  
  ### ______________________________________________________________________ ###
  ####                      > changement de commune                         ####
  
  observeEvent(RV$selected_Com,{
    cat('>> accueil > changement commune > déb\n')
    
    # interco
    selected_interco_lib <<- filter(df_communes, Commune == RV$selected_Com)$EPCI
    selected_interco_com <<- filter(df_communes, EPCI == selected_interco_lib)$Commune
    selected_interco_num <<- filter(df_communes, EPCI == selected_interco_lib)%>%nrow()
    
    palette_zone <<- f_palette(RV$selected_Com, selected_interco_lib)
    
    cat('>>                              > 2 résumés\n')
    
    # tableaux de résumé
    df_resume_commune <<- f_resume_commune(N_SAU_com,
                                           RV$selected_Com,
                                           selected_interco_com,
                                           selected_interco_lib)
    
    df_resume_culture <<- f_resume_culture(df_culture,
                                           RV$selected_Com,
                                           selected_interco_com,
                                           selected_interco_lib,
                                           selected_interco_num)
    
    df_resume_cheptel <<- f_resume_betiole(df_cheptel,
                                           RV$selected_Com,
                                           selected_interco_com,
                                           selected_interco_lib,
                                           selected_interco_num)
    
    df_resume_label <<- f_resume_commune_label(df_resume_commune, RV$selected_Com)
    
    
    # Mise à jour la carte pour highlighter le polygone sélectionné
    cat('>>                              > 4 maj carte\n')
    
    leafletProxy("carte_communes") %>%
      removeShape("polygone_selectionne") %>%
      addPolygons(
        data = sf_communes[sf_communes$code_insee == filter(df_communes, Commune == RV$selected_Com)$insee, ],
        fillColor = "white",
        fillOpacity = 0.7,
        color = "#e3191b", 
        weight = 2,
        layerId = "polygone_selectionne"
      )
    
    cat('>>                              > fin\n\n')
  })
  
  
  ### ______________________________________________________________________ ###
  ####                        > Rendu initial carte                         ####
  
  output$carte_communes <- renderLeaflet({
    cat('>> accueil > carte init\n')
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 55.525,
              lat = -21.1,
              zoom = 9.3) %>%
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
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  #####                            PAGE 1 - RGA                            #####
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  
  ### ______________________________________________________________________ ###
  ####                           > infos commune                            ####
  
  output$Commune <- renderUI({
    
    if(is.null(RV$selected_Com)) return(NULL)
    
    p(RV$selected_Com)
  })
  
  output$chiffre_global <- renderUI(
    f_chiffre_global(
      N_SAU_com,
      df_resume_commune,
      RV$selected_Com,
      selected_interco_lib,
      selected_interco_num
    ))
  
  output$g_global_sau_et_n  <- renderPlot(
    fg_global_sau_et_n(
      df_resume_commune, 
      df_resume_label, 
      RV$selected_Com
    ))
  
  ### ______________________________________________________________________ ###
  ####                      > output généraux végétal                       ####
  
  output$g_veg_SAU  <- renderPlot(fg_veg_SAU(df_resume_culture, RV$selected_Com))
  output$g_veg_N  <- renderPlot(fg_veg_N(df_resume_culture, RV$selected_Com))
  
  output$t_veg <- renderTable({
    
    RV$selected_Com # nécéssaire pour déclancher l'actualisation
    
    filter(df_resume_culture, An == 2020) %>%
      # mutate(SAU = round(SAU),
      #        N = round(N)) %>%
      select(Zone, Culture, SAU, Nbr.Exp = N) %>%
      pivot_wider(id_cols = Culture, 
                  names_from = Zone,
                  names_sep = ' ',
                  values_from = c(SAU, Nbr.Exp), ) %>%
      arrange(Culture)
    
  }, digits = 0)
  
  ### ______________________________________________________________________ ###
  ####                      > output généraux animal                        ####
  
  output$g_anim_ugb  <- renderPlot(fg_anim_ugb(df_resume_cheptel, RV$selected_Com))
  output$g_anim_N  <- renderPlot(fg_anim_N(df_resume_cheptel, RV$selected_Com))
  
  output$t_anim <- renderTable({
    
    RV$selected_Com # nécessaire pour déclancher l'actualisation
    
    filter(df_resume_cheptel, An == 2020) %>%
      # mutate(ugb = round(ugb),
      #        tetes = round(tetes),
      #        N = round(N)) %>%
      select(Zone, Animal = Bestiole, UGB = ugb, Nbr.Exp = N, têtes = tetes) %>%
      pivot_wider(id_cols = Animal, 
                  names_from = Zone,
                  names_sep = ' ',
                  values_from = c(UGB, Nbr.Exp, têtes)) %>%
      arrange(Animal)
    
  }, digits = 0)
  
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  #####                         PAGE 2 - PARCELLES                         #####
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  
  
  ### ______________________________________________________________________ ###
  ####                        > Rendu initial carte                         ####
  
  output$carte_parcelles <- renderLeaflet({
    cat('>> PAGE 2 > carte parcelles\n')
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 55.71,
              lat = -21.08,
              zoom = 13) %>%
      addMapPane("dessus", zIndex = 630) %>% 
      addMapPane("dessous", zIndex = 620) %>%
      
      #BOS
      addPolygons(
        data = sf_parcelles,
        fillColor =  ~as.vector(palette_exploitations[expl]),
        fillOpacity = 0.5,
        # les petits polygones disparaissent quand la carte est dézoomée
        # ne leur mettant une bordure, on les force à apparaitre
        color =  ~as.vector(palette_exploitations[expl]),      # Couleur des bordures
        weight = 2,           # Épaisseur des bordures
        opacity = 0.4,
        layerId = ~id_prcl,
        
        highlight = highlightOptions(
          weight = 2,
          fillOpacity = 1,
          opacity = 1,
          color = 'black',
          # fillColor = "white",
          #
        ),
        group = 'dynamique',
      ) %>%
      addMarkers(
        data=df_exploit,
        lng = ~long,
        lat = ~lat,
        icon = ~list_icons[expl],
        layerId = ~expl,
        options = pathOptions(pane = "dessus")
        
      )
    
  })
  
  ### ______________________________________________________________________ ###
  ####                     > sélection depuis carte                         ####
  
  observeEvent(input$carte_parcelles_marker_click, {
    cat('>> PAGE 2 > click marker > déb\n')
    
    RV$selected_expl <- input$carte_parcelles_marker_click$id
    cat('                         >', RV$selected_expl,'\n')
    
    RV$selected_parcelle <- ''
    cat('                         > fin\n\n')
    
  })
  
  observeEvent(input$carte_parcelles_shape_click, {
    cat('>> PAGE 2 > click polygon > déb\n')
    
    RV$selected_parcelle <- input$carte_parcelles_shape_click$id
    cat('                          >', RV$selected_parcelle,'\n')
    
    RV$selected_expl <- str_extract(RV$selected_parcelle, '(?<= - ).+')
    cat('                          >', RV$selected_expl,'\n')
    cat('                          > fin\n\n')
  })
  
  
  observeEvent(c(RV$selected_expl,RV$selected_parcelle),{    
    cat('>> PAGE 2 > MAJ carte > déb\n')
    
    if(RV$selected_parcelle != 'polygone_selectionne'){
      
      leafletProxy("carte_parcelles") %>%
        clearGroup('dynamique') %>%
        
        addPolygons(
          data = sf_parcelles,
          fillColor =  ~as.vector(palette_exploitations[expl]),
          fillOpacity = ~ifelse(expl == RV$selected_expl, 1, 0.5),
          
          color = ~ifelse(expl == RV$selected_expl,'black',  as.vector(palette_exploitations[expl])),      # Couleur des bordures
          weight = ~case_when(  # Épaisseur des bordures
            id_prcl == RV$selected_parcelle ~ 2,
            expl == RV$selected_expl ~ 1,
            .default = 2),
          
          opacity = ~ifelse(expl == RV$selected_expl, 1, 0.4),
          layerId = ~id_prcl,
          
          highlight = highlightOptions(
            weight = 2,
            fillOpacity = 1,
            opacity = 1,
            color = 'black',
          ),
          group = 'dynamique',
          
        ) %>%
        addMarkers(
          data = filter(df_exploit, expl == RV$selected_expl),
          lng  = ~long,
          lat  = ~lat,
          icon = black_dot,
          options = pathOptions(pane = "dessous"),
          group = 'dynamique',
          
          
        ) 
    }
    
    cat('>>                    > fin\n\n')
    
  })
  
  output$info_exploitation <- renderUI({
    
    if(is.null(RV$selected_expl)) return(NULL)

    HTML(paste(
      '<h2>', RV$selected_expl,"</h2>",
      "<br><br><p><i>Informations et graphiques à propos de l'exploitation</i></p>"))
  })
  
  
  
  output$info_parcelle <- renderUI({
    
    if(is.null(RV$selected_parcelle)) return(NULL)
    if(RV$selected_parcelle == '') return(NULL)
    
    HTML(paste(
      '<br><h2>', str_remove(RV$selected_parcelle,' - .+'),"</h2>",
      "<br><br><p><i>Informations et graphiques à propos de la parcelle</i></p>"))
  })
  
  
}




