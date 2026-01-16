
ui <- fluidPage(
  style = "padding: 0px;", # no gap in navbar
  
  div(
    style = "position: absolute; top: 10px; right: 30px; z-index: 10000; font-size: 20px; ",
    selectInput('choix_commune', 
                label = NULL,
                choices = df_communes$Commune),
    # htmlOutput('Commune'),
  ),
  
  # # ~~~~{    image d'arrière plan    }~~~~
  # tags$img(
  #   src = "logo simplifié.png",
  #   alt = 'logo chambre simple',
  #   style = 'position: fixed ; right: 10% ;top: 10% ;  z-index: -1',
  #   height = '90%'
  # ),
  
  # ~~~~{    logo / nom appli    }~~~~
  tags$img(
    src = "CA_LA REUNION_H_CMJN.png",
    alt = "logo chambre d'agriculture de La Réunion",
    style = 'position: absolute;
    top: 0px; left: 50%;
    transform: translate(-50%, -10%) ;
    z-index: 9000; ',
    # height = '60px'
    height = '50px'
  ),
  
  tags$p(
    '> démo Shiny * Leaflet',
    style = 'position: absolute;
    top: 40px; left: 50%;
    transform: translate(-50%, 0%) ;
    z-index: 9000;
    font-family: Lucida Console;
    font-size: 9px;'
  ),
  
  navbarPage(
    id = 'main_page',
    title = NULL,
    fluid = FALSE,
    
    # header = uiOutput('selected_station'),
    
    #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
    #####                      PAGE 1 - Commune / RGA                        #####
    #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
    
    tabPanel(
      value = 'panel_RGA',
      title = "Communes", 
      
      # contenu rassemblé dans une colonne centrale de 1100 px max. 
      # Les marges absorbent le redimensionnement de la fenêtre
      div(style = "max-width: 1100px; margin: 0 auto;",
          
          fluidRow(
            column(4,
                   leafletOutput(outputId  = 'carte_communes'), #input$carte_communes_shape_click
            ),
            
            column(3,
                   # textOutput("info_clic"),
                   htmlOutput("chiffre_global"),
            ),
            column(5,
                   plotOutput('g_global_sau_et_n'),
            ),
          ),
          
          fluidRow(
            h1('Productions Végétales'),
            
            column(6,
                   plotOutput('g_veg_SAU'),
            ),
            column(6,
                   plotOutput('g_veg_N'),
            ),
            column(12,
                   tableOutput('t_veg')
            ),    
          ),
          
          fluidRow(
            h1('Productions Animales'),
            column(6,
                   plotOutput('g_anim_ugb'),
            ),
            column(6,
                   plotOutput('g_anim_N'),
            ),
            column(12,
                   tableOutput('t_anim')
            ),    
            
          ),
      ),
    ),
    
    #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
    #####                         PAGE 2 - Parcelles                         #####
    #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
    
    tabPanel(
      value = 'panel_parcelle',
      title = "Parcelles", 
      
      # contenu rassemblé dans une colonne centrale de 1100 px max. 
      # Les marges absorbent le redimensionnement de la fenêtre
      div(style = "max-width: 1100px; margin: 0 auto;",
          
          tags$h3("Démo sélection de parcelle, mise en avant du reste de l'exploitation sur la carte"),
          
          fluidRow(
            column(8,
                   leafletOutput(outputId  = 'carte_parcelles'), #input$carte_parcelles_shape_click & input$carte_parcelles_marker_click
            ),
            column(4,
                   htmlOutput("info_exploitation"),
                   htmlOutput("info_parcelle")
                   
            )
          ),
          tabPanel(
            value = 'panel_etc',
            title = "etc.", 
          )
      )
    )
  ),
  
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  #####                             BAS DE PAGE                            #####
  #  ¤¤¤¤¤¤¤¤¤¤                         ¤¤                         ¤¤¤¤¤¤¤¤¤¤  #
  # Espacement avant le footer
  br(),
  
  tags$div(
    style = "max-width: 1100px; margin: 0 auto; padding: 0 10px; font-size: 0.85em;",  # Réduit la taille de police de 15%
    
    hr(),
    h4("📧 Contacts"),
    fluidRow(
      column(4,
             tags$p(
               tags$strong("Équipe de développement"),
               tags$br(),
               "Amaury Jorant - Bureau des références statistiques (D3P)",
               tags$br(),
               "Email: ", tags$a("amaury.jorant@reunion.chambagri.fr",
                                 href = "mailto:amaury.jorant@reunion.chambagri.fr"),
               tags$br(),
               "Tél: +262 262 944 628"
             ),
      ),
      column(4,
             tags$p(
               tags$strong("Référent des données"),
               tags$br(),
               tags$br(),
               "Email: ", tags$a("contact@reunion.chambagri.fr", 
                                 href = "mailto:contact@reunion.chambagri.fr"),
               tags$br(),
               "Tél: +262 XXX XXX XXX "
             ),
      ), 
      column(4,
             tags$img(
               src = "CA_LA REUNION_H_CMJN.png",
               alt = "logo chambre d'agriculture de La Réunion",
               style = 'transform: translate(0%, -20%)',
               height = '100px'
             ),
      )
    ),
    
    # Bas de page
    hr(style = "margin-top: 10px; margin-bottom: 5px;"),
    tags$footer(
      style = "text-align: center; color: #666; padding: 5px;",
      tags$p(
        "© 2026 - Chambre d'Agriculture de La Réunion | ",
        "Version 1.0 | ",
        "Dernière mise à jour: Janvier 2026"
      )
      
    )
  )
  
)
