
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
    '> démo Shiny',
    style = 'position: absolute;
    top: 40px; left: 50%;
    transform: translate(-50%, 0%) ;
    z-index: 9000;
    font-family: Lucida Console;
    font-size: 9px;'
  ),
  
  navbarPage(
    id = 'main_page',
    title = NULL, #'Mise en valeur des données',
    
    # header = uiOutput('selected_station'),
    
    # ~~~~{    Page 1 - carte, résumé commune    }~~~~
    tabPanel(
      value = 'panel_RGA',
      title = "RGA", 
      
      # contenu rassemblé dans une colone centrale de 1100 px max. 
      # Les marges absorbent le redimensionnement de la fenètre
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
    
    tabPanel(
      value = 'panel_parcelle',
      title = "parcelle", 
      
      tags$h3("Démo sélection de parcelle, mise en avant du reste de l'exploitation sur la carte"),
      
      fluidRow(
        column(8,
               leafletOutput(outputId  = 'carte_parcelles'), #input$carte_parcelles_shape_click
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
)
