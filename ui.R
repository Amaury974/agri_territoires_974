
ui <- fluidPage(
  style = "padding: 0px;", # no gap in navbar
  
  div(
    style = "position: absolute; top: 10px; right: 60px; z-index: 10000; font-size: 20px; ",
    htmlOutput('Commune'),
  ),
  
  # # ~~~~{    image d'arrière plan    }~~~~
  # tags$img(
  #   src = "logo simplifié.png",
  #   alt = 'logo chambre simple',
  #   style = 'position: fixed ; right: 10% ;top: 10% ;  z-index: -1',
  #   height = '90%'
  # ),
  
  # ~~~~{    image bandeau    }~~~~
  tags$img(
    src = "CA_LA REUNION_H_CMJN.png",
    alt = 'logo chambre',
    style = 'position: absolute; 
    top: 0px; left: 50%; 
    transform: translate(-50%, -10%) ;
    z-index: 9000; ',
    height = '60px'
  ),
  
  navbarPage(
    id = 'main_page',
    title = NULL, #'Mise en valeur des données',
    selected ='panel_carte',
    
    # header = uiOutput('selected_station'),
    
    # ~~~~{    Page 1 - carte, résumé commune    }~~~~
    tabPanel(
      value = 'panel_carte',
      title = "RGA", 
      
      fluidRow(
        column(4,
               leafletOutput(outputId  = 'map'), #input$Carte_marker_click
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
      ),
      
      fluidRow(
        h1('Productions Animales'),
        column(6,
               plotOutput('g_anim_ugb'),
        ),
        column(6,
               plotOutput('g_anim_N'),
        ),
        
      ),
      
    )
  )
)
