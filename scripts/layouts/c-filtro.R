# Layout: filtragem avançada -----------------------------------------------
# recebe a base de dados com todas as colunas
# base de dados completa
# ponto de corte
# covariavel
# variavel tempo

# pegar a base, filtrar ela e enviar ela após o botão
# resetar o filtro novamente (pegar só a base enviada)

ui_filtro <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    br(),
    # Input: botão filtro avançado
    actionButton(ns("botao_filtro"),"Filtro Avançado"),
    br(),
    hr(),
    verbatimTextOutput(ns("texto_filtro"))
  )
}

server_filtro <- function(id, base_inicial) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive: valores filtros -----------
    
    filtros <- reactiveValues(
      faixaetar = NULL,
      sexo = NULL
      )

    # ObserveEvent: botão filtro  --------------------
    observeEvent(input$botao_filtro,{
      
      df <- base_inicial$data()
      
      showModal(
        modalDialog(
          title = "Filtro avançado",
          
          fluidRow(
            
            ## Social --------------
            column(4,
                   h4("Características Sociais"),
                   
                   #### faixa etária ------------
                   selectizeInput(
                     ns("filtro_faixa_etaria"),
                     "Selecione a faixa etária:",
                     choices = unique(df$faixaetar),
                     multiple = TRUE,
                     selected = unique(df$faixaetar),
                   ),
                   
                   #### sexo ---------------------
                   checkboxGroupInput(
                     ns("filtro_sexo"), 
                     "Selecione os genêros:",
                     choices = unique(df$sexo),
                     selected = unique(df$sexo)
                   ),
                   
                   textInput("filtro1_3", "Filtro 3:")
            ),
            
            # Tumores -------------------
            column(4,
                   h4("Filtros - Coluna 2"),
                   selectInput("filtro2_1", "Filtro 1:", choices = c("X", "Y", "Z")),
                   sliderInput("filtro2_2", "Filtro 2:", min = 10, max = 500, value = 100),
                   textInput("filtro2_3", "Filtro 3:")
            ),
            
            # Coluna 3
            column(4,
                   h4("Filtros - Coluna 3"),
                   selectInput("filtro3_1", "Filtro 1:", choices = c("Red", "Green", "Blue")),
                   sliderInput("filtro3_2", "Filtro 2:", min = 1, max = 10, value = 5),
                   textInput("filtro3_3", "Filtro 3:")
            )
          ),

          # fluidRow(
          #   column(width = 3, div(style = "background-color: #f8f9fa; padding: 20px;", "Coluna 1")),
          #   column(width = 3, div(style = "background-color: #dee2e6; padding: 20px;", "Coluna 2")),
          #   column(width = 3, div(style = "background-color: #f8f9fa; padding: 20px;", "Coluna 3")),
          #   column(width = 3, div(style = "background-color: #dee2e6; padding: 20px;", "Coluna 4"))
          # ),
          # 
          # fluidRow(
          #   column(
          #     width = 3,
          #     sliderInput(
          #       ns("testeteste"),
          #       "Escolha o ponto de corte:", 
          #       min = 0,
          #       max = 10,
          #       value = 5, 
          #       step = 1
          #     )
          #     ),
          #   column(
          #     width = 3,
          #     sliderInput(
          #       ns("testeteste"),
          #       "Escolha o ponto de corte:", 
          #       min = 0,
          #       max = 10,
          #       value = 5, 
          #       step = 1
          #     )
          #   ),
          #   column(
          #     width = 3,
          #     sliderInput(
          #       ns("testeteste"),
          #       "Escolha o ponto de corte:", 
          #       min = 0,
          #       max = 10,
          #       value = 5, 
          #       step = 1
          #     )
          #   ),
          #   column(
          #     width = 3,
          #     sliderInput(
          #       ns("testeteste"),
          #       "Escolha o ponto de corte:", 
          #       min = 0,
          #       max = 10,
          #       value = 5, 
          #       step = 1
          #     )
          #   ),
          #   
          #   ),
          # 
          # 
          # sliderInput(
          #   ns("testeteste"),
          #   "Escolha o ponto de corte:", 
          #   min = 0,
          #   max = 10,
          #   value = 5, 
          #   step = 1
          # ),
          # 
          # selectizeInput(
          #   'e2', '2. Multi-select', choices = colnames(mtcars), multiple = TRUE
          # ),
          # selectInput(
          #   'in3', 'Options',colnames(mtcars), multiple=TRUE, selectize=FALSE),
          # 
          # 
          # layout_columns( 
          #   card( 
          #     card_header("Card 1 header"),
          #     p("Card 1 body"),
          #     sliderInput("slider", "Slider", 0, 10, 5),
          #   ), 
          #   card( 
          #     card_header("Card 2 header"),
          #     p("Card 2 body"),
          #     textInput("text", "Add text", ""),
          #   ), 
          #   
          #   checkboxGroupInput(
          #     "species", "Filter by species",
          #     choices = unique(iris$Species), 
          #     selected = unique(iris$Species)
          #   ),
          #   
          #   card( 
          #     card_header("Card 1 header"),
          #     p("Card 1 body"),
          #     sliderInput("slider", "Slider", 0, 10, 5),
          #   ),
          #   card( 
          #     card_header("Card 1 header"),
          #     p("Card 1 body"),
          #     sliderInput("slider", "Slider", 0, 10, 5),
          #   )
          # ),
          # 
          footer = tagList(
            modalButton("Cancelar"),
            # actionButton(ns("resetar_filtros"), "Resetar Filtros"),
            actionButton(ns("aplicar_filtro"), "Aplicar filtro")
          ),
          size = "l"
        )
      )
      
    })
    
    # ObserveEvent: filtros ---------------------------------------------------
    observeEvent(input$aplicar_filtro, {
      
      ## Social ---------
      filtros$faixaetar <- input$filtro_faixa_etaria
      filtros$sexo <- input$filtro_sexo
      removeModal()
    })
    
    # Reactive: filtros --------------------------------------------------------
    react_base_filtrada <- reactive({
      df <- base_inicial$data()
      if (!is.null(filtros$faixaetar) && length(filtros$faixaetar) > 0) {
        df <- df[df$faixaetar %in% filtros$faixaetar, ]
      }
      if (!is.null(filtros$sexo) && length(filtros$sexo) > 0) {
        df <- df[df$sexo %in% filtros$sexo, ]
      }
      df
    })
    
    output$texto_filtro <- renderText({
      react_base_filtrada() %>% 
        as.data.frame() %>% 
        select(faixaetar) %>% 
        unique() %>% 
        as.character()
    })
     

    # Saída: -------------------------------------------------------------------
    
    return(
      list(
        data = react_base_filtrada
      )
    )
  })
}