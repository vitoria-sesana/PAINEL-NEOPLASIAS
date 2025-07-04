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
      sexo = NULL,
      escolari = NULL,
      ufnasc = NULL,
      ufresid = NULL
      )

    # ObserveEvent: botão filtro  --------------------
    observeEvent(input$botao_filtro,{
      
      df <- base_inicial$data()
      
      showModal(
        modalDialog(
          title = "Filtro avançado",
          
          fluidRow(
            
            ## Social --------------
            column(3,
                   h4("Características Sociais"),
                   
                   
                   #### sexo ---------------------
                   checkboxGroupInput(
                     ns("filtro_sexo"), 
                     "Gênero:",
                     choices = unique(df$sexo),
                     selected = unique(df$sexo)
                   ),
                   
                   #### escolaridade ------------
                   selectInput(
                     ns("filtro_escolari"),
                     'Escolaridade:',
                     sort(unique(df$escolari)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   
                   
                   #### faixa etária ------------
                   selectizeInput(
                     ns("filtro_faixa_etaria"),
                     "Faixa etária:",
                     choices = sort(unique(df$faixaetar)),
                     multiple = TRUE,
                     selected = unique(df$faixaetar),
                   ),
                  
                   
                   #### ufnasc -----------------
                   selectInput(
                     ns("filtro_ufnasc"),
                     'UF de nascimento:',
                     sort(unique(df$ufnasc)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### ufresid -----------------
                   selectInput(
                     ns("filtro_ufresid"),
                     'UF de residência:',
                     sort(unique(df$ufresid)),
                     multiple=TRUE, 
                     selectize=FALSE),

            ),
            
            # Tumores -------------------
            column(3,
                   h4("Filtros - Coluna 2"),
                   selectInput("filtro2_1", "Filtro 1:", choices = c("X", "Y", "Z")),
                   sliderInput("filtro2_2", "Filtro 2:", min = 10, max = 500, value = 100),
                   textInput("filtro2_3", "Filtro 3:")
            ),
            
            # Coluna 3
            column(3,
                   h4("Filtros - Coluna 3"),
                   selectInput("filtro3_1", "Filtro 1:", choices = c("Red", "Green", "Blue")),
                   sliderInput("filtro3_2", "Filtro 2:", min = 1, max = 10, value = 5),
                   textInput("filtro3_3", "Filtro 3:")
            ),
            # Coluna 4
            column(3,
                   h4("Filtros - Coluna 4"),
                   selectInput("filtro3_1", "Filtro 1:", choices = c("Red", "Green", "Blue")),
                   sliderInput("filtro3_2", "Filtro 2:", min = 1, max = 10, value = 5),
                   textInput("filtro3_3", "Filtro 3:")
            )
            
          ),

          selectizeInput(
            'e2', '2. Multi-select', choices = colnames(mtcars), multiple = TRUE
          ),
          selectInput(
            ns("in3"),
            'Options',
            colnames(mtcars),
            multiple=TRUE, 
            selectize=FALSE),

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
      filtros$escolari <- input$filtro_escolari
      filtros$ufnasc <- input$filtro_ufnasc
      filtros$ufresid <- input$filtro_ufresid
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
      if (!is.null(filtros$escolari) && length(filtros$escolari) > 0) {
        df <- df[df$escolari %in% filtros$escolari, ]
      }
      if (!is.null(filtros$ufnasc) && length(filtros$ufnasc) > 0) {
        df <- df[df$ufnasc %in% filtros$ufnasc, ]
      }
      if (!is.null(filtros$ufresid) && length(filtros$ufresid) > 0) {
        df <- df[df$ufresid %in% filtros$ufresid, ]
      }
      df
    })
    
    output$texto_filtro <- renderText({
      react_base_filtrada() %>% 
        as.data.frame() %>% 
        select(escolari) %>% 
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