# Layout de filtro avançado -----------------------------------------------


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
    "Filtro",
    br(),
    actionButton(ns("botao_filtro"),"Filtro Avançado"),
    br(),
    actionButton(ns("botao_reset"), "Resetar Filtro "),
  )
  
}

server_filtro <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    subsetted <- reactive({
      req(input$species)
      iris |> filter(Species %in% input$species)
    })
    
    

# Botão: filtro avançado --------------------------------------------------

    observeEvent(input$botao_filtro,{
      
      showModal(
        modalDialog(
          title = "Filtro avançado",
          
          fluidRow(
            column(width = 3, div(style = "background-color: #f8f9fa; padding: 20px;", "Coluna 1")),
            column(width = 3, div(style = "background-color: #dee2e6; padding: 20px;", "Coluna 2")),
            column(width = 3, div(style = "background-color: #f8f9fa; padding: 20px;", "Coluna 3")),
            column(width = 3, div(style = "background-color: #dee2e6; padding: 20px;", "Coluna 4"))
          ),
          
          fluidRow(
            column(
              width = 3,
              sliderInput(
                ns("testeteste"),
                "Escolha o ponto de corte:", 
                min = 0,
                max = 10,
                value = 5, 
                step = 1
              )
              ),
            column(
              width = 3,
              sliderInput(
                ns("testeteste"),
                "Escolha o ponto de corte:", 
                min = 0,
                max = 10,
                value = 5, 
                step = 1
              )
            ),
            column(
              width = 3,
              sliderInput(
                ns("testeteste"),
                "Escolha o ponto de corte:", 
                min = 0,
                max = 10,
                value = 5, 
                step = 1
              )
            ),
            column(
              width = 3,
              sliderInput(
                ns("testeteste"),
                "Escolha o ponto de corte:", 
                min = 0,
                max = 10,
                value = 5, 
                step = 1
              )
            ),
            
            ),
          
          
          sliderInput(
            ns("testeteste"),
            "Escolha o ponto de corte:", 
            min = 0,
            max = 10,
            value = 5, 
            step = 1
          ),
          
          selectizeInput(
            'e2', '2. Multi-select', choices = colnames(mtcars), multiple = TRUE
          ),
          selectInput(
            'in3', 'Options',colnames(mtcars), multiple=TRUE, selectize=FALSE),
          
          
          layout_columns( 
            card( 
              card_header("Card 1 header"),
              p("Card 1 body"),
              sliderInput("slider", "Slider", 0, 10, 5),
            ), 
            card( 
              card_header("Card 2 header"),
              p("Card 2 body"),
              textInput("text", "Add text", ""),
            ), 
            
            checkboxGroupInput(
              "species", "Filter by species",
              choices = unique(iris$Species), 
              selected = unique(iris$Species)
            ),
            
            card( 
              card_header("Card 1 header"),
              p("Card 1 body"),
              sliderInput("slider", "Slider", 0, 10, 5),
            ),
            card( 
              card_header("Card 1 header"),
              p("Card 1 body"),
              sliderInput("slider", "Slider", 0, 10, 5),
            )
          ),
          
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("confirmar_filtro"), "Confirmar")
          ),
          # size = "l" 
          class = "modal-wide"
        )
      )
      
    })
    
    observeEvent(input$confirmar_filtro, {
      removeModal()
    })
    

# Botão: Resete de filtro -------------------------------------------------

    # observeEvent(input$botao_filtro,{
    #   
    # 
    # })
     

    # Saída: base selecionada ------------------------------------
    # Com filtro e sem filtro
  })
}