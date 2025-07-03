library(shiny)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Exemplo com Filtros no Modal"),
  
  # Botão para abrir modal
  actionButton("abrir_filtros", "Filtros"),
  
  # Mostrar tabela com os dados filtrados
  DTOutput("tabela_filtrada")
)

server <- function(input, output, session) {
  
  
  # Base original carregada previamente
  base_dados <- mtcars
  base_dados$cyl <- as.factor(base_dados$cyl)
  base_dados$gear <- as.factor(base_dados$gear)
  
  
  
  # Armazenar os filtros aplicados
  filtros <- reactiveValues(cyl = NULL, gear = NULL)
  
  
  
  # Abrir o modal com os filtros
  observeEvent(input$abrir_filtros, {
    showModal(modalDialog(
      title = "Filtros",
      selectInput("filtro_cyl", "Cilindradas", choices = unique(base_dados$cyl), multiple = TRUE),
      selectInput("filtro_gear", "Marchas", choices = unique(base_dados$gear), multiple = TRUE),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("aplicar_filtros", "Aplicar Filtros")
      )
    ))
  })
  
  
  
  # Atualizar filtros ao clicar em "Aplicar Filtros"
  observeEvent(input$aplicar_filtros, {
    filtros$cyl <- input$filtro_cyl
    filtros$gear <- input$filtro_gear
    removeModal()
  })
  
  
  
  # Filtrar os dados com base nos filtros aplicados
  dados_filtrados <- reactive({
    dados <- base_dados
    
    if (!is.null(filtros$cyl) && length(filtros$cyl) > 0) {
      dados <- dados %>% filter(cyl %in% filtros$cyl)
    }
    
    if (!is.null(filtros$gear) && length(filtros$gear) > 0) {
      dados <- dados %>% filter(gear %in% filtros$gear)
    }
    
    return(dados)
  })
  
  # Renderizar a tabela
  output$tabela_filtrada <- renderDT({
    datatable(dados_filtrados())
  })

  
}

shinyApp(ui, server)
