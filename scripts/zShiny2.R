library(shiny)

dados <- reactiveVal(mtcars)
covariaveis_numericas <- c("mpg", "hp", "wt")

ui <- fluidPage(
  selectInput("selecionar_covariavel", "Escolha a variável:", choices = names(mtcars)),
  tableOutput("tabela_filtrada")
)

server <- function(input, output, session) {
  corte <- reactiveVal(NULL)
  
  # SÓ APARECE O MODAL E SALVA O PONTO DE CORTE
  observeEvent(input$selecionar_covariavel, {
    var <- input$selecionar_covariavel
    
    if (var %in% covariaveis_numericas) {
      # pega os valores da variável selecionada
      valores <- dados()[[var]]
      min_val <- min(valores, na.rm = TRUE)
      max_val <- max(valores, na.rm = TRUE)
      
      # COLOCAR A FUNC PONTO DE CORTE
      # GRÁFICO PONTO DE CORTE
      
      showModal(
        modalDialog(
          title = paste("Variável numérica selecionada:", var),
          sliderInput("ponto_corte", "Escolha o ponto de corte:", 
                      min = min_val, max = max_val,
                      value = min_val, step = 0.1),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("confirmar_corte", "Confirmar")
          )
        )
      )
    } else {
      corte(NULL)
    }
  })
  
  # ATUALIZA O PONTO DE CORTE CASO APERTE O BOTAO CONFRIMAR DO SHOW MODAL
  observeEvent(input$confirmar_corte, {
    corte(input$ponto_corte)
    removeModal()
  })
  
  # SELEÇÃO DA BASE DE DADOS
  base_filtrada <- reactive({
    var <- input$selecionar_covariavel
    req(var)
    
    if (var %in% covariaveis_numericas && !is.null(corte())) {
      dados()[dados()[[var]] > corte(), ]
    } else if (!(var %in% covariaveis_numericas)) {
      subset(dados(), dados()[[var]] == max(dados()[[var]]))
    } else {
      dados()
    }
  })
  
  output$tabela_filtrada <- renderTable({
    base_filtrada()
  })
}

shinyApp(ui, server)
