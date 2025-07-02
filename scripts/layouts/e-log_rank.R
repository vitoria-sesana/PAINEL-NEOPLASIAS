ui_log_rank <- function(id) {
  ns <- NS(id)
  
  tagList(
    hr(),
    verbatimTextOutput(ns("texto_corte")),
    tableOutput(ns("tabela_log_rank")),
    tableOutput(ns("tabela_base"))
  )
  
}


server_log_rank <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    base <- reactive({
      base_grafico_kp <- base_selecionada$data() 
      
      tabela_log_rank <- 
        teste_logrank_pares(
          base = base_grafico_kp,
          tempo = "tempo",
          evento = "indicadora",
          variavel = "covariavel"
          ) 
      
      colnames(tabela_log_rank) <-
        c(
          "Comparação",
          "Estatística do teste",
          "Graus de liberdade",
          "P-valor"
        )
      
      return(tabela_log_rank)
    })
    
    output$tabela_log_rank <- renderTable(base())
    
    output$tabela_base <- renderTable({

      # Pega a base de dados da reativa base()
      base_grafico_kp <- base_selecionada$data()

      head(base_grafico_kp)
    })

    output$texto_corte <- renderText({
      # is.null(base_selecionada$corte())
      class(base_selecionada$data())
    })
    
  })
}