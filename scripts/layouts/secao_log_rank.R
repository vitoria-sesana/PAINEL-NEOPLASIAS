ui_log_rank <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    hr(),
    tableOutput(ns("tabela_log_rank")),
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
    
    
    
    # output$tabela_log_rank <- renderTable({
    # 
    #   # Pega a base de dados da reativa base()
    #   base_grafico_kp <- base()$base_grafico_kp
    # 
    #   head(base_grafico_kp)
    #   # Executa o teste log-rank par a par
    #   teste_logrank_pares(
    #     base = base_grafico_kp,
    #     tempo = "tempo",
    #     evento = "indicadora",
    #     variavel = "covariavel"
    #   )
    # })
    
  })
}