ui_log_rank <- function(id) {
  ns <- NS(id)
  
  tagList(
    hr(),
    
    # Input: tabela log-rank ---------------------------------------------------
    tableOutput(ns("tabela_log_rank")),
    
    # Input: texto log-rank 1 classe -------------------------------------------
    textOutput(ns("texto_log_rank"))
  )
  
}


server_log_rank <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive: base para tabela log-rank --------------------------------------
    base <- reactive({
      
      base_log_rank <- base_selecionada$data() 
      
      ## Calculando log-rank
      if (length(unique(base_log_rank$covariavel)) > 1) {
        tabela_log_rank <- 
          funcao_logrank(
            base = base_log_rank,
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
      } else {
        NULL
      }
    })
    
    # Render: tabela log-rank --------------------------------------------------
    output$tabela_log_rank <- renderTable(
      base()
      )
    
    # Render: texto log-rank 1 classe ------------------------------------------
    output$texto_log_rank <- renderText({
      if (is.null(base())) {
        "É necessário que a covariável selecionada tenha duas ou mais classes! Verifique a covariável selecionada e seus filtros."
      }
      
    })
  })
}