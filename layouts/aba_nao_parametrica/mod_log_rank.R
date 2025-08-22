mod_log_rank_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "font-weight: bold; font-size: 24px; text-align: left;",
      textOutput(ns("texto_tabela_lg"))
    ),
    br(),
    
    # Input: tabela log-rank ---------------------------------------------------
    reactableOutput(ns("TABELA_LOG_RANK")),
    
    # Input: texto log-rank 1 classe -------------------------------------------
    textOutput(ns("TEXTO_LOG_RANK"))
  )
  
}


mod_log_rank_server <- function(id, saida_selecao_avancada) {
  moduleServer(id, function(input, output, session) {
    
    
    
    output$texto_tabela_lg <- renderText({
      
      "Resultado do teste de log-rank"
    })
    
    # Reactive: base para tabela log-rank --------------------------------------
    saida_log_rank <- reactive({
      
      base <- saida_selecao_avancada$data_selecionada_avancada() 
      
      ## Calculando log-rank
      if (length(unique(base$covariavel)) > 1) {
        tabela_log_rank <- 
          funcao_log_rank(
            base = base,
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
    output$TABELA_LOG_RANK <- renderReactable({
      req(saida_log_rank())
      reactable(
        saida_log_rank(),
        searchable = FALSE,
        filterable = FALSE,
        pagination = FALSE,  # Desativa paginação para evitar scroll
        highlight = TRUE,
        striped = TRUE,
        bordered = TRUE,
        defaultColDef = colDef(
          style = list(
            whiteSpace = "normal",     # Quebra linha se necessário
            overflow = "hidden",       # Oculta overflow
            textOverflow = "ellipsis"
          )
        ),
        style = list(
          width = "100%",             # Usa largura total do contêiner
          overflow = "hidden",        # Remove qualquer scroll
          boxSizing = "border-box"    # Evita passar da borda
        )
      )
    })
    
    # Render: texto log-rank 1 classe ------------------------------------------
    output$TEXTO_LOG_RANK <- renderText({
      if (is.null(saida_log_rank())) {
        "É necessário que a covariável selecionada tenha duas ou mais classes! Verifique a covariável selecionada e seus filtros."
      }
      
    })
  })
}