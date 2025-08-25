mod_log_rank_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    div(
      style = "font-weight: bold; font-size: 24px; text-align: left;",
      textOutput(ns("texto_tabela_lg"))
    ),
    br(),
    "O teste de Log-Rank é utilizado para comparar as curvas de sobrevivência entre dois ou mais grupos, verificando se há diferenças estatisticamente significativas na probabilidade de ocorrência do evento ao longo do tempo. Como em casos onde não há mudança brusca na probabilidade de sobrevivência entre grupos.",
    br(),
    br(),
    # Input: tabela log-rank ---------------------------------------------------
    reactableOutput(ns("TABELA_LOG_RANK")),
    br(),
    "Rejeita-se a hipótese nula e conclui-se que as curvas de sobrevivência são significativamente diferentes quando o valor-p do teste de Log-Rank é menor que 0,05.",
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
        pagination = TRUE,
        highlight = TRUE,
        striped = TRUE,
        bordered = TRUE,
        style = list(
          maxHeight = "400px",   # ou qualquer valor em px/vh/rem
          overflowY = "auto"
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