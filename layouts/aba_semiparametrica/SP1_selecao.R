SP1_selecao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    ## Input: Cid --------------------------------------------------------------
    selectInput(ns("INPUT_SP_CID"),
                "Escolha o CID de interesse:",
                choices = NULL,
                multiple = TRUE),

    textOutput(ns("texto_aaa")),
    
    ## INPUT: Seleção das covariaveis ------
    
    selectInput(ns("INPUT_SP_COVARIAVEL"), 
                "Escolha as covariáveis de interesse:",
                choices = NULL,
                multiple = TRUE),
    
    ## Input: Tipo de tempo ----------------------------------------------------
    radioButtons(ns("INPUT_SP_TEMPO"),
                 "Selecione a unidade de medida da variável tempo de interesse:",
                 choices = c("Dias" = "tempo_dias",
                             "Semanas" = "tempo_semanas",
                             "Meses" = "tempo_meses",
                             "Anos" = "tempo_anos")),
    
  )
}

SP1_selecao_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    # teste -------------------------------------------------------------------
    output$texto_aaa <- renderText({
      req(data())
      req(formula_covariveis())
      # class(data())
      paste("Formula final:", formula_final())
    })
    
    # OBSERVE: dados ---------------------------------------------------------------
    observe({
      req(data())
      
      ## OBSERVE: cids
      cids <- sort(unique(data()[["topogrup"]]))
      
      updateSelectInput(session, "INPUT_SP_CID", choices = cids, selected = "C38 - Coração, mediastino e pleura")
      
      ## OBSERVE: colunas para covariavel
      num_cols <- dicionario_nomes 
      
      updateSelectInput(session, "INPUT_SP_COVARIAVEL", choices = num_cols, selected = "sexo")
    })
    
    
    # FÓRMULAS -----------------------------------------------------------------

    formula_cid <- reactive({
      as.character(input$INPUT_SP_CID)
    })
    
    formula_covariveis <- reactive({
      req(input$INPUT_SP_COVARIAVEL)
      paste(as.character(input$INPUT_SP_COVARIAVEL), collapse = " + ")
    })
    
    formula_tempo <- reactive({
      req(input$INPUT_SP_TEMPO)
      as.character(input$INPUT_SP_TEMPO)
    })
      
    formula_final <- reactive({
      req(formula_covariveis())
      req(formula_tempo())
      
      formula_cox_texto <- 
        paste0(
          "Surv(", 
          formula_tempo(), 
          ", indicadora) ~ ", 
          formula_covariveis()
          )
      
      
    })
    # SAÍDA --------------------------------------------------------------------    
    return(
      list(
        formula_selecionada = formula_final
      )
    )
  })
}


