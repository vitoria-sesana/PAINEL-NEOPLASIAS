SP1_selecao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    ## Input: Cid --------------------------------------------------------------
    selectInput(ns("INPUT_SP_CID"),
                "Escolha os CID's de interesse:",
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
  
    # # teste -------------------------------------------------------------------
    # output$texto_aaa <- renderText({
    #   req(data())
    #   req(formula_covariveis())
    #   # class(data())
    #   paste("Formula final:", formula_final())
    # })
    
    # OBSERVE: dados ---------------------------------------------------------------
    observe({
      req(data())
      
      ## OBSERVE: cids
      cids <- sort(unique(data()[["topogrup"]]))
      updateSelectInput(session, "INPUT_SP_CID", choices = cids, selected = "C38 - Coração, mediastino e pleura")
      
      # ## OBSERVE: colunas para covariavel
      # num_cols <- dicionario_nomes 
      # updateSelectInput(session, "INPUT_SP_COVARIAVEL", choices = num_cols, selected = "sexo")
      valid_cols <- dicionario_nomes[
        sapply(dicionario_nomes, function(col) {
          col_data <- data()[[col]]
          length(unique(col_data[!is.na(col_data)])) >= 2
        })
      ]
      
      updateSelectInput(session, "INPUT_SP_COVARIAVEL", choices = valid_cols, selected = "sexo")
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
    

  # ajuste do modelo cox ----------------------------------------------------

    modelo_cox <- reactive({
      req(formula_final())
      req(data())
      
      formula_escolhida <- as.formula(formula_final())
      
      modelo_cox_ajustado <- 
        survival::coxph(
          formula = formula_escolhida, 
          data = data(),
          ties = "efron") # ties=c("efron","breslow","exact")
      
      sumario <- summary(modelo_cox_ajustado)
      
      tabela_coeficientes <- sumario$coefficients 
        
      tabela_IC_razao_risco <- sumario$conf.int
      
      return(list(
        modelo_cox_ajustado = modelo_cox_ajustado,
        lista_cox_sumario = sumario,
        tabela_coeficientes = tabela_coeficientes,
        tabela_IC_razao_risco = tabela_IC_razao_risco
      ))
    })
    
    
    saida_SP2 <- reactive({
      req(formula_final())
      req(modelo_cox())
      
      formula_final_saida <- formula_final()
      modelo_cox_saida <- modelo_cox()
      
      list(
        formula_final = formula_final_saida,
        cox = modelo_cox_saida
      )
    })
    
    # saídas -------------------------------------------------------------------    
    return(saida_SP2)
  })
}


