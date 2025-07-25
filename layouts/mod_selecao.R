# mod_histograma.R


# UI ----------------------------------------------------------------------

mod_selecao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    ## Input: Cid --------------------------------------------------------------
    selectInput(ns("INPUT_CID"), 
                "Escolha a variável:",
                choices = NULL),
    
    ## Input: Covariável -------------------------------------------------------
    selectInput(ns("INPUT_COVARIAVEL"), 
                "Escolha a variável:",
                choices = NULL),
    ## ponto de corte
    textOutput(ns("TEXTO_PONTO_DE_CORTE")),
    ## Input: Tipo de tempo ----------------------------------------------------
    radioButtons(ns("INPUT_TEMPO"),
                 "Selecione a unidade de medida da variável tempo de interesse:",
                 choices = c("Dias" = "tempo_dias",
                             "Semanas" = "tempo_semanas",
                              "Meses" = "tempo_meses",
                              "Anos" = "tempo_anos")),
    
    ## Input: intervalo --------------------------------------------------------
    radioButtons(ns("INPUT_IC"), 
                 "Intervalo de confiança", 
                 choices = c("Sim" = TRUE, "Não" = FALSE), 
                 selected = FALSE)
  )
}



# SERVER ------------------------------------------------------------------

mod_selecao_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

# OBSERVE: dados ---------------------------------------------------------------
    observe({
      req(data())
      
      ## OBSERVE: cids
      cids <- sort(unique(data()[["topogrup"]]))
      
      updateSelectInput(session, "INPUT_CID", choices = cids, selected = "C38 - Coração, mediastino e pleura")
      
      ## OBSERVE: colunas para covariavel
      num_cols <- dicionario_nomes 
      
      updateSelectInput(session, "INPUT_COVARIAVEL", choices = num_cols, selected = "sexo")
    })
    


# SELEÇÃO -----------------------------------------------------------------

    
    cid_selecionado <- reactive({
      as.character(input$INPUT_CID)
    })
    
    covariavel_selecionada <- reactive({
      req(input$INPUT_COVARIAVEL)
      as.character(input$INPUT_COVARIAVEL)
    })
    
    tempo_selecionado <- reactive({
      req(input$INPUT_TEMPO)
      as.character(input$INPUT_TEMPO)
    })
    
    ic_selecionado <- reactive({
      req(input$INPUT_IC)
      as.character(input$INPUT_IC)
    })
    
    nome_tempo <- reactive({
      req(input$INPUT_TEMPO)
      
      case_when(
        input$INPUT_TEMPO == "tempo_dias" ~ "Tempo (dias)",
        input$INPUT_TEMPO == "tempo_semanas" ~ "Tempo (semanas)",
        input$INPUT_TEMPO == "tempo_meses" ~ "Tempo (meses)",
        input$INPUT_TEMPO == "tempo_anos" ~ "Tempo (anos)",
      )
    })
    
    nome_covariavel <- reactive({
      req(input$INPUT_COVARIAVEL)
      as.character(input$INPUT_COVARIAVEL)
    })
    
# SELECIONANDO A BASE -----------------------------------------------------

    data_selecionada <- reactive({
      req(covariavel_selecionada())
      req(tempo_selecionado())
      req(cid_selecionado())
      req(data())

      data() %>%
        filter(topogrup == as.character(cid_selecionado())) %>%
        select(
          all_of(c(
            tempo_selecionado(),
            covariavel_selecionada(),
            "indicadora",
            # caracteristicas sociais
            "faixaetar",
            "sexo",
            "escolari",
            "ufnasc",
            "ufresid",
            # caracteristicas dos tumores
            "filtro_subtopo",
            "ec",
            "ecgrup",
            "dsccido",

            # tumor infantil
            "cici",
            "cicigrup",
            "cicisubgru",

            # tratamento
            "naotrat",
            "tratamento",
            "trathosp",

            # diagnostico
            "clinica",
            "diagprev",
            "basediag",
            "anodiag",
            "cateatend",

            # habilitação,
            "habilit",
            "habilit1",

            # recidiva
            "recnenhum",
            "reclocal",
            "recregio",
            "recdist"
          ))
        ) %>%
        mutate(
          tempo = round(.data[[tempo_selecionado()]], 0),
          covariavel = .data[[covariavel_selecionada()]]
        )
    })
    

    # PONTO DE CORTE ----------------------------------------------------------
    
    # SHOWMODAL: --------------------------------------------------------------
    
    corte <- reactiveVal("sem")
    
    observeEvent(input$INPUT_COVARIAVEL, {
      covariavel <- nome_covariavel()
      base <- data_selecionada()
      
      if (covariavel %in% covariaveis_numericas) {
        # pega os valores da variável selecionada
        valores <- base[[covariavel]]
        min_val <- min(valores, na.rm = TRUE)
        max_val <- max(valores, na.rm = TRUE)
        
        ## Calcula: ponto de corte -----
        corte_sugerido <-
          func_ponto_corte(
            base = base,
            tempo = "tempo",
            evento = "indicadora",
            variavel_continua = "covariavel"
          )
        
        ## ShowModal: pontode corte ----------------
        showModal(
          modalDialog(
            title = paste("Variável numérica selecionada:", covariavel),
            
            plotlyOutput(ns("GRAFICO_PONTO_DE_CORTE")),  
            
            ## Input: ponto de corte ------
            sliderInput(
              ns("PONTO_DE_CORTE"),
              "Escolha o ponto de corte:", 
              min = min_val,
              max = max_val,
              value = corte_sugerido$estimate, 
              step = 1),
            
            ## botão de sair do modal
            footer = tagList(
              modalButton("Cancelar"),
              actionButton(ns("CONFIRMAR_CORTE"), "Confirmar")
            )
          )
        )
        
        ## gráfico ponto de corte
        output$GRAFICO_PONTO_DE_CORTE <- renderPlotly({
          func_ponto_corte_grafico(corte_sugerido)
        })
      } else {
        corte("sem")
      }
      
    })
    # ATUALIZA O PONTO DE CORTE CASO APERTE O BOTAO CONFRIMAR DO SHOW MODAL
    observeEvent(input$CONFIRMAR_CORTE, {
      corte(input$PONTO_DE_CORTE)
      removeModal()
    })
    
    # SELEÇÃO DA BASE DE DADOS
    data_selecionada_com_corte <- reactive({
      data <- data_selecionada()
      corte <- corte()
      req(data)
      req(corte)
  
        data <-
          data %>%
          mutate(
            covariavel = case_when(
              covariavel <= corte ~ paste("Menor ou igual à", corte),
              covariavel > corte ~ paste("Maior que", corte)
            )
          )
        
        return(data)
      })
    
    output$TEXTO_PONTO_DE_CORTE <- renderText({
      if (corte() != "sem") paste("Ponto de corte utilizado:", corte()) 
    })
    
    data_saida <- reactive({
      if (corte() != "sem") data_selecionada_com_corte() else data_selecionada()
    })
    
    # SAÍDA --------------------------------------------------------------------    
    return(
      list(
        data_selecionada = data_saida,
        cid_selecionado = cid_selecionado,
        covariavel_selecionada = covariavel_selecionada,
        tempo_selecionado = tempo_selecionado,
        ic_selecionado = ic_selecionado,
        nome_tempo = nome_tempo
        )
      )
  })
}
