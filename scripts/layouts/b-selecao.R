ui_selecao <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    "Texto de introdução explicando o que são estimativas não paramétricas",
    hr(),
    
    ## Input: Cid ----------------------------------------
    # selectInput(
    #   ns("selecionar_cid"),
    #   "Selecione o CID:",
    #   choices = unique(base$topogrup)
    # ),
    
    selectizeInput(
      ns("selecionar_cid"),
      "Selecione o CID:",
      choices = unique(base$topogrup),
      multiple = TRUE
    ),
    
    ## Input: Covariável ---------------------------------
    selectInput(
      ns("selecionar_covariavel"),
      "Selecione a covariável de interesse",
      choices = c(
        colnames(
          base %>% 
            select(
              -topogrup,
              -filtro_subtopo,
              # -dtdiag, 
              # -dttrat,
              # -dtultinfo,
              # -ultinfo,
              -indicadora,
              -tempo_dias, -tempo_anos, -tempo_semanas, -tempo_meses))
      )
    ),
    
    textOutput(ns("texto_cov")),
    
    ## Input: Tipo de tempo ------------------------------
    radioButtons(
      ns("selecionar_tempo"), 
      "Selecione a unidade de medida da variável tempo de interesse:",
      choices = c("Dias" = "tempo_dias",
                  "Semanas" = "tempo_semanas",
                  "Meses" = "tempo_meses",
                  "Anos" = "tempo_anos")),
    
    ## Input: intervalo ----------------------------------
    checkboxInput(ns("selecionar_intervalo"), "Intervalo de confiança", FALSE)
  )
  
}


server_selecao <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    # texto ponto de corte
    output$texto_cov <- renderText({
      req(corte())
      paste(corte(), is.null( corte()))
    })

  ## Reactive: tratamento da base de dados -----------------------------------

    base_selecionada_sem_corte <- reactive({
      req(input$selecionar_cid)
      req(input$selecionar_tempo)
      req(input$selecionar_covariavel)
      
      base %>%
        filter(topogrup == as.character(input$selecionar_cid)) %>%
        select(
          all_of(c(
            input$selecionar_tempo,
            input$selecionar_covariavel,
            "indicadora",
            "faixaetar",
            "sexo",
            "escolari",
            "ufnasc",
            "ufresid"
          ))
        ) %>%
        mutate(
          tempo = round(.data[[input$selecionar_tempo]], 0),
          covariavel = .data[[input$selecionar_covariavel]]
        )
    })
    
# Ponto de corte ---------------------------------------------------------------
    
    corte <- reactiveVal(NULL)
    dados <- reactiveVal(base)
    
    # SÓ APARECE O MODAL E SALVA O PONTO DE CORTE
    observeEvent(input$selecionar_covariavel, {
      covariavel_selecionada <- input$selecionar_covariavel
      base_corte <- base_selecionada_sem_corte()
      
      if (covariavel_selecionada %in% covariaveis_numericas) {
        # pega os valores da variável selecionada
        valores <- dados()[[covariavel_selecionada]]
        min_val <- min(valores, na.rm = TRUE)
        max_val <- max(valores, na.rm = TRUE)
        
        ## Calcula: ponto de corte -----
        corte_sugerido <-
          func_ponto_corte(
            base = base_corte,
            tempo = "tempo",
            evento = "indicadora",
            variavel_continua = "covariavel"
        )
        
        ## ShowModal: pontode corte ----------------
        showModal(
          modalDialog(
            title = paste("Variável numérica selecionada:", covariavel_selecionada),
            
            plotlyOutput(ns("grafico_ponto_corte")),  
            
            ## Input: ponto de corte ------
            sliderInput(
              ns("ponto_corte"),
              "Escolha o ponto de corte:", 
              min = min_val,
              max = max_val,
              value = corte_sugerido$estimate, 
              step = 1),
            
            ## botão de sair do modal
            footer = tagList(
              modalButton("Cancelar"),
              actionButton(ns("confirmar_corte"), "Confirmar")
            )
          )
        )
        
        ## gráfico ponto de corte
        output$grafico_ponto_corte <- renderPlotly({
          func_ponto_corte_grafico(corte_sugerido)
        })
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
    base_selecionada_com_corte <-
      reactive({
        req(base_selecionada_sem_corte())
        req(corte())

      base <- base_selecionada_sem_corte()
      corte <- round(corte(),0)

      if (is.null(corte())) {
        return(base_selecionada_sem_corte())
      } else {
        base <-
          base %>%
          mutate(
            covariavel = case_when(
              covariavel <= corte ~ paste("Menor ou igual à", corte),
              covariavel > corte ~ paste("Maior que", corte)
            )
          )

        return(base)
      }
      return(base)
    })
    
    # Reactive: informação da vaariável tempo ---------------------------------
    
    nome_tempo <- reactive({
      req(input$selecionar_tempo)
      
      case_when(
        input$selecionar_tempo == "tempo_dias" ~ "Tempo (dias)",
        input$selecionar_tempo == "tempo_semanas" ~ "Tempo (semanas)",
        input$selecionar_tempo == "tempo_meses" ~ "Tempo (meses)",
        input$selecionar_tempo == "tempo_anos" ~ "Tempo (anos)",
      )
    })

    ## validação da covariável -------------------------------------------------
    
    #### nome da covariável
    nome_covariavel <- reactive({
      req(input$selecionar_covariavel)
      as.character(input$selecionar_covariavel)
    })

    #### classe da covariável
    classe_covariavel <- reactive({
      
      req(input$selecionar_covariavel)
      
      quantidade_elementos_covariavel <- 
        base %>% 
        select(input$selecionar_covariavel) %>% 
        unique() %>% 
        nrow()
    })

    ## Intervalo de confiança --------------------------------------------------

    escolha_intervalo_confianca <- reactive({
      isTRUE(input$selecionar_intervalo) 
    })
    
    data <- reactive({
      if (!is.null(corte())) {
        base_selecionada_com_corte()
      } else {
        base_selecionada_sem_corte()
      }
    })
    
    ## saída -------------------------------------------------------------------
    
    return(
      list(
        data = data,
        covariavel = nome_covariavel,
        classe = classe_covariavel,
        tempo = nome_tempo,
        IC = escolha_intervalo_confianca,
        corte = corte
      )
    )
    
  })
}