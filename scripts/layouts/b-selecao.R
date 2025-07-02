ui_selecao <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    "Texto de introdução explicando o que são estimativas não paramétricas",
    hr(),
    
    ## Input: Cid ----------------------------------------
    selectInput(
      ns("selecionar_cid"),
      "Selecione o CID:",
      choices = unique(base$topogrup)
    ),
    
    ## Input: Tipo de tempo ------------------------------
    radioButtons(
      ns("selecionar_tempo"), 
      "Selecione a unidade de medida da variável tempo de interesse:",
      choices = c("Dias" = "tempo_dias",
                  "Semanas" = "tempo_semanas",
                  "Meses" = "tempo_meses",
                  "Anos" = "tempo_anos")),
    
    ## Input: Covariável ---------------------------------
    selectInput(
      ns("selecionar_covariavel"),
      "Selecione a covariável de interesse",
      choices = c(
        colnames(
          base %>% 
            select(
              -topogrup,
              -dtdiag, 
              -dttrat,
              -dtultinfo,
              -ultinfo,
              -indicadora,
              -tempo_dias, -tempo_anos, -tempo_semanas, -tempo_meses))
      )
    ),
    
    ## Input: intervalo ----------------------------------
    radioButtons(
      ns("selecionar_intervalo"), 
      "Com ou sem intervalo?:",
      choices = c("Com IC" = TRUE,
                  "Sem IC" = FALSE))
    
    # checkboxInput(inputId = ns("teste_01"), "Intervalo de confiança"),
    # verbatimTextOutput(ns("aa")),
  )
  
}


server_selecao <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## Reactive: tratamento da base de dados -----------------------------------

    selected_data <- reactive({
      req(input$selecionar_cid)
      req(input$selecionar_tempo)
      req(input$selecionar_covariavel)
      
      base %>% 
        filter(topogrup == as.character(input$selecionar_cid)) %>% # CID
        select(
          input$selecionar_tempo, # TEMPO
          input$selecionar_covariavel, # COVARIAVEL
          indicadora
          ) %>% 
        rename(
          tempo = input$selecionar_tempo,
          covariavel = input$selecionar_covariavel
          ) %>% 
        mutate(tempo = round(tempo, 0))
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


    ## Observe: Ponto de corte -------------------------------------------------
  
    observeEvent(input$selecionar_covariavel, {
      if (input$selecionar_covariavel %in% covariaveis_numericas) {
        showModal(
          modalDialog(
            title = "Alerta: Variável com múltiplas categorias!",
            sliderInput("oscars", "Minimum number of Oscar wins (all categories)",
                        0, 4, 0, step = 1),
            verbatimTextOutput("saida_modal"),
            easyClose = TRUE
          )
        )
      }})
    # 
    # observeEvent(input$selecionar_covariavel, {
    #   if (input$selecionar_covariavel %in% covariaveis_numericas) {
    #     return(TRUE)
    #   } else {
    #     returno(FALSE)
    #   }
    #   
    #   })
    
    ## Intervalo de confiança --------------------------------------------------
    
    escolha_intervalo_confianca <- reactive({

      req(input$selecionar_intervalo)

      input$selecionar_intervalo

    })
    
    # escolha_intervalo_confianca <- reactive({
    #   req(input$teste_01)
    #   input$teste_01
    # })
    # 
    # output$aa <- renderText({input$teste_01})
    
    
    ## saída -------------------------------------------------------------------
    
    return(
      list(
        data = selected_data,
        covariavel = nome_covariavel,
        classe = classe_covariavel,
        tempo = nome_tempo,
        IC = escolha_intervalo_confianca
      )
    )
    
  })
}