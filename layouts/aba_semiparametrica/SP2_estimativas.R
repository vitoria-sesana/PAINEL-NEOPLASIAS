SP2_cox_estimativas_ui <- function(id) {
  ns <- NS(id)
  tagList(

    br(),

    # formula -----------------------------------------------------------------
    # div(
    #   style = "font-weight: bold; font-size: 24px; text-align: center;",
    #   textOutput(ns("texto_formula"))
    # ),
    # hr(),
    
    div(
      style = "font-weight: bold; font-size: 24px; text-align: left;",
      textOutput(ns("texto_tabela"))
    ),
    br(),
    "O modelo de regressão de Cox é utilizado na análise de sobrevivência para entender a relação entre uma variável dependente de tempo e várias covariáveis. Ele é baseado no conceito de riscos proporcionais, onde o risco de um evento ocorrer em um determinado momento depende de características (covariáveis) dos indivíduos.",
    h3("Razão de Risco (RR) e sua interpretação:"),
    "A Razão de Risco (RR) é a medida mais importante no modelo de Cox. Ela representa a taxa de risco de ocorrência de um evento para um grupo comparado a outro, dada a covariável.",
    tags$ul(
      tags$li("RR = 1: Não há diferença no risco entre os grupos."),
      tags$li("RR > 1: O risco do evento aumenta à medida que a covariável aumenta."),
      tags$li("RR < 1: O risco do evento diminui à medida que a covariável aumenta.")
    ),
    br(),
    br(),
    # tabela coeficientes -----------------------------------------------------
    reactableOutput(ns("tabela_coeficientes")),

    # tabela intervalo de confiança -------------------------------------------
    hr(),
    br(),
    "O Forest Plot é uma representação gráfica comumente usada para visualizar os resultados de modelos de regressão de Cox e outras análises estatísticas. Ele é frequentemente utilizado para mostrar a Razão de Risco (RR), juntamente com os intervalos de confiança (IC), permitindo uma comparação rápida dos efeitos das covariáveis em diferentes grupos ou estudos.",
    br(),
    br(),
    plotOutput(ns("grafico_RR")),
    br(),
    br(),
  )
}


SP2_cox_estimativas_server <- function(id, saida_cox) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

# formula -----------------------------------------------------------------
    output$texto_formula <- renderText({
      req(saida_cox())
      paste0("Fórmula selecionada: ", saida_cox()$formula_final)
    })
    

# titulo tabela -----------------------------------------------------------
    
    output$texto_tabela <- renderText({
      
      "Resultado do ajuste do modelo de regressão de Cox"
    })
    
# tabela resultados regressão ---------------------------------------------
    tabela_coeficientes <- reactive({
      req(saida_cox)
      
      tabela_coeficientes <- 
        saida_cox()$cox$tabela_coeficientes %>%
        round(4) %>%
        as.data.frame() %>% 
        rownames_to_column() %>% 
        janitor::clean_names() %>% 
        mutate(x = exp_coef) %>%
        select(-exp_coef)
      
      
      tabela_ic <- 
        saida_cox()$cox$tabela_IC_razao_risco %>% 
        round(4) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        janitor::clean_names() %>%
        mutate(
          variavel = str_extract(rowname, "^[a-z]+"),
          nivel = str_extract(rowname, "[A-Z].*"),
          covariaveis = paste0(str_to_title(variavel), ": ", nivel),
          IC = paste0("(", round(lower_95, 2), "; ", round(upper_95, 2), ")")
        ) %>% 
        select(
          rowname,
          covariaveis,
          exp_coef,
          exp_coef_2,
          IC,
          variavel, 
          nivel
        )
      
      
      tabela_final <- 
        left_join(
          tabela_coeficientes,
          tabela_ic,
          by = "rowname"
        ) %>% 
        mutate(
          p = pr_z  ,
          interpretacao = case_when(
            exp_coef < 1 & p < 0.05 ~ paste0("Grupo ", nivel, " tem ", round((1 - exp_coef) * 100, 0), "% menos risco que o grupo referência (", variavel, "1). Significativo."),
            exp_coef < 1 & p >= 0.05 ~ paste0("Grupo ", nivel, " tem ", round((1 - exp_coef) * 100, 0), "% menos risco que o grupo referência (", variavel, "1), mas o resultado não é significativo (p = ", round(p, 4), ")."),
            exp_coef > 1 & p < 0.05 ~ paste0("Grupo ", nivel, " tem ", round((exp_coef - 1) * 100, 0), "% mais risco que o grupo referência (", variavel, "1). Significativo."),
            exp_coef > 1 & p >= 0.05 ~ paste0("Grupo ", nivel, " tem ", round((exp_coef - 1) * 100, 0), "% mais risco que o grupo referência (", variavel, "1), mas o resultado não é significativo (p = ", round(p, 4), ")."),
            TRUE ~ "Interpretação não disponível."
          )
        ) %>% 
        select(
          covariaveis, 
          coef, 
          se_coef,
          exp_coef,
          exp_coef_2,
          IC,
          z,
          pr_z,
          interpretacao
        )  
      
      colnames(tabela_final) <-
        c(
          "Covariáveis",
          "Estimativa",
          "Erro-Padrão",
          "RR",
          "1/RR",
          "IC(RR, 95%)",
          "Estatística",
          "P-Valor",
          "Interpretação"
          )
      
      return(tabela_final)
    })
    
    output$tabela_coeficientes <- renderReactable({
      req(tabela_coeficientes())
      reactable(
        tabela_coeficientes(),
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
    
    
    output$grafico_RR <- renderPlot({
      req(saida_cox())
      
      model <- saida_cox()$cox$modelo_cox_ajustado
      ci2 <- confint(model); hr2 <- exp(coef(model))
      hr_tab2 <- data.frame(
        term = names(hr2),
        HR   = as.numeric(hr2),
        L95  = exp(ci2[,1]),
        U95  = exp(ci2[,2])
      )
      
      ggplot(hr_tab2, aes(y = term, x = HR)) +
        geom_point() +
        geom_errorbarh(aes(xmin = L95, xmax = U95), height = 0.15) +
        geom_vline(xintercept = 1, linetype = "dashed") +
        labs(title = "",
             x = "Razão de Risco (IC 95%)", y = NULL) +
        theme_bw()
      
    })
  })
}


