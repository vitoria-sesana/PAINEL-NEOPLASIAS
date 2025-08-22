SP2_cox_estimativas_ui <- function(id) {
  ns <- NS(id)
  tagList(

    hr(),

    # formula -----------------------------------------------------------------
    # div(
    #   style = "font-weight: bold; font-size: 24px; text-align: center;",
    #   textOutput(ns("texto_formula"))
    # ),
    # hr(),
    
    div(
      style = "font-weight: bold; font-size: 16px; text-align: left;",
      textOutput(ns("texto_tabela"))
    ),
    br(),
    # tabela coeficientes -----------------------------------------------------
    reactableOutput(ns("tabela_coeficientes")),

    # tabela intervalo de confiança -------------------------------------------

    hr(),
    
    plotOutput(ns("grafico_RR"))
    
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
          "Z",
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
        labs(title = "Forest plot – Cox (2 covariáveis)",
             x = "Hazard Ratio (IC 95%)", y = NULL) +
        theme_bw()
      
    })
  })
}


