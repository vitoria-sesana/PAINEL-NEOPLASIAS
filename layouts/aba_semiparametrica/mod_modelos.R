ui_modelos <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h2("Gráfico da Função de Sobrevivência Estimada pelo Modelo de Cox"),
    plotlyOutput(ns("gráfico_kp")),
    br(),
    hr(),
    h2("Tabela da Função de Sobrevivência Estimada pelo Modelo de Cox"),
    DT::DTOutput(ns("tabela_kp"))
  )
}

server_modelos <- function(id, saida_selecao, saida_selecao_avancada) {
  moduleServer(id, function(input, output, session) {
    
    kaplan_meier <- reactive({
      base_grafico_kp <- saida_selecao_avancada$data_selecionada_avancada()
      req(base_grafico_kp)
      
      base_grafico_kp$covariavel <- factor(base_grafico_kp$covariavel)
      
      ajuste_cox <- coxph(Surv(tempo, indicadora) ~ covariavel, data = base_grafico_kp)
      
      niveis_cov <- levels(base_grafico_kp$covariavel)
      newdata <- data.frame(covariavel = niveis_cov)
      
      curva_cox <- survfit(ajuste_cox, newdata = newdata)
      
      dados_surv <- surv_summary(curva_cox)
      
      dados_surv$grupo <- factor(dados_surv$strata)
      levels(dados_surv$grupo) <- sub("^covariavel=", "", levels(dados_surv$grupo))
      
      dados_surv <- dados_surv %>%
        mutate(
          surv = as.numeric(surv)
        )
      
      list(
        base_grafico_kp = base_grafico_kp,
        ajuste_kp = curva_cox,
        dados_surv = dados_surv
      )
    })
    
    output$tabela_kp <- DT::renderDT({
      dados <- kaplan_meier()$dados_surv
      
      dados %>%
        dplyr::rename(
          Tempo = time,
          Sobreviventes = n.risk,
          Eventos = n.event,
          Censuras = n.censor,
          Sobrevivência = surv,
          Grupo = grupo
        ) %>%
        DT::datatable(options = list(scrollX = TRUE), class = 'display nowrap')
    })
    
    output$gráfico_kp <- renderPlotly({
      base_grafico_kp <- kaplan_meier()$base_grafico_kp
      dados_surv <- kaplan_meier()$dados_surv
      nome_tempo <- saida_selecao$tempo_selecionado()
      
      p <- ggplot(dados_surv, aes(x = time, y = surv, color = grupo)) +
        geom_step(size = 1) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_x_continuous(limits = c(0, max(base_grafico_kp$tempo, na.rm = TRUE))) +
        labs(x = nome_tempo, y = "S(t) estimada", color = "") +
        theme_bw() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("x", "y", "grupo"))
    })
    
  })
}