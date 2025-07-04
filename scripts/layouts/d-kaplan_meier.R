ui_kaplan_meier <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    br(),
    h2("Gráfico da Função de Sobrevivência Estimada por Kaplan-Meier"),
    plotlyOutput(ns("gráfico_kp")),
    br(),
    hr(),
    h2("Tabela da Função de Sobrevivência Estimada por Kaplan-Meier"),
    DT::DTOutput(ns("tabela_kp"))
  )
  
}

server_kaplan_meier <- function(id, base_selecionada, base_inicial) {
  moduleServer(id, function(input, output, session) {
    
    ## tabela kaplan-meier -----------------------

    kaplan_meier <- reactive({
      
      ### elementos
      base_grafico_kp <- base_selecionada$data() 
      
      ## estimando kaplan-meier -----------------------
      ajuste_kp <- 
        survfit(
          Surv(
            time = tempo, 
            event = indicadora) ~ 
            covariavel, 
          data = base_grafico_kp
        )
      
      ## tabela para intervalo de confiança ------------
      dados_surv <- 
        surv_summary(
          ajuste_kp, 
          data = base_grafico_kp) %>% 
        mutate(
          surv = formatC(surv, digits = 2),
          std.err = formatC(std.err, digits = 2),
          upper = formatC(upper, digits = 2),
          lower = formatC(lower, digits = 2)
        ) 

      if ("strata" %in% names(dados_surv)) {
        dados_surv <- dados_surv[, !(names(dados_surv) %in% "strata")]
      }
      
      return(
        list(
          base_grafico_kp = base_grafico_kp,
          ajuste_kp = ajuste_kp,
          dados_surv = dados_surv
        )
      )
    })
    
    
    output$tabela_kp <- DT::renderDT({
      dados <- kaplan_meier()$dados_surv 
      dados <- dados %>% 
        dplyr::rename(
          Tempo = time,
          Sobreviventes = n.risk,
          Eventos = n.event,
          Censuras = n.censor,
          `Sobrevivência` = surv,
          `Erro Padrão` = std.err,
          `Limite Superior` = upper,
          `Limite Inferior` = lower,
          `Covariável` = covariavel
        )
      
      DT::datatable(dados, options = list(scrollX = TRUE), class = 'display nowrap')
    })
    

    # Gráfico: Kaplan-Meier ---------------------------------------------------
    
    output$gráfico_kp <- renderPlotly({
      
      ### elementos
      base_grafico_kp <- kaplan_meier()$base_grafico_kp
      ajuste_kp <- kaplan_meier()$ajuste_kp
      dados_surv <-  kaplan_meier()$dados_surv
      nome_tempo <- base_inicial$tempo() 
      valor_IC <- base_inicial$IC() 
      
      ## Sem Intervalo de confiança -------
      if (valor_IC == FALSE) {
        
        gg_kp <- 
          ggsurvplot(
            ajuste_kp,
            data = base_grafico_kp,
            conf.int = TRUE,
            conf.int.style = "ribbon",
            ylab = "S(t) estimada",
            xlab = nome_tempo, 
            legend.title = "",
            ggtheme = theme_bw()
            )
        
        gg_kp$plot <- 
          gg_kp$plot +
          scale_y_continuous(
            limits = c(0, 1), 
            breaks = 
              seq(
                from = min(base_grafico_kp$tempo, na.rm = TRUE),
                to = max(base_grafico_kp$tempo, na.rm = TRUE),
                length.out = 7)
            ) +
          scale_x_continuous(
            limits = c(0, max(base_grafico_kp$tempo))
            ) +
          theme(legend.position = "bottom")
        
        gg_kp_saida <- 
          ggplotly(
            gg_kp$plot,
            tooltip = c("x", "y")
            )
        
        gg_kp_saida
        
      }
      
      ## Com Intervalo de confiança -------
      
      else {
        if ("covariavel" %in% colnames(dados_surv)) {
        
          # Modelo Kaplan-Meier
          fit <- survfit(Surv(tempo, indicadora) ~ covariavel, 
                         data = base_grafico_kp)
          
          # Extrair dados do objeto survfit para data frame
          df_km <- survminer::surv_summary(ajuste_kp, data = base_grafico_kp)
          
          # Criar gráfico com ggplot
          p <- ggplot(df_km, aes(x = time, y = surv, color = covariavel, fill = covariavel)) +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
            geom_step() +
            scale_y_continuous(limits = c(0, 1)) +
            scale_x_continuous(limits = c(0, max(base_grafico_kp$tempo))) +
            labs(x = nome_tempo, y = "S(t) estimada", color = "", fill = "") +
            theme_bw() +
            theme(legend.position = "bottom")
          
          # Tornar interativo com plotly
          gg_kp_saida <- ggplotly(p, tooltip = c("x", "y", "covariavel"))
          

        }
        else{
          fit <- survfit(Surv(tempo, indicadora) ~ 1, 
                         data = base_grafico_kp)
          
          df_km <- survminer::surv_summary(ajuste_kp, data = base_grafico_kp)
          
          p <- ggplot(df_km, aes(x = time, y = surv, color = "blue")) +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
            geom_step() +
            scale_y_continuous(limits = c(0, 1)) +
            scale_x_continuous(limits = c(0, max(base_grafico_kp$tempo))) +
            labs(x = nome_tempo, y = "S(t) estimada", color = "", fill = "") +
            theme_bw() +
            theme(legend.position = "bottom")
          
          gg_kp_saida <- ggplotly(p, tooltip = c("x", "y"))
          
        }
      }
      
    })
    
  })
}