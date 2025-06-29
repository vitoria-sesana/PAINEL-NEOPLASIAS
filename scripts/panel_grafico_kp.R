ui_grafico_kp <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textOutput(ns("texto")),
    plotlyOutput(ns("gráfico_kp")),
    br(),
    tableOutput(ns("tabela_kp"))
  )
  
}


server_grafico_kp <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    ## tabela ------------------------------------------------------------------
    
    output$tabela_kp <- renderTable(
      base_selecionada$data() %>% 
        head()
    )
    
    output$texto <- renderText(
      base_selecionada$classe()
    )
    
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
          data = base_grafico_kp)
      
      return(
        list(
          base_grafico_kp = base_grafico_kp,
          ajuste_kp = ajuste_kp,
          dados_surv = dados_surv
        )
      )
    })
    
    output$tabela_kp <- renderTable({
      req(kaplan_meier()$dados_surv)
      
      kaplan_meier()$dados_surv %>% 
        head() 
    })
    
    output$gráfico_kp <- renderPlotly({
      
      ### elementos
      base_grafico_kp <- kaplan_meier()$base_grafico_kp
      ajuste_kp <- kaplan_meier()$ajuste_kp
      dados_surv <-  kaplan_meier()$dados_surv
      nome_tempo <- base_selecionada$tempo() 
      valor_IC <- base_selecionada$IC() 
      
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
        
      } else {
        
        gg_kp <-
          ggplot(
            dados_surv,
            aes(x = time, y = surv, color = covariavel, fill = covariavel)
            ) +
          geom_step(
            size = 1
            ) +
          geom_ribbon(
            aes(
              ymin = lower,
              ymax = upper
              ),
            alpha = 0.2,
            color = NA
            ) +
          labs(
            x = nome_tempo,
            y = "S(t) estimada",
            color = "", fill = ""
          ) +
          scale_y_continuous(
            limits = c(0, 1), 
            breaks = seq(from = min(base_grafico_kp$tempo, na.rm = TRUE),
                         to = max(base_grafico_kp$tempo, na.rm = TRUE),
                         length.out = 7)
            
          ) +
          scale_x_continuous(limits = c(0, max(base_grafico_kp$tempo))) +
          theme_bw() +
          theme(legend.position = "bottom")
        
        # Converte para plotly com ICs visíveis
        ggplotly(gg_kp, tooltip = c("x", "y", "sexo"))
      }
      
    })
    
  })
}