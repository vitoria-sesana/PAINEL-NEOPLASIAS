ui_risco <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    hr(),
    plotlyOutput(ns("gráfico_risco")),
  )
  
}


server_risco <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
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
    
    
    output$gráfico_risco <- renderPlotly({
      
      ### elementos
      base_grafico_kp <- kaplan_meier()$base_grafico_kp
      ajuste_kp <- kaplan_meier()$ajuste_kp
      dados_surv <-  kaplan_meier()$dados_surv
      nome_tempo <- base_selecionada$tempo() 
      
      ggl <- plot_hazard_por_grupo(
        base_grafico_kp, 
        time_var = tempo, 
        status_var = indicadora, 
        group_var = covariavel)
      
      ggl
    })
    
  })
}