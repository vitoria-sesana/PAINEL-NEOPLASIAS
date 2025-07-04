# Layout: gráfico da função de risco estimada -----------------------------

ui_risco <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    
    # Input: gráfico risco estimada -------------------------
    plotlyOutput(ns("gráfico_risco")),
  )
  
}


server_risco <- function(id, base_selecionada) {
  
  moduleServer(id, function(input, output, session) {
    
    # Render: gráfico risco estimada -------------------------
    output$gráfico_risco <- renderPlotly({
      base <- base_selecionada$data()
      ggl <- plot_hazard_por_grupo(
        base, 
        time_var = tempo, 
        status_var = indicadora, 
        group_var = covariavel)
      ggl
    })
    # sem sáídas ---------------------------------------------
  })
}