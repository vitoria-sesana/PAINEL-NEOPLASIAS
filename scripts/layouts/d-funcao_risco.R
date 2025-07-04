ui_risco <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    hr(),
    plotlyOutput(ns("gráfico_risco")),
  )
  
}


server_risco <- function(id, base_selecionada) {
  
  moduleServer(id, function(input, output, session) {
    
    output$gráfico_risco <- renderPlotly({
      base <- base_selecionada$data()
      ggl <- plot_hazard_por_grupo(
        base, 
        time_var = tempo, 
        status_var = indicadora, 
        group_var = covariavel)
      ggl
    })
    
  })
}