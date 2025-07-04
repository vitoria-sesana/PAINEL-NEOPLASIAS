ui_tabela <- function(id) {
  ns <- NS(id)
  
  tagList(
    hr(),
    
    # Input: tabela selecionada ------------------------------------------------
    DT::DTOutput(ns("tabela_selecionada")),
    
  )
  
}


server_tabela <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    # Render: tabela log-rank --------------------------------------------------
    output$tabela_selecionada <- DT::renderDT(
      base_selecionada$data(),
      options = list(scrollX = TRUE),
      class = 'display nowrap'

    )
    
  })
}