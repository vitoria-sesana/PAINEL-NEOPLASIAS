ui_tabela <- function(id) {
  ns <- NS(id)
  
  tagList(
    hr(),
    
    # Input: tabela selecionada ------------------------------------------------
    tableOutput(ns("tabela_selecionada")),
    
  )
  
}


server_tabela <- function(id, base_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    # Render: tabela log-rank --------------------------------------------------
    output$tabela_selecionada <- renderTable(
      base_selecionada$data() 
    )
    
  })
}