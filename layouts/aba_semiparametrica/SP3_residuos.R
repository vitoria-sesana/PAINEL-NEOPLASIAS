SP3_riscos_proporcionais_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "A suposição de riscos proporcionais significa que o efeito das covariáveis sobre o risco é constante ao longo do tempo — ou seja, a razão de riscos entre dois indivíduos não muda com o tempo.",
  )
}


SP3_riscos_proporcionais_server <- function(id, saida_cox) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # teste -------------------------------------------------------------------
    
    # output$teste_xx <- renderText({
    #   paste(class(as.data.frame(saida_cox()$cox$tabela_coeficientes)), 
    #         colnames(as.data.frame(saida_cox()$cox$tabela_coeficientes)))
    # })

  })
}


