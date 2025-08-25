SP3_riscos_proporcionais_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    div(
      style = "font-weight: bold; font-size: 24px; text-align: left;",
      textOutput(ns("texto_tabela_ar"))
    ),
    br(),
    "A suposição de riscos proporcionais significa que o efeito das covariáveis sobre o risco é constante ao longo do tempo — ou seja, a razão de riscos entre dois indivíduos não muda com o tempo.",
    br(),
    br(),
    reactableOutput(ns("tabela_riscos_proporcionais")),
  )
}

SP3_riscos_proporcionais_server <- function(id, saida_cox) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$texto_tabela_ar <- renderText({
      
      "Análises das suposições de riscos proporcionais"
    })
    
    tabela_riscos_proporcionais <- reactive({
      req(saida_cox())
      
      modelo_cox <- saida_cox()$cox$modelo_cox_ajustado 
      
      teste_schoenfeld <- cox.zph(modelo_cox) 
      resultado <- teste_schoenfeld$table %>% as.data.frame() %>% rownames_to_column()
      colnames(resultado) <- c("Covariável", "Estatística", "Graus de liberdade", "P-valor")
      
      resultado <- resultado %>% 
        mutate(`Estatística` = round(`Estatística`, 4),
               `P-valor` = round(`P-valor`, 4)
               )
      return(resultado)
    })
    
    output$tabela_riscos_proporcionais <- renderReactable({
      req(tabela_riscos_proporcionais())
      reactable(
        tabela_riscos_proporcionais(),
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
    
    # output$analise_riscos <- renderUI({
    #   req(saida_cox())
    #   
    #   # Rodar o cox.zph uma vez
    #   ph2 <- cox.zph(saida_cox()$cox$modelo_cox_ajustado, transform = "km")
    #   
    #   # Um gráfico para cada covariável testada
    #   variaveis <- rownames(ph2$table)
    #   
    #   plot_output_list <- lapply(seq_along(variaveis), function(i) {
    #     plotOutput(ns(paste0("plot_", i)))
    #   })
    #   
    #   do.call(tagList, plot_output_list)
    # })
    # 
    # Criar os plots dinamicamente
    # observe({
    #   req(saida_cox())
    #   
    #   ph2 <- cox.zph(saida_cox()$cox$modelo_cox_ajustado, transform = "km")
    #   variaveis <- rownames(ph2$table)
    #   
    #   for (i in seq_along(variaveis)) {
    #     local({
    #       ii <- i
    #       output[[paste0("plot_", ii)]] <- renderPlot({
    #         plot(ph2[ii])
    #       })
    #     })
    #   }
    # })
  })
} 