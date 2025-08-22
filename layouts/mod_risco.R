# mod_histograma.R

# UI do módulo
mod_risco_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("GRAFICO_RISCO")),
    reactableOutput(ns("TABELA_RISCO"))
  )
}

# Server do módulo

mod_risco_server <- function(id, saida_selecao, saida_selecao_avancada) {
  moduleServer(id, function(input, output, session) {
    
    # CALCULO: risco --------------------------------------------------------

    saida_risco <- reactive({
      base <- saida_selecao_avancada$data_selecionada_avancada()
      req(base)
  
      obter_risco <- function(df) {
        grupo_nome <- unique(df$covariavel)
        tempo <- df$tempo
        status <- df$indicadora
        
        resultado <- tryCatch({
          muhaz(times = tempo, delta = status)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(resultado) || length(resultado$haz.est) == 0) {
          return(NULL)
        }
        
        df_final <- data.frame(
          tempo = resultado$est.grid,
          risco = resultado$haz.est,
          grupo = grupo_nome
        )
        
        return(df_final)
      }
      
      # Aplica por grupo com segurança
      tabela_risco <- base %>%
        group_by(covariavel) %>%
        group_split() %>%
        purrr::map_df(obter_risco)
      
      list(
        tabela_risco = tabela_risco
      )
    })
    
    
    # GRÁFICO: risco -------------------------------------------------
    
    output$GRAFICO_RISCO <- renderPlot({
      
      tabela_risco <- saida_risco()$tabela_risco
      
      req(tabela_risco)
      req(saida_selecao$tempo_selecionado())
      req(saida_selecao$nome_tempo())
      req(saida_selecao$covariavel_selecionada())
      
      ggplot(tabela_risco, aes(x = tempo, y = risco, color = grupo)) +
        geom_line(size = 1) +
        labs(
          title = paste("Função de Risco Estimada por",saida_selecao$covariavel_selecionada() ),
          x = saida_selecao$nome_tempo(),
          y = "Função de Risco"
        ) +
        theme_minimal() +
        theme(legend.title = element_blank())

    })
    
    # TABELA: Risco ----------------------------------------------------
    
    TABELA_RISCO <- reactive({
      req(saida_risco()$tabela_risco)
      tab <- saida_risco()$tabela_risco
      return(tab)
    })
    
    output$TABELA_RISCO <- renderReactable({
      req(saida_risco()$tabela_risco)
      reactable(
        saida_risco()$tabela_risco,
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
      
  })
}
