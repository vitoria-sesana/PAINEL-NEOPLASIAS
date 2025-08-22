# mod_histograma.R

# UI do módulo
mod_kaplan_meier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # textOutput(ns("aaaa")),
    # tableOutput(ns("TABELA")),
    plotOutput(ns("GRAFICO_KAPLAN_MEIER")),
    reactableOutput(ns("TABELA_KAPLAN_MEIER"))
  )
}

# Server do módulo

mod_kaplan_meier_server <- function(id, saida_selecao, saida_selecao_avancada) {
  moduleServer(id, function(input, output, session) {
    
    ## teste ################
    output$aaaa <- renderText(
      as.character(saida_selecao$ic_selecionado() == "TRUE")
      # class(saida_kaplan_meier()$tabela_kaplan_meier)
    )
    
    ## teste ##############33
    output$TABELA <-
      renderTable(
        head(select(saida_selecao_avancada$data_selecionada_avancada(),indicadora , tempo, covariavel))
      )
    
    ## CALCULO: Kaplan-Meier -------------------------------------
    saida_kaplan_meier <- reactive({
      base <- saida_selecao_avancada$data_selecionada_avancada()
      req(base)
      
      ajuste_kaplan_meier <-
        survfit(
          Surv(
            time = tempo,
            event = indicadora) ~
            covariavel,
          data = base
        )

      tabela_kaplan_meier <-
        surv_summary(
          ajuste_kaplan_meier,
          data = base) %>%
        mutate(
          surv = formatC(surv, digits = 2),
          std.err = formatC(std.err, digits = 2),
          upper = formatC(upper, digits = 2),
          lower = formatC(lower, digits = 2)
        )
      
      if ("strata" %in% names(tabela_kaplan_meier)) {
        tabela_kaplan_meier <- tabela_kaplan_meier[, !(names(tabela_kaplan_meier) %in% "strata")]
        
        tabela_kaplan_meier <- tabela_kaplan_meier %>% 
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
      } else {
        tabela_kaplan_meier <- tabela_kaplan_meier %>% 
          dplyr::rename(
            Tempo = time,
            Sobreviventes = n.risk,
            Eventos = n.event,
            Censuras = n.censor,
            `Sobrevivência` = surv,
            `Erro Padrão` = std.err,
            `Limite Superior` = upper,
            `Limite Inferior` = lower
          )
      }
      
      list(
        ajuste_kaplan_meier = ajuste_kaplan_meier,
        tabela_kaplan_meier = tabela_kaplan_meier
      )
    })
    

    # GRÁFICO: Kaplan-Meier -------------------------------------------------
    
    output$GRAFICO_KAPLAN_MEIER <- renderPlot({

      req(saida_selecao_avancada$data_selecionada_avancada())
      req(saida_selecao$cid_selecionado())
      req(saida_selecao$tempo_selecionado())
      req(saida_selecao$nome_tempo())
      req(saida_selecao$covariavel_selecionada())
      req(saida_selecao$ic_selecionado())

      req(saida_kaplan_meier()$ajuste_kaplan_meier)

      if (saida_selecao$ic_selecionado()) {
        gg_kp <-
          ggsurvplot(
            saida_kaplan_meier()$ajuste_kaplan_meier,
            data = saida_selecao_avancada$data_selecionada_avancada(),
            conf.int = TRUE,
            conf.int.style = "ribbon",
            ylab = "S(t) estimada",
            xlab = saida_selecao$nome_tempo(),
            legend.title = "",
            ggtheme = theme_bw()
          )

        gg_kp$plot <-
          gg_kp$plot +
          scale_y_continuous(
            limits = c(0, 1),
            breaks =
              seq(
                from = min(saida_selecao_avancada$data_selecionada_avancada()$tempo, na.rm = TRUE),
                to = max(saida_selecao_avancada$data_selecionada_avancada()$tempo, na.rm = TRUE),
                length.out = 7)
          ) +
          scale_x_continuous(
            limits = c(0, max(saida_selecao_avancada$data_selecionada_avancada()$tempo))
          ) +
          theme(legend.position = "bottom")


        saida_gg_kp <- gg_kp$plot

        saida_gg_kp

      } else {
        gg_kp <-
          ggsurvplot(
            saida_kaplan_meier()$ajuste_kaplan_meier,
            data = saida_selecao_avancada$data_selecionada_avancada(),
            conf.int = FALSE,
            conf.int.style = "ribbon",
            ylab = "S(t) estimada",
            xlab = saida_selecao$nome_tempo(),
            legend.title = "",
            ggtheme = theme_bw()
          )

        gg_kp$plot <-
          gg_kp$plot +
          scale_y_continuous(
            limits = c(0, 1),
            breaks =
              seq(
                from = min(saida_selecao_avancada$data_selecionada_avancada()$tempo, na.rm = TRUE),
                to = max(saida_selecao_avancada$data_selecionada_avancada()$tempo, na.rm = TRUE),
                length.out = 7)
          ) +
          scale_x_continuous(
            limits = c(0, max(saida_selecao_avancada$data_selecionada_avancada()$tempo))
          ) +
          theme(legend.position = "bottom")


        saida_gg_kp <- gg_kp$plot

        saida_gg_kp

      }

    })
    
    # TABELA: Kaplan-Meier ----------------------------------------------------

    TABELA_KAPLAN_MEIER <- reactive({
       x <- saida_kaplan_meier()$tabela_kaplan_meier
       return(x)
    })

    output$TABELA_KAPLAN_MEIER <- renderReactable({
      req(TABELA_KAPLAN_MEIER())
      reactable(
        TABELA_KAPLAN_MEIER(),
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
