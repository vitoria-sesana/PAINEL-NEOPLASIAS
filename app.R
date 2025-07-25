library(shiny)
library(tidyverse)
require(survival)
require(survminer)
library(rlang)
library(muhaz)
library(bshazard)
library(shinyjs)
library(shinydashboard)
library(bslib)
library(plotly)

# módulos -----------------------------------------------------------------
source("layouts/mod_selecao.R")
source("layouts/mod_selecao_avancada.R")
source("layouts/mod_kaplan_meier.R")
source("layouts/mod_risco.R")
source("layouts/mod_log_rank.R")
source("layouts/mod_sobre.R")
source("funcoes_auxiliares/funcao_log_rank.R")
source("funcoes_auxiliares/funcao_chamar_bases.R")
source("funcoes_auxiliares/funcao_ponto_de_corte.R")

ui <- ui <- navbarPage(
  "Painel Neoplasias",
  tabPanel(
    title = "Análise Não Paramétrica",
    sidebarLayout(
      sidebarPanel(
        helpText("Escolha uma variável numérica da base para visualizar o histograma."),
        mod_selecao_ui("selecao"),
        mod_selecao_avancada_ui("selecao_avancada")
      ),
      mainPanel(
        navset_tab( 
          nav_panel(
            "Kaplan Meier",
            mod_kaplan_meier_ui("kaplan_meier")
          ), 
          nav_panel(
            "Taxa de Risco", 
            mod_risco_ui("risco")
          ), 
          nav_panel(
            "Teste de Log-Rank", 
            mod_log_rank_ui("log_rank")
            )
        ),
        br(),
      )
    )
  ),
  
  ## informações ---------------------------------------------------------
  navbarMenu(
    "Sobre",
    tabPanel("Metodologia", ui_sobre("sobre"))
    # tabPanel("Membros", "Leticía, Mario e Vitória")
  )
)

server <- function(input, output, session) {
  # Base de dados reativa
  dados <- reactiveFileReader(
    intervalMillis = 5000,  # atualiza a cada 5 segundos
    session = session,
    filePath = "bases/base_pequena.parquet",
    readFunc = arrow::read_parquet
  )
  
  # layouts --------------------------------------------------------------------
  saida_selecao <- 
    mod_selecao_server("selecao", dados)
  saida_selecao_avancada <- 
    mod_selecao_avancada_server("selecao_avancada",
                                saida_selecao = saida_selecao)
  
  mod_kaplan_meier_server("kaplan_meier", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  mod_risco_server("risco", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  mod_log_rank_server("log_rank",
                      saida_selecao_avancada = saida_selecao_avancada)
  
}

shinyApp(ui, server)