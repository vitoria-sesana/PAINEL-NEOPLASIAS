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
library(reactable)

# módulos -----------------------------------------------------------------
source("layouts/mod_selecao.R")
source("layouts/mod_selecao_avancada.R")
source("layouts/mod_kaplan_meier.R")
source("layouts/mod_risco.R")
source("layouts/mod_log_rank.R")
source("layouts/aba_semiparametrica/SP1_selecao.R")
source("layouts/aba_semiparametrica/SP2_estimativas.R")
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
        helpText("Escolha e filtre as opções abaixo para visualizar os resultados."),
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
  
  ## análise paramétrica  -------------------------------------------------
  tabPanel(
    title = "Análise Semiparamétrica",
    sidebarLayout(
      sidebarPanel(
        helpText("Escolha e filtre as opções abaixo para visualizar os resultados."),
        SP1_selecao_ui("SP1_selecao")
        # mod_selecao_avancada_ui("selecao_avancada")
      ),
      mainPanel(
        navset_tab( 
          nav_panel(
            "Estimativas & Interpretações",
            h1("Resultados do Modelo de Regressão Cox"),
            SP2_cox_estimativas_ui("SP2_estimativas")
          )
        )
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
  
  ## chamando base de dados reativa -------
  
  dados <- reactiveFileReader(
    intervalMillis = 5000,  # atualiza a cada 5 segundos
    session = session,
    filePath = "bases/base_pequena.parquet",
    readFunc = arrow::read_parquet
  )
  
  ## server inputs nao parametrico -------
  
  saida_selecao <- 
    mod_selecao_server("selecao", dados)
  
  saida_selecao_avancada <- 
    mod_selecao_avancada_server("selecao_avancada",
                                saida_selecao = saida_selecao)
  
  ## server output nao parametrico -------
  mod_kaplan_meier_server("kaplan_meier", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  
  mod_risco_server("risco", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  
  mod_log_rank_server("log_rank",
                      saida_selecao_avancada = saida_selecao_avancada)
  
  ## server modelo semiparametrico -------
  saida_cox <- 
    SP1_selecao_server("SP1_selecao", data = dados)
  
  SP2_cox_estimativas_server("SP2_estimativas", saida_cox = saida_cox)
}

shinyApp(ui, server)