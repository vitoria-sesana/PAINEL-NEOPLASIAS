rm(list=ls())
# Bibliotecas -------------------------------------------------------------
library(shiny)
library(data.table)
library(tidyverse)
library(data.table)
library(shinyjs)
library(shinydashboard)
library(bslib)

require(survival)
require(survminer)
require(plotly)

# Layouts: ui e server ----------------------------------------------------

source("scripts/layouts/a-homepage.R", encoding = "UTF-8")
source("scripts/layouts/b-selecao.R", encoding = "UTF-8")
source("scripts/layouts/c-kaplan_meier.R", encoding = "UTF-8")
source("scripts/layouts/d-funcao_risco.R", encoding = "UTF-8")
source("scripts/layouts/e-log_rank.R", encoding = "UTF-8")
source("scripts/layouts/f-sobre.R", encoding = "UTF-8")

# Funções auxiliares ------------------------------------------------------

source("scripts/funcoes/a-chamar_bases.R", encoding = "UTF-8")
source("scripts/funcoes/b-ponto_de_corte.R", encoding = "UTF-8")
source("scripts/funcoes/d-funcao-risco.R", encoding = "UTF-8")
source("scripts/funcoes/e-log_rank.R", encoding = "UTF-8")

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "Painel Neoplasias",

  # análise de kaplan meier -------------------------------------------------
  # tabPanel(
  #   title = "Home",
  #   ui_homepage("a-homepage")
  # ),
  
  tabPanel(
    title = "Análise Não Paramétrica",
    sidebarLayout(
      sidebarPanel(
        ui_selecao("b-selecao")
        ),
      mainPanel(
        navset_tab( 
          nav_panel(
            "Kaplan Meier",
            ui_grafico_kp("c-kaplan_meier")
            ), 
          nav_panel(
            "Taxa de Risco", 
            "Page B content",
            ui_grafico_risco("d-funcao_risco")
            ), 
          nav_panel(
            "Teste de Log-Rank", 
            ui_log_rank("e-log_rank")
            )
          )
        )
      )
  ),
  
  ## informações ---------------------------------------------------------
  navbarMenu(
    "Sobre",
    tabPanel("Metodologia", ui_sobre("funcionalidade_sobre")),
    tabPanel("Membros", "Leticía, Mario e Vitória")
  )
)



# server ------------------------------------------------------------------
server <- function(input, output, session) {
  base_selecionada <- server_selecao("b-selecao")
  server_grafico_kp("c-kaplan_meier", base_selecionada)
  server_grafico_risco("d-funcao_risco", base_selecionada)
  server_log_rank("e-log_rank", base_selecionada)
  server_sobre("f-sobre")
}

shinyApp(ui, server)