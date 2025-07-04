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
source("scripts/layouts/c-filtro.R", encoding = "UTF-8")
source("scripts/layouts/d-kaplan_meier.R", encoding = "UTF-8")
source("scripts/layouts/e-funcao_risco.R", encoding = "UTF-8")
source("scripts/layouts/f-log_rank.R", encoding = "UTF-8")
source("scripts/layouts/g-sobre.R", encoding = "UTF-8")

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
        ui_selecao("b-selecao"),
        ui_filtro("c-filtro")
        ),
      mainPanel(
        navset_tab( 
          nav_panel(
            "Kaplan Meier",
            ui_kaplan_meier("d-kaplan_meier")
            ), 
          nav_panel(
            "Taxa de Risco", 
            ui_risco("e-funcao_risco")
            ), 
          nav_panel(
            "Teste de Log-Rank", 
            ui_log_rank("f-log_rank")
            )
          )
        )
      )
  ),
  
  ## informações ---------------------------------------------------------
  navbarMenu(
    "Sobre",
    tabPanel("Metodologia", ui_sobre("g_sobre")),
    tabPanel("Membros", "Leticía, Mario e Vitória")
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  base_inicial <- server_selecao("b-selecao")
  base_selecionada <- server_filtro("c-filtro", base_inicial)
  server_kaplan_meier("d-kaplan_meier", base_selecionada, base_inicial)
  server_risco("e-funcao_risco", base_selecionada)
  server_log_rank("f-log_rank", base_selecionada)
  server_sobre("g-sobre")
}

shinyApp(ui, server)