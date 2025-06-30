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

## home -------------
source("scripts/layouts/panel_home.R", encoding = "UTF-8")

## seleção -----------
source("scripts/layouts/panel_kp.R", encoding = "UTF-8")

## gráficos ----------
source("scripts/layouts/panel_grafico_kp.R", encoding = "UTF-8")
source("scripts/layouts/panel_grafico_risco.R", encoding = "UTF-8")


## tabelas ------- 
source("scripts/layouts/secao_log_rank.R", encoding = "UTF-8")



## informações -------
source("scripts/layouts/funcionalidade_sobre.R", encoding = "UTF-8")

# Funções auxiliares ------------------------------------------------------

## gráficos -----
source("scripts/funcoes/d-funcao-risco.R", encoding = "UTF-8")
source("scripts/funcoes/funcao_log_rank_pares.R", encoding = "UTF-8")

# base --------------------------------------------------------------------

base <-
  arrow::read_parquet(
    "bases/base_app.parquet"
  )



# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "Painel Neoplasias",

  # análise de kaplan meier -------------------------------------------------

  tabPanel(
    title = "Análise Não Paramétrica",
    sidebarLayout(
      sidebarPanel(
        ui_panel_kp("panel_kp")
        ),
      mainPanel(
        ui_grafico_kp("panel_grafico_kp"),
        ui_grafico_risco("panel_grafico_risco"),
        ui_log_rank("secao_log_rank")
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
  base_selecionada <- server_panel_kp("panel_kp")
  server_grafico_kp("panel_grafico_kp", base_selecionada)
  server_grafico_risco("panel_grafico_risco", base_selecionada)
  server_log_rank("secao_log_rank", base_selecionada)
  server_sobre("funcionalidade_sobre")
}

shinyApp(ui, server)