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

# Módulos -----------------------------------------------------------------
source("scripts/panel_home.R", encoding = "UTF-8")
source("scripts/panel_kp.R", encoding = "UTF-8")
source("scripts/panel_grafico_kp.R", encoding = "UTF-8")


# base --------------------------------------------------------------------

base <-
  arrow::read_parquet(
    "bases/base_app.parquet"
  )

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "Painel Neoplasias",

  # análise de kaplan meier -------------------------------------------------

  tabPanel(   title = "Análise Não Paramétrica",
           sidebarLayout(
             sidebarPanel(
               ui_panel_kp("panel_kp")
             ),
             mainPanel(
               ui_grafico_kp("panel_grafico_kp")
             )
           )
  ),
  
  ## informações ---------------------------------------------------------
  navbarMenu("Sobre",
             tabPanel("Metodologia", "Análise de Sobrevivência"),
             tabPanel("Membros", "Leticía, Mario e Vitória")
  )
)



# server ------------------------------------------------------------------
server <- function(input, output, session) {
  base_selecionada <- server_panel_kp("panel_kp")
  server_grafico_kp("panel_grafico_kp", base_selecionada)
}

shinyApp(ui, server)