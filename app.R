rm(list=ls())
# Bibliotecas -------------------------------------------------------------
library(shiny)
library(data.table)
library(tidyverse)
library(data.table)
library(shinyjs)
library(shinydashboard)
library(bslib)
# Módulos -----------------------------------------------------------------
#source("", encoding = "UTF-8")

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "Painel Neoplasias",
  tabPanel(
    title = "Home", 
    "Neoplasias Torácicas Não Respiratórias, Osso e Pele e Tecidos Moles"
    ),
  tabPanel("Análises Exploratória", "two"),
  tabPanel("Análises Não Paramétricas", "three"),
  tabPanel("Análises Paramétricas e Semiparamétricas", "three"),
  navbarMenu("Sobre",
             tabPanel("Metodologia", "Análise de Sobrevivência"),
             tabPanel("Membros", "Leticía, Mario e Vitória")
  )
)



# server ------------------------------------------------------------------
server <- function(input, output, session) {

}

shinyApp(ui, server)