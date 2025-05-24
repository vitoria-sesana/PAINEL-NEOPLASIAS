rm(list=ls())
# Bibliotecas -------------------------------------------------------------
library(shiny)
library(data.table)
library(tidyverse)
library(data.table)
library(shinyjs)
library(shinydashboard)

# Módulos -----------------------------------------------------------------
#source("", encoding = "UTF-8")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Neoplasias Torácicas Não Respiratórias"),
  h3("Lorem iptsum")
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
}

shinyApp(ui, server)