ui_sobre <- function(id) {
  ns <- NS(id)
  tagList(
    "Neoplasias Torácicas Não Respiratórias, Osso e Pele e Tecidos Moles",
    br(),
    hr(),
    "VITOIA",
    hr(),
    verbatimTextOutput(ns("textovv"))
  )
}

server_sobre <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # textOutput$textovv <- 
    #     as.character(DADO)

  })
}