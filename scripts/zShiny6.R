library(shiny)

ui <- fluidPage(
  titlePanel("Todos os Tipos de Filtros (Inputs) no Shiny"),
  
  fluidRow(
    column(4,
           h4("Entrada de Texto"),
           textInput("text", "Texto:"),
           passwordInput("password", "Senha:"),
           numericInput("numeric", "Número:", value = 10, min = 0, max = 100)
    ),
    
    column(4,
           h4("Seleções"),
           selectInput("select", "Seleção Única:", choices = c("Opção 1", "Opção 2", "Opção 3")),
           checkboxInput("checkbox", "Checkbox (Sim/Não)", value = TRUE),
           checkboxGroupInput("checkboxGroup", "Grupo de Checkboxes:",
                              choices = c("Item A", "Item B", "Item C")),
           radioButtons("radio", "Botões de Rádio:",
                        choices = c("Opção X", "Opção Y", "Opção Z"))
    ),
    
    column(4,
           h4("Outros Inputs"),
           sliderInput("slider", "Controle Deslizante:",
                       min = 0, max = 100, value = 50),
           dateInput("date", "Data única:", value = Sys.Date()),
           dateRangeInput("dateRange", "Intervalo de Datas:",
                          start = Sys.Date() - 7, end = Sys.Date()),
           fileInput("file", "Upload de Arquivo:"),
           actionButton("botao", "Botão de Ação"),
           actionLink("link", "Link de Ação")
    )
  ),
  
  hr(),
  h4("Valores dos Inputs:"),
  verbatimTextOutput("valores")
)

server <- function(input, output) {
  output$valores <- renderPrint({
    list(
      Texto = input$text,
      Senha = input$password,
      Número = input$numeric,
      Selecao = input$select,
      Checkbox = input$checkbox,
      GrupoCheckbox = input$checkboxGroup,
      Radio = input$radio,
      Slider = input$slider,
      Data = input$date,
      IntervaloData = input$dateRange,
      Arquivo = input$file$name,
      BotaoClique = input$botao,
      LinkClique = input$link
    )
  })
}

shinyApp(ui, server)
