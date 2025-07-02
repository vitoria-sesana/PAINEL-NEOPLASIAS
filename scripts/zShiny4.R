library(shiny)
library(bslib)

ui <- page_fluid(
  accordion(  
      accordion_panel( 
        title = "Section A", 
        icon = bsicons::bs_icon("menu-app"),
        "Section A content"
      ),  
      accordion_panel(
        title = "Section B",
        icon = bsicons::bs_icon("sliders"),
        "Section B content"
      ),  
      accordion_panel(
        title = "Section C",
        icon = bsicons::bs_icon("bar-chart"),
        "Section C content"
      ),  
      accordion_panel(
        title = "Section D",
        icon = bsicons::bs_icon("calendar-date"), 
        "Section D content" 
      ),  
      id = "acc",  
      open = "Section A"  
  )  
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)