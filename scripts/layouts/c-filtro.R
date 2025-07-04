# Layout: filtragem avançada -----------------------------------------------
# recebe a base de dados com todas as colunas
# base de dados completa
# ponto de corte
# covariavel
# variavel tempo

# pegar a base, filtrar ela e enviar ela após o botão
# resetar o filtro novamente (pegar só a base enviada)

ui_filtro <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    br(),
    # Input: botão filtro avançado
    actionButton(ns("botao_filtro"),"Filtro Avançado"),
    br(),
    hr(),
    verbatimTextOutput(ns("texto_filtro"))
  )
}

server_filtro <- function(id, base_inicial) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive: valores filtros -----------
    
    filtros <- reactiveValues(
      # filtros sociais
      
      faixaetar = NULL,
      sexo = NULL,
      escolari = NULL,
      ufnasc = NULL,
      ufresid = NULL,
      
      # filtros tumores
      filtro_subtopo = NULL,
      ec = NULL,
      ecgrup = NULL,
      dsccido = NULL,
      cici = NULL,
      cicigrup = NULL,
      cicisubgru = NULL
      
      )

    # ObserveEvent: botão filtro  --------------------
    observeEvent(input$botao_filtro,{
      
      df <- base_inicial$data()
      
      showModal(
        modalDialog(
          title = "Filtro avançado",
          
          fluidRow(
            
            ## Social --------------
            column(3,
                   h4("Características Sociais"),
                   
                   
                   #### sexo ---------------------
                   checkboxGroupInput(
                     ns("filtro_sexo"), 
                     "Gênero:",
                     choices = unique(df$sexo),
                     selected = unique(df$sexo)
                   ),
                   
                   #### escolaridade ------------
                   selectInput(
                     ns("filtro_escolari"),
                     'Escolaridade:',
                     sort(unique(df$escolari)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   
                   
                   #### faixa etária ------------
                   selectizeInput(
                     ns("filtro_faixa_etaria"),
                     "Faixa etária:",
                     choices = sort(unique(df$faixaetar)),
                     multiple = TRUE,
                     selected = unique(df$faixaetar),
                   ),
                  
                   
                   #### ufnasc -----------------
                   selectInput(
                     ns("filtro_ufnasc"),
                     'UF de nascimento:',
                     sort(unique(df$ufnasc)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### ufresid -----------------
                   selectInput(
                     ns("filtro_ufresid"),
                     'UF de residência:',
                     sort(unique(df$ufresid)),
                     multiple=TRUE, 
                     selectize=FALSE),

            ),
            
            ## Tumores -------------------
            column(4,
                   h4("Características do tumor"),
                   
                   #### subcategoria cids -------
                   selectInput(
                     ns("filtro_subtopo"),
                     'Subcategoria CIDs:',
                     sort(unique(df$filtro_subtopo)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### estagio clínico -------
                   selectInput(
                     ns("filtro_ec"),
                     'Estadio Clínico:',
                     sort(unique(df$ec)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### subcategoria estagio clínico -------
                   selectInput(
                     ns("filtro_ecgrup"),
                     'Subcategoria Estadio Clínico:',
                     sort(unique(df$ecgrup)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### descrição da morfologia -------
                   selectInput(
                     ns("filtro_dsccido"),
                     'Descrição da Morofologia:',
                     sort(unique(df$dsccido)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
            ),
            
            ## Tumor infantil-------------------
            column(4,
                   h4("Características de tumores infantis"),
                   
                   #### infantil -------
                   selectInput(
                     ns("filtro_cici"),
                     'Tumor infantil:',
                     sort(unique(df$cici)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### grupo infantil -------
                   selectInput(
                     ns("filtro_dcicigrup"),
                     'Grupo:',
                     sort(unique(df$cicigrup)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### subgrupo infantil -------
                   selectInput(
                     ns("filtro_cicisubgru"),
                     'Subgrupo:',
                     sort(unique(df$cicisubgru)),
                     multiple=TRUE, 
                     selectize=FALSE),
            )
            
          ),

          selectizeInput(
            'e2', '2. Multi-select', choices = colnames(mtcars), multiple = TRUE
          ),
          selectInput(
            ns("in3"),
            'Options',
            colnames(mtcars),
            multiple=TRUE, 
            selectize=FALSE),

          footer = tagList(
            modalButton("Cancelar"),
            # actionButton(ns("resetar_filtros"), "Resetar Filtros"),
            actionButton(ns("aplicar_filtro"), "Aplicar filtro")
          ),
          size = "l"
        )
      )
      
    })
    
    # ObserveEvent: filtros ---------------------------------------------------
    observeEvent(input$aplicar_filtro, {
      
      ## Social ---------
      filtros$faixaetar <- input$filtro_faixa_etaria
      filtros$sexo <- input$filtro_sexo
      filtros$escolari <- input$filtro_escolari
      filtros$ufnasc <- input$filtro_ufnasc
      filtros$ufresid <- input$filtro_ufresid
      
      ## Tumor ---------- 
      filtros$filtro_subtopo <- input$filtro_subtopo
      filtros$ec  <- input$filtro_ec 
      filtros$ecgrup <- input$filtro_ecgrup 
      filtros$dsccido <- input$filtro_dsccido
      
      ## Tumor infantil -------------
      filtros$cici <- input$filtro_cici
      filtros$cicigrup  <- input$filtro_cicigrup
      filtros$cicisubgru <- input$filtro_cicisubgru
      removeModal()
    })
    
    # Reactive: filtros --------------------------------------------------------
    react_base_filtrada <- reactive({
      df <- base_inicial$data()
      if (!is.null(filtros$faixaetar) && length(filtros$faixaetar) > 0) {
        df <- df[df$faixaetar %in% filtros$faixaetar, ]
      }
      if (!is.null(filtros$sexo) && length(filtros$sexo) > 0) {
        df <- df[df$sexo %in% filtros$sexo, ]
      }
      if (!is.null(filtros$escolari) && length(filtros$escolari) > 0) {
        df <- df[df$escolari %in% filtros$escolari, ]
      }
      if (!is.null(filtros$ufnasc) && length(filtros$ufnasc) > 0) {
        df <- df[df$ufnasc %in% filtros$ufnasc, ]
      }
      if (!is.null(filtros$ufresid) && length(filtros$ufresid) > 0) {
        df <- df[df$ufresid %in% filtros$ufresid, ]
      }
      if (!is.null(filtros$filtro_subtopo) && length(filtros$filtro_subtopo) > 0) {
        df <- df[df$filtro_subtopo %in% filtros$filtro_subtopo, ]
      }
      if (!is.null(filtros$ec) && length(filtros$ec) > 0) {
        df <- df[df$ec %in% filtros$ec, ]
      }
      if (!is.null(filtros$ecgrup) && length(filtros$ecgrup) > 0) {
        df <- df[df$ecgrup %in% filtros$ecgrup, ]
      }
      if (!is.null(filtros$dsccido) && length(filtros$dsccido) > 0) {
        df <- df[df$dsccido %in% filtros$dsccido, ]
      }
      if (!is.null(filtros$cici ) && length(filtros$cici ) > 0) {
        df <- df[df$cici  %in% filtros$cici , ]
      }
      if (!is.null(filtros$cicigrup) && length(filtros$cicigrup) > 0) {
        df <- df[df$cicigrup %in% filtros$cicigrup, ]
      }
      if (!is.null(filtros$cicisubgru) && length(filtros$cicisubgru) > 0) {
        df <- df[df$cicisubgru %in% filtros$cicisubgru, ]
      }
      
      df
    })
    
    output$texto_filtro <- renderText({
      react_base_filtrada() %>% 
        as.data.frame() %>% 
        select(cicigrup) %>% 
        unique() %>% 
        as.character()
    })
     

    # Saída: -------------------------------------------------------------------
    
    return(
      list(
        data = react_base_filtrada
      )
    )
  })
}