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
    # Input: botão filtro avançado ------------------
    actionButton(ns("botao_filtro"),"Filtro Avançado"),
    br(),
    hr()
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
      cicisubgru = NULL,
      
      # tratamento
      naotrat = NULL,
      tratamento = NULL,
      trathosp = NULL,
      
      # diagnostico
      clinica = NULL,
      diagprev = NULL,
      basediag = NULL,
      anodiag = NULL,  
      cateatend = NULL,
      
      # habilitação
      habilit = NULL,
      habilit1 = NULL,
      habilit2= NULL,
      
      # recidiva
      recnenhum = NULL,
      reclocal = NULL,
      recregio = NULL,
      recdist = NULL
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
                   h4("Tumores infantis"),
                   
                   #### infantil -------
                   selectInput(
                     ns("filtro_cici"),
                     'Tumor infantil:',
                     sort(unique(df$cici)),
                     multiple=TRUE, 
                     selectize=FALSE),
                   
                   #### grupo infantil -------
                   selectInput(
                     ns("filtro_cicigrup"),
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
          br(),
          hr(),
          
          
          fluidRow(
            
            ## Diagnostico -------------------
            column(
              3,
              h4("Diagnóstico"),
              
              ####  Clinica -------
              selectInput(
                ns("filtro_clinica"),
                'Tipo de clínica :',
                sort(unique(df$clinica)),
                multiple=TRUE, 
                selectize=FALSE),
              
              #### Atendimento -------
              selectInput(
                ns("filtro_cateatend"),
                'Tipo de atendimento:',
                sort(unique(df$cateatend)),
                multiple=TRUE, 
                selectize=FALSE),
              
              ####  Diag e trat anterior-------
              selectInput(
                ns("filtro_diagprev"),
                'Diagnóstico e tratamento anterior:',
                sort(unique(df$diagprev)),
                multiple=TRUE, 
                selectize=FALSE),
              
              ####  Ano diag-------
              selectInput(
                ns("filtro_anodiag"),
                'Ano do diagnóstico:',
                sort(unique(df$anodiag)),
                multiple=TRUE, 
                selectize=FALSE),
              
              ####  Tipo diag-------
              selectInput(
                ns("filtro_basediag"),
                'Tipo de diagnóstico:',
                sort(unique(df$basediag)),
                multiple=TRUE, 
                selectize=FALSE),
              
            ),
            
            ## Tratamento -------------------
            
            column(
              3,
              h4("Tratamento"),
              
              #### tratamento realizados -------
              selectInput(
                ns("filtro_tratamento"),
                'Tratamentos realizados:',
                sort(unique(df$tratamento)),
                multiple=TRUE, 
                selectize=FALSE),
              
              #### tratamento hospital -------
              selectInput(
                ns("filtro_trathosp"),
                'Tratamentos realizados no hospital:',
                sort(unique(df$trathosp)),
                multiple=TRUE, 
                selectize=FALSE),
              
              #### Razão de não trat -------
              selectInput(
                ns("filtro_naotrat"),
                'Razão de não tratamento:',
                sort(unique(df$naotrat)),
                multiple=TRUE, 
                selectize=FALSE),
            ),
            
            
            ## Reciditiva -------------------
            column(
              3,
              h4("Reciditva"),
              
              checkboxGroupInput(
                ns("filtro_recnenhum"), 
                'Sem recidiva:',
                choices = unique(df$recnenhum),
                selected = unique(df$recnenhum)
              ),
              
              checkboxGroupInput(
                ns("filtro_reclocal"), 
                'Recidiva local:',
                choices = unique(df$reclocal),
                selected = unique(df$reclocal)
              ),
              
              
              checkboxGroupInput(
                ns("filtro_recregio"), 
                'Recidiva regional:',
                choices = unique(df$recregio),
                selected = unique(df$recregio)
              ),
              
              checkboxGroupInput(
                ns("filtro_recdist"), 
                'Recidiva a distância/metástase:',
                choices = unique(df$recdist),
                selected = unique(df$recdist)
                ),
            ),
            ## Habilitação -------------------
            column(
              3,
              h4("Habilitação"),
              
              #### Habilitações -------
              selectInput(
                ns("filtro_habilit"),
                'Habilitações:',
                sort(unique(df$habilit)),
                multiple=TRUE, 
                selectize=FALSE),
              
              #### Habilitações1 -------
              selectInput(
                ns("filtro_habilit1"),
                'Categorias de habilitações:',
                sort(unique(df$habilit1)),
                multiple=TRUE, 
                selectize=FALSE),
            ),
            
          
          ),

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
      
      ## Trtatamento ----------------
      filtros$tratamento <- input$filtro_tratamento
      filtros$trathosp  <- input$filtro_trathosp
      filtros$naotrat <- input$filtro_naotrat
      
      
      ## Clínica -------------------
      filtros$clinica <- input$filtro_clinica
      filtros$diagprev  <- input$filtro_diagprev
      filtros$basediag <- input$filtro_basediag
      filtros$anodiag  <- input$filtro_anodiag
      filtros$cateatend <- input$filtro_cateatend
      
      ## Habilitação ---------------
      filtros$habilit <- input$filtro_habilit
      filtros$habilit1 <- input$filtro_habilit1
      
      ## Recidiva  ---------------
      filtros$recnenhum <- input$filtro_recnenhum
      filtros$reclocal<- input$filtro_reclocal
      filtros$recregio <- input$filtro_recregio
      filtros$recdist <- input$filtro_recdist
      
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
      if (!is.null(filtros$tratamento ) && length(filtros$tratamento ) > 0) {
        df <- df[df$tratamento  %in% filtros$tratamento , ]
      }
      if (!is.null(filtros$trathosp) && length(filtros$trathosp) > 0) {
        df <- df[df$trathosp %in% filtros$trathosp, ]
      }
      if (!is.null(filtros$naotrat) && length(filtros$naotrat) > 0) {
        df <- df[df$naotrat %in% filtros$naotrat, ]
      } #tratamento
      if (!is.null(filtros$clinica) && length(filtros$clinica) > 0) {
        df <- df[df$clinica %in% filtros$clinica, ]
      }
      if (!is.null(filtros$diagprev) && length(filtros$diagprev) > 0) {
        df <- df[df$diagprev %in% filtros$diagprev, ]
      }
      if (!is.null(filtros$basediag ) && length(filtros$basediag ) > 0) {
        df <- df[df$basediag  %in% filtros$basediag , ]
      }
      if (!is.null(filtros$anodiag) && length(filtros$anodiag) > 0) {
        df <- df[df$anodiag %in% filtros$anodiag, ]
      }
      if (!is.null(filtros$cateatend) && length(filtros$cateatend) > 0) {
        df <- df[df$cateatend %in% filtros$cateatend, ]
      } # habilitação
      if (!is.null(filtros$habilit) && length(filtros$habilit) > 0) {
        df <- df[df$habilit %in% filtros$habilit, ]
      }
      if (!is.null(filtros$habilit1) && length(filtros$habilit1) > 0) {
        df <- df[df$habilit1 %in% filtros$habilit1, ]
      } # recidiva
      if (!is.null(filtros$recnenhum) && length(filtros$recnenhum) > 0) {
        df <- df[df$recnenhum %in% filtros$recnenhum, ]
      }
      if (!is.null(filtros$reclocal) && length(filtros$reclocal) > 0) {
        df <- df[df$reclocal %in% filtros$reclocal, ]
      } 
      if (!is.null(filtros$recregio) && length(filtros$recregio) > 0) {
        df <- df[df$recregio %in% filtros$recregio, ]
      }
      if (!is.null(filtros$recdist) && length(filtros$recdist) > 0) {
        df <- df[df$recdist %in% filtros$recdist, ]
      } 
      
      df
    })

    # Saída: -------------------------------------------------------------------
    
    return(
      list(
        data = react_base_filtrada
      )
    )
  })
}