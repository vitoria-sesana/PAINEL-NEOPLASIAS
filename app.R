# bibliotecas -------------------------------------------------------------
library(shiny)
library(tidyverse)
require(survival)
require(survminer)
library(rlang)
library(muhaz)
library(bshazard)
library(shinyjs)
library(shinydashboard)
library(bslib)
library(plotly)
library(reactable)

# módulos -----------------------------------------------------------------

# aba não paramétrica
source("layouts/aba_nao_parametrica/mod_selecao.R")
source("layouts/aba_nao_parametrica/mod_selecao_avancada.R")
source("layouts/aba_nao_parametrica/mod_kaplan_meier.R")
source("layouts/aba_nao_parametrica/mod_risco.R")
source("layouts/aba_nao_parametrica/mod_log_rank.R")

# aba análise semiparamétrica
source("layouts/aba_semiparametrica/SP1_selecao.R")
source("layouts/aba_semiparametrica/SP2_estimativas.R")
source("layouts/aba_semiparametrica/SP3_riscos_proporcionais.R")

# aba informações gerais
source("layouts/aba_sobre/mod_sobre.R")

# aba funções auxiliares
source("funcoes_auxiliares/funcao_log_rank.R")
source("funcoes_auxiliares/funcao_chamar_bases.R")
source("funcoes_auxiliares/funcao_ponto_de_corte.R")


ui <- navbarPage(
  "Painel Neoplasias",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    /* Barra de navegação fixa no topo */
    .navbar {
      background-color: #2c3e50;
      position: fixed;
      width: 100%;
      top: 0;
      z-index: 1000;
    }
    
    /* Espaço para o conteúdo não ser coberto pela navbar */
    body {
      padding-top: 70px;
    }
    
    /* Cor do texto da navbar */
    .navbar .navbar-nav > li > a {
      color: #ecf0f1 !important;
      font-size: 18px;
    }
    
    /* Cor do texto ao passar o mouse sobre os itens da navbar */
    .navbar .navbar-nav > li > a:hover {
      color: #c4d1d4 !important;
    }
    
    /* Cor da navbar quando ela está fixa */
    .navbar.fixed-top {
      background-color: #ecf0f1 !important;
    }
    
    /* Cor do título da navbar */
    .navbar .navbar-brand {
      color: #ecf0f1 !important;
    }
    
    /* Cor do título da navbar ao passar o mouse */
    .navbar .navbar-brand:hover {
      color: #ecf0f1 !important;
    }
    
    /* Modificando o estilo do botão de fixar a navbar */
    .navbar-toggle {
      border-color: #3498db;
    }
    
    .navbar-toggle:hover, .navbar-toggle:focus {
      background-color: #3498db;
      border-color: #2980b9;
    }
    
    /* Mudando o fundo ao fixar a navbar (quando rolar a página para baixo) */
    .navbar-scrolled {
      background-color: #2980b9 !important;
    }
    
    /* Cor de fundo da aba ativa (selecionada) */
    .navbar-nav > .active > a {
      background-color: #486684 !important;  /* Cor roxa para a aba ativa */
    }
  "))
  ),
  
# análise não paramétrica -------------------------------------------------
  tabPanel(
    title = "Análise Não Paramétrica",
    sidebarLayout(
      sidebarPanel(
        helpText("Escolha e filtre as opções abaixo para visualizar os resultados."),
        mod_selecao_ui("selecao"),
        mod_selecao_avancada_ui("selecao_avancada")
      ),
      mainPanel(
        tags$style(HTML("
      .container-fluid {
        margin-left: 15px;
        margin-right: 15px;
      }
    ")),
        navset_tab( 
          nav_panel(
            "Kaplan Meier",
            mod_kaplan_meier_ui("kaplan_meier")
          ), 
          nav_panel(
            "Taxa de Risco", 
            mod_risco_ui("risco")
          ), 
          nav_panel(
            "Teste de Log-Rank", 
            mod_log_rank_ui("log_rank")
            )
        ),
        br(),
      )
    )
  ),
  
  ## análise paramétrica  -------------------------------------------------
  tabPanel(
    title = "Análise Semiparamétrica",
    sidebarLayout(
      sidebarPanel(
        helpText("Escolha e filtre as opções abaixo para visualizar os resultados."),
        SP1_selecao_ui("SP1_selecao")
        # mod_selecao_avancada_ui("selecao_avancada")
      ),
      mainPanel(
        tags$style(HTML("
      .container-fluid {
        margin-left: 15px;
        margin-right: 15px;
      }
    ")),
        navset_tab( 
          nav_panel(
            "Estimativas & Interpretações",
            SP2_cox_estimativas_ui("SP2_estimativas")
          ),
          nav_panel(
            "Riscos Proporcionais",
            SP3_riscos_proporcionais_ui("SP3_riscos_proporcionais")
          )
        )
      )
    )
  ),
  ## informações ---------------------------------------------------------
    tabPanel(
    "Sobre",
    ui_sobre("sobre")
  )
)

server <- function(input, output, session) {
  
  
  observe({
    showModal(
      modalDialog(
        title = "Bem-vindo ao Painel Neoplasias!",
        "Este é um painel cujo objetivo é analisar, por meio de técnicas estatísticas, o tempo até o óbito de paciêncentes diganósticados com os câncers de CID 37 a 49.",
        "Aqui você verá técnicas de ajustes da função de sobrevivência com ajustes paramétricos e não paramétricos, como o estimador de Kaplan-Meier e o modelo de regressão de Cox.",
        tags$h3("Lista de CIDs com suas descrições:"),
        tags$ul(
          tags$li(tags$b("C37 - Timo:"), " Refere-se ao câncer que afeta o timo, uma glândula localizada no mediastino."),
          tags$li(tags$b("C38 - Coração, mediastino e pleura:"), " Indica cânceres que afetam o coração, o mediastino (área entre os pulmões) e a pleura."),
          tags$li(tags$b("C40 - Ossos e cartilagens dos membros:"), " Refere-se aos cânceres malignos que afetam os ossos e cartilagens dos braços e pernas."),
          tags$li(tags$b("C41 - Ossos e cartilagens de outras localizações:"), " Indica cânceres que afetam os ossos e cartilagens de outras áreas do corpo além dos membros."),
          tags$li(tags$b("C42 - Órgãos hematopoéticos e linfáticos:"), " Envolve cânceres que afetam órgãos responsáveis pela produção de sangue, como medula óssea, e os linfáticos, como os linfonodos."),
          tags$li(tags$b("C44 - Nervos periféricos:"), " Refere-se a tumores malignos que afetam os nervos periféricos."),
          tags$li(tags$b("C47 - Pele:"), " Refere-se aos tumores malignos que afetam a pele."),
          tags$li(tags$b("C48 - Retroperitônio e peritônio:"), " Refere-se a tumores malignos localizados no retroperitônio e no peritônio."),
          tags$li(tags$b("C49 - Outros tecidos conjuntivos e moles:"), " Refere-se a tumores malignos que afetam outros tecidos conjuntivos e moles, como músculos e tendões.")
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Fechar")
        )
      )
    )
  })
  
  
  ## chamando base de dados reativa -------
  dados <- reactiveFileReader(
    intervalMillis = 5000,  # atualiza a cada 5 segundos
    session = session,
    filePath = "bases/base_pequena.parquet",
    readFunc = arrow::read_parquet
  )
  
  ## server inputs nao parametrico -------
  
  saida_selecao <- 
    mod_selecao_server("selecao", dados)
  
  saida_selecao_avancada <- 
    mod_selecao_avancada_server("selecao_avancada",
                                saida_selecao = saida_selecao)
  
  ## server output nao parametrico -------
  mod_kaplan_meier_server("kaplan_meier", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  
  mod_risco_server("risco", 
                        saida_selecao = saida_selecao,
                        saida_selecao_avancada = saida_selecao_avancada)
  
  mod_log_rank_server("log_rank",
                      saida_selecao_avancada = saida_selecao_avancada)
  
  ## server input semiparametrico -------
  saida_cox <- 
    SP1_selecao_server("SP1_selecao", data = dados)
  
  ## server output semiparametrico -------
  SP2_cox_estimativas_server("SP2_estimativas", saida_cox = saida_cox)
  
  SP3_riscos_proporcionais_server("SP3_riscos_proporcionais", saida_cox = saida_cox)
}

shinyApp(ui, server)