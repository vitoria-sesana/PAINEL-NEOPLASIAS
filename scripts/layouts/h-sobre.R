ui_sobre <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    shinyBS::bsCollapse(
      id = ns("sobre"),
      shinyBS::bsCollapsePanel("Sobre",
                      "As classificações utilizadas neste estudo foram baseadas nos códigos da Classificação Internacional de Doenças – 10ª Revisão (CID-10), com foco específico nas neoplasias malignas localizadas em estruturas torácicas não respiratórias, ossos, pele, tecidos moles, sistema linfático e órgãos hematopoéticos. Os códigos selecionados englobam tanto as categorias principais (topogrupo), como C37 a C49, quanto suas subdivisões específicas (topo), permitindo uma análise mais refinada da localização anatômica dos tumores.
Essa categorização é importante para compreender a distribuição topográfica das neoplasias e suas possíveis implicações clínicas e prognósticas, especialmente em análises de sobrevida ou risco de morte. Além disso, o uso de códigos padronizados garante a reprodutibilidade e a comparabilidade dos achados com outros estudos epidemiológicos baseados em dados do Sistema de Informações sobre Mortalidade ou registros hospitalares.",
                      style = "info"
                      )
      ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia4"),
      shinyBS::bsCollapsePanel("Delineamento do Estudo",
                               "Este estudo realizou uma análise de sobrevivência utilizando dados de pacientes diagnosticados com Neoplasias Torácicas Não Respiratórias, de Osso, Pele e Tecidos Moles, de acordo com a Classificação Internacional de Doenças – 10ª Revisão (CID-10), códigos C37 a C49. O evento de interesse foi o óbito do paciente, e os indivíduos foram acompanhados desde o momento do diagnóstico até a ocorrência do evento ou censura (isto é, perda de seguimento ou final do período de observação).",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia1"),
      shinyBS::bsCollapsePanel("Kaplan-Meier",
                               "Foi utilizada a técnica não paramétrica de Kaplan-Meier para estimar a função de sobrevivência dos pacientes ao longo do tempo. Essa abordagem permite calcular a probabilidade de um paciente sobreviver (isto é, permanecer vivo) após certo número de dias desde o diagnóstico. As curvas de Kaplan-Meier foram construídas para descrever a evolução da sobrevivência global e, quando aplicável, para comparar subgrupos com base em variáveis clínicas ou demográficas.",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia2"),
      shinyBS::bsCollapsePanel("Estimação não paramétrica da função de risco",
                               "Além da função de sobrevivência, foi estimada de forma não paramétrica a função de risco, que representa a taxa instantânea de morte em cada momento do tempo, dado que o paciente ainda não faleceu até aquele ponto. Essa função é útil para identificar períodos críticos em que o risco de morte se eleva. A estimativa foi realizada por meio do estimador de Nelson-Aalen e, quando necessário, aplicou-se suavização para melhor visualização do padrão de risco ao longo do tempo.",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Ponto de corte",
                               "Variáveis contínuas presentes na base de dados serão categorizadas ao serem selecionadas por meio da definição de pontos de corte (cut-offs). Esses pontos foram determinados com o método de maximização do log-rank (MaxStat), com o objetivo de identificar o valor que melhor separa os grupos com maior e menor probabilidade de sobrevivência. Essa categorização permitiu a construção de comparações estatísticas robustas entre grupos distintos.",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Membros",
                               "Letícia Emile, Mário Prado e Vitória Sesana",
                               style = "info"
      )
    ),
  )
}

server_sobre <- function(id) {
  moduleServer(id, function(input, output, session) {
    

  })
}