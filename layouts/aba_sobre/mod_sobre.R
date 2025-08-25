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
    shinyBS::bsCollapse(
      id = ns("metodologia2"),
      shinyBS::bsCollapsePanel("Estimação não paramétrica da função de risco",
                               "Além da função de sobrevivência, foi estimada de forma não paramétrica a função de risco, que representa a taxa instantânea de morte em cada momento do tempo, dado que o paciente ainda não faleceu até aquele ponto. Essa função é útil para identificar períodos críticos em que o risco de morte se eleva. A estimativa foi realizada por meio do estimador de Nelson-Aalen e, quando necessário, aplicou-se suavização para melhor visualização do padrão de risco ao longo do tempo.",
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Ponto de corte",
                               "Variáveis contínuas presentes na base de dados serão categorizadas ao serem selecionadas por meio da definição de pontos de corte (cut-offs). Esses pontos foram determinados com o método de maximização do log-rank (MaxStat), com o objetivo de identificar o valor que melhor separa os grupos com maior e menor probabilidade de sobrevivência. Essa categorização permitiu a construção de comparações estatísticas robustas entre grupos distintos.",
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Teste de log-rank",
                               "Ele compara as funções de sobrevivência entre dois ou mais grupos (por exemplo, pacientes que receberam tratamentos diferentes), testando a hipótese nula de que não há diferença nas taxas de risco entre os grupos ao longo do tempo.",
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Modelo de cox",
                               "O modelo de Cox é um modelo semiparamétrico que descreve a razão de risco (hazard ratio) para indivíduos com diferentes características, sem precisar assumir uma forma específica para a função de risco base (baseline hazard function).",
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Análise de suposição de risco",
                               "O principal pressuposto do modelo de Cox é o da proporcionalidade dos riscos que é a razão de risco entre dois indivíduos com diferentes covariáveis é constante ao longo do tempo. Ou seja, se o indivíduo A tem o dobro do risco de B no início do estudo, ele continuará tendo o dobro ao longo de todo o tempo. A verificação dessa suposição é crucial, porque violações podem tornar as estimativas do modelo inválidas. Por conta disso, utilizamos testes estatísticos de Schoenfeld. Um p-valor pequeno (< 0.05) indica violação da suposição de proporcionalidade para alguma variável.",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Pré processamento de dados",
                               tags$h3("Passos do Processo de Pré-processamento:"),
                               tags$ol(
                                 tags$li(tags$b("Construção do Dicionário de Dados:"), 
                                         " Criação de um dicionário com duas abas: uma com a relação de variáveis e suas classes, e outra com a descrição dos elementos associados."),
                                 tags$li(tags$b("Leitura e Tratamento Inicial dos Dados:"), 
                                         " Leitura da base com 1.149.055 registros, aplicação de encoding nas variáveis character/factor e filtro para a variável 'topogrup' no intervalo de 37 a 49."),
                                 tags$li(tags$b("Verificação das Classes das Colunas:"), 
                                         " Classificação das colunas como integer, character ou date, com identificação das colunas de data a partir de um PDF."),
                                 tags$li(tags$b("Armazenamento e Eficiência:"), 
                                         " Salvamento da base de dados em formato parquet para garantir eficiência e compatibilidade."),
                                 tags$li(tags$b("Validação da Estrutura da Base:"), 
                                         " Identificação de 4 colunas inconsistentes no dicionário, mas confirmação de 98 colunas corretas na base."),
                                 tags$li(tags$b("Criação da Variável Temporal:"), 
                                         " Cálculo da variável 'tempo até o óbito' com a diferença entre 'ultinfo' e 'dtdiag'."),
                                 tags$li(tags$b("Tratamento de Dados Categóricos:"), 
                                         " Conversão de valores numéricos das variáveis conforme as descrições do dicionário para garantir correta interpretação."),
                                 tags$li(tags$b("Amostragem para Shiny:"), 
                                         " Limitação de 1.000 registros para reduzir a complexidade computacional e facilitar a análise e desenvolvimento da interface.")
                               ),
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Membros",
                               "Letícia Emile, Mário Prado e Vitória Sesana",
                               style = "info"
      )
    ),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Código",
                               "Códigos do painel disponíveis no github:",
                               tags$a(href = "https://github.com/vitoria-sesana/PAINEL-NEOPLASIAS", "link github."), 
                               style = "info"
      )
    ),
  )
}

server_sobre <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })
}