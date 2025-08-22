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
                               "O principal pressuposto do modelo de Cox é o da proporcionalidade dos riscos que é a razão de risco entre dois indivíduos com diferentes covariáveis é constante ao longo do tempo. Ou seja, se o indivíduo A tem o dobro do risco de B no início do estudo, ele continuará tendo o dobro ao longo de todo o tempo. A verificação dessa suposição é crucial, porque violações podem tornar as estimativas do modelo inválidas. Por conta disso, utilizamos testes estatísticos de Schoenfeld para testar a independêcia entre os resíduos e o tempo. Um p-valor pequeno (< 0.05) indica violação da suposição de proporcionalidade para alguma variável.",
                               style = "info"
      )
    ),
    hr(),
    shinyBS::bsCollapse(
      id = ns("metodologia3"),
      shinyBS::bsCollapsePanel("Pré processamento de dados",
                               "O processo de processamento e pré-processamento dos dados seguiu uma sequência estruturada de etapas, com o objetivo de garantir a organização, consistência e integridade das informações antes de sua utilização em aplicações como o Shiny. A base de dados principal continha 1.149.055 registros, e o trabalho teve início com a construção de uma base dicionário, composta por duas abas distintas: uma relacionando cada variável à sua respectiva classe, e outra descrevendo os elementos associados a essas variáveis. Esse dicionário foi fundamental para orientar o tratamento posterior das variáveis e valores.

A primeira etapa do pré-processamento envolveu a leitura da base de dados original. Durante essa leitura, foi aplicado o tratamento de encoding para todas as variáveis classificadas como do tipo character ou factor, com o objetivo de evitar problemas relacionados à codificação de caracteres. Em seguida, foi feito um filtro para manter apenas os registros cujo valor da variável topogrup pertencesse ao intervalo de 37 a 49, reduzindo o escopo da análise para o subconjunto de interesse.

Após o filtro inicial, foi realizada uma verificação detalhada das classes de todas as colunas da base, classificando-as entre integer, character e date. A identificação correta das colunas do tipo data foi baseada nas informações contidas em um PDF com a descrição das variáveis — esse documento foi utilizado para definir com mais precisão quais colunas deveriam ser tratadas como datas. Com essa estrutura de classes estabelecida, os dados foram então salvos em formato parquet, visando eficiência e compatibilidade com ferramentas modernas de análise.

Durante o processo de validação da estrutura da base em relação ao dicionário, identificou-se uma pequena discrepância: havia quatro colunas inconsistentes — três que constavam no PDF, mas não estavam presentes na base, e uma que existia na base, mas não aparecia no dicionário. Ainda assim, a base contava com 98 colunas ao todo, o que demonstra uma consistência razoável com a documentação.

Uma variável temporal importante, denominada 'tempo até o óbito', foi criada a partir das colunas de data identificadas. Especificamente, essa variável foi calculada como a diferença entre a data de última informação (ultinfo) e a data de diagnóstico (dtdiag). O resultado foi mais uma vez salvo em formato parquet, garantindo persistência e organização do dado processado.

No passo seguinte, foi realizado o tratamento dos elementos das variáveis com base na classificação contida na base dicionário. Isso significava que, para cada valor numérico encontrado na base, buscava-se sua correspondência na aba do dicionário que continha a descrição dos elementos. Essa etapa permitiu a conversão e a interpretação correta dos dados categóricos codificados numericamente, garantindo que fossem compreendidos de forma legível e analítica.

Por fim, devido às limitações de desempenho da aplicação Shiny, apenas os 1.000 primeiros registros da base foram utilizados para fins de visualização ou teste. Essa amostragem reduziu a complexidade computacional e permitiu uma análise mais rápida durante o desenvolvimento da interface ou ferramenta interativa.",
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
  )
}

server_sobre <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })
}