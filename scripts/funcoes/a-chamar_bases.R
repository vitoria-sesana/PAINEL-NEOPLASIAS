# Função para chamar as bases necessárias pro painel ----------------------
 
# base <-
#   arrow::read_parquet(
#     "bases/base_app.parquet"
#   )

# base <-
#   arrow::read_parquet(
#     "bases/base_app_completo.parquet"
#   )

base <- 
  arrow::read_parquet(
    "bases/base_final.parquet"
  )

dicionario_classe <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "classe"
  ) %>% 
  rownames_to_column()

dicionario_dominio <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "dominio"
  ) %>% 
  rownames_to_column()

colunas_numericas <- 
  dicionario_classe %>% 
  filter(ponto_corte == 1) %>% 
  select(campo)  

covariaveis_numericas <- colunas_numericas$campo

# dicionario ------------

dicionario_nomes <- c(
  "Escolaridade" = "escolari",
  "Idade" = "idade",
  "Sexo" = "sexo",
  "UF de nascimento" = "ufnasc",
  "UF de residência" = "ufresid",
  # "Código IBGE" = "ibge",
  # "Cidade" = "cidade",
  "Categoria de atendimento" = "cateatend",
  # "Data da consulta" = "dtconsult",
  "Clínica" = "clinica",
  "Diagnóstico prévio" = "diagprev",
  # "Data do diagnóstico" = "dtdiag",
  "Base do diagnóstico" = "basediag",
  "Topografia" = "topo",
  "Grupo topográfico" = "topogrup",
  "Descrição da topografia" = "desctopo",
  "Morfologia" = "morfo",
  "Descrição da morfologia" = "descmorfo",
  "Estadiamento clínico (EC)" = "ec",
  "Grupo EC" = "ecgrup",
  "T" = "t",
  "N" = "n",
  "M" = "m",
  "pT" = "pt",
  "pN" = "pn",
  "pM" = "pm",
  "Estágio S" = "s",
  "Grau (G)" = "g",
  "Local TNM" = "localtnm",
  "Índice mitótico" = "idmitotic",
  "PSA" = "psa",
  "Gleason" = "gleason",
  "Outra classificação" = "outracla",
  "Metástase 01" = "meta01",
  "Metástase 02" = "meta02",
  "Metástase 03" = "meta03",
  "Metástase 04" = "meta04",
  # "Data do tratamento" = "dttrat",
  "Não tratado" = "naotrat",
  "Tratamento" = "tratamento",
  "Tratamento hospitalar" = "trathosp",
  "Tratamento antes do diagnóstico" = "tratfantes",
  "Tratamento após o diagnóstico" = "tratfapos",
  "Nenhum tratamento" = "nenhum",
  "Cirurgia" = "cirurgia",
  "Radioterapia" = "radio",
  "Quimioterapia" = "quimio",
  "Hormonioterapia" = "hormonio",
  "Transplante de medula óssea (TMO)" = "tmo",
  "Imunoterapia" = "imuno",
  "Outros tratamentos" = "outros",
  "Nenhum tratamento anterior" = "nenhumant",
  "Cirurgia anterior" = "cirurant",
  "Radioterapia anterior" = "radioant",
  "Quimioterapia anterior" = "quimioant",
  "Hormonioterapia anterior" = "hormoant",
  "TMO anterior" = "tmoant",
  "Imunoterapia anterior" = "imunoant",
  "Outro tratamento anterior" = "outroant",
  "Nenhum tratamento após" = "nenhumapos",
  "Cirurgia após" = "cirurapos",
  "Radioterapia após" = "radioapos",
  "Quimioterapia após" = "quimioapos",
  "Hormonioterapia após" = "hormoapos",
  "TMO após" = "tmoapos",
  "Imunoterapia após" = "imunoapos",
  "Outro tratamento após" = "outroapos",
  # "Data da última informação" = "dtultinfo",
  # "Última informação" = "ultinfo",
  "Consulta antes do diagnóstico" = "consdiag",
  "Tratamento antes da consulta" = "tratcons",
  "Diagnóstico antes do tratamento" = "diagtrat",
  "Ano do diagnóstico" = "anodiag",
  "CICI" = "cici",
  "Grupo CICI" = "cicigrup",
  "Subgrupo CICI" = "cicisubgru",
  "Faixa etária" = "faixaetar",
  "Lateralidade" = "laterali",
  "Instituição de origem" = "instorig",
  "DRS" = "drs",
  "RRAS" = "rras",
  "Perda de seguimento" = "perdaseg",
  "Erro de preenchimento" = "erro",
  # "Data da recidiva" = "dtrecidiva",
  "Sem recidiva" = "recnenhum",
  "Recidiva local" = "reclocal",
  "Recidiva regional" = "recregio",
  "Recidiva à distância" = "recdist",
  "Recidiva 01" = "rec01",
  "Recidiva 02" = "rec02",
  "Recidiva 03" = "rec03",
  "Recidiva 04" = "rec04",
  "IBGE de atendimento" = "ibgeaten",
  "CID-O" = "cido",
  "Descrição CID-O" = "dsccido",
  "Habilitação" = "habilit",
  "Habilitação 1" = "habilit1",
  "Habilitação 2" = "habilit2"
  # "Indicadora" = "indicadora",
  # "Tempo (dias)" = "tempo_dias",
  # "Tempo (semanas)" = "tempo_semanas",
  # "Tempo (meses)" = "tempo_meses",
  # "Tempo (anos)" = "tempo_anos"
)



