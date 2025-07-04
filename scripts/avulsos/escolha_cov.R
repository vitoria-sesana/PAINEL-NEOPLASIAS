base <-
  arrow::read_parquet(
    "bases/base_app_completo.parquet"
  )

# tratamento, consulta e diagnostico



# social ------------------------------------------------------------------

base$escolari %>% table
base$idade %>% table # nao aparece no filtro 
base$sexo %>% table
base$faixaetar %>% table
base$ufnasc %>% table
base$ufresid %>% table
base$ibge %>% table # não usar
base$cidade %>% table # usar


# diagnostico -------------------------------------------------------------

base$cateatend %>% table
base$dtconsult %>% table
base$clinica %>% table
base$diagprev %>% table
base$dtdiag %>% table
base$basediag %>% table
base$anodiag %>% table 

# localização do tumor/topografia -----------------------------------------

base$topo %>% unique() %>% length()
base$topogrup %>% unique() %>% length()
base$desctopo %>% unique() %>% length() # tratar e utilizar no painel

## morfologia ---
base$morfo %>% table # morfologia
base$cido %>% table
base$dsccido %>% table

## estagio da doença -----

base$ec %>% table
base$ecgrup %>% table


## categoria: TNM ----------------------------------------------------------

base$t %>% table
base$n %>% table
base$m %>% table
base$pt %>% table
base$pn %>% table
base$pm %>% table
base$s %>% table # remover
base$g %>% table
base$localtnm %>% table  # remover
base$idmitotic %>% table
base$psa %>% table # remover
base$gleason %>% table # remover

base$outracla %>% table ######## character ## remover?


## metástase ------------

base$meta01 %>% table
base$meta02 %>% table
base$meta03 %>% table
base$meta04 %>% table


# tratamento --------------------------------------------------------------

base$dttrat # data
base$naotrat %>% table # recusa de tratamento !!!
base$tratamento %>% table
base$tratamento %>% table
base$tratamento %>% table
base$tratamento %>% table
base$tratamento %>% table
base$tratamento %>% table



# data da ultima infor ----------------------------------------------------

base$dtultinfo %>% table
base$ultinfo %>% table

# dias entre consulta, diagnosto e tratamento -----------------------------

base$consdiag %>% table
base$tratcons %>% table
base$anodiag %>% table

# Tumor infantil ----------------------------------------------------------
base$cici %>% table
base$cicigrup %>% table
base$cicisubgru %>% table

# não faço ideia ----------------------------------------------------------

base$laterali %>% table
base$instorig %>% table
base$drs %>% table
base$rras %>% table
base$perdaseg %>% table


# admissão com erro -------------------------------------------------------

base$erro %>% table
#base$perdaseg %>% table # cade dtpreench


# recidiva ----------------------------------------------------------------

base$dtrecidiva %>% table # não colocar
base$recnenhum %>% table
base$reclocal %>% table
base$recregio %>% table
base$recdist %>% table
base$rec01 %>% table
base$rec02 %>% table
base$rec03 %>% table
base$rec04 %>% table




# habilitação oncologico ---------------------------------------------------

## instituição ---------

# base$dscinst %>% unique() # cade a base DSCINST 
base$ibgeaten %>% unique()

base$habilit %>% table
base$habilit1 %>% table
base$habilit2 %>% table
base$habit11 %>% table ### 

