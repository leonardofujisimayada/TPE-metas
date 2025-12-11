#=======================
#= Importa bibliotecas =
#=======================

library(data.table)    # importa e trata arquivos .csv
library(here)          # facilita inclusão de caminho
library(dplyr)         # trata dataframes/datatables

#==============================
#= Importa dados do Saeb 2023 = 
#==============================

# Caminho para os microdados do 9º ano do Ensino Fundamental (2023)
caminho_saeb_9_ef_23 <- here("dados", "SAEB_2023", "TS_ALUNO_9EF.csv")
# Importação dos dados utilizando data.table 
saeb_9_ef_23 <- data.table::fread(caminho_saeb_9_ef_23, encoding="Latin-1")


