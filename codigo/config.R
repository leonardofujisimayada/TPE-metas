#=======================
#= Importa bibliotecas =
#=======================

library(data.table)    # importa e trata arquivos .csv
library(here)          # facilita inclusão de caminho
library(dplyr)         # trata dataframes/datatables
library(writexl)       # exporta arquivo para Excel
library(haven)         # importa dados .dta

#=========================
#= Importa dados do Saeb = 
#=========================

# Caminho para os microdados ANTIGOS do 9º ano do Ensino Fundamental (2019)
caminho_saeb_19_dta <- here("dados", "saeb_2019.dta")

# Caminho para os microdados do 9º ano do Ensino Fundamental (2019)
caminho_saeb_9_ef_19 <- here("dados", "SAEB_2019", "TS_ALUNO_9EF.csv")
# Caminho para os microdados do 9º ano do Ensino Fundamental (2021)
caminho_saeb_9_ef_21 <- here("dados", "SAEB_2021", "TS_ALUNO_9EF.csv")
# Caminho para os microdados do 9º ano do Ensino Fundamental (2023)
caminho_saeb_9_ef_23 <- here("dados", "SAEB_2023", "TS_ALUNO_9EF.csv")

# Importação dos microdados ANTIGOS do 9º ano do Ensino Fundamental (2019)
saeb_19_dta <- haven::read_dta(caminho_saeb_19_dta)
saeb_19_dta <- setDT(saeb_19_dta)

# Importação dos dados utilizando data.table 
saeb_9_ef_19 <- data.table::fread(caminho_saeb_9_ef_19, encoding="Latin-1")
# Importação dos dados utilizando data.table 
saeb_9_ef_21 <- data.table::fread(caminho_saeb_9_ef_21, encoding="Latin-1")
# Importação dos dados utilizando data.table 
saeb_9_ef_23 <- data.table::fread(caminho_saeb_9_ef_23, encoding="Latin-1")


