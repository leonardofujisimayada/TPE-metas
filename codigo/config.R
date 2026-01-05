#=======================
#= Importa bibliotecas =
#=======================

library(data.table)    # importa e trata arquivos .csv
library(here)          # facilita inclusão de caminho
library(dplyr)         # trata dataframes/datatables
library(writexl)       # exporta arquivo para Excel
library(haven)         # importa dados .dta
library(readxl)        # importa arquivo Excel

#=====================================================
#= Importa dados dos subgrpos (tamanho da população) = 
#=====================================================

# Dados dos subgrupos do TPE 
tb_subgrupos <- readxl::read_xlsx(path = here("dados", "TB_SUBGRUPOS.xlsx"))

#=========================
#= Importa dados do Saeb = 
#=========================

# Caminho para os microdados ANTIGOS do 9º ano do Ensino Fundamental (2013)
caminho_saeb_13_dta <- here("dados", "saeb_2013.dta")
# Caminho para os microdados ANTIGOS do 9º ano do Ensino Fundamental (2015)
caminho_saeb_15_dta <- here("dados", "saeb_2015.dta")
# Caminho para os microdados ANTIGOS do 9º ano do Ensino Fundamental (2017)
caminho_saeb_17_dta <- here("dados", "saeb_2017.dta")
# Caminho para os microdados ANTIGOS do 9º ano do Ensino Fundamental (2019)
caminho_saeb_19_dta <- here("dados", "saeb_2019.dta")

# Caminho para os microdados do 9º ano do Ensino Fundamental (2019)
caminho_saeb_9_ef_19 <- here("dados", "SAEB_2019", "TS_ALUNO_9EF.csv")
# Caminho para os microdados do 9º ano do Ensino Fundamental (2021)
caminho_saeb_9_ef_21 <- here("dados", "SAEB_2021", "TS_ALUNO_9EF.csv")
# Caminho para os microdados do 9º ano do Ensino Fundamental (2023)
caminho_saeb_9_ef_23 <- here("dados", "SAEB_2023", "TS_ALUNO_9EF.csv")

# Importação dos microdados ANTIGOS do 9º ano do Ensino Fundamental (2013)
saeb_13_dta <- haven::read_dta(caminho_saeb_13_dta)
saeb_13_dta <- setDT(saeb_13_dta)
# Importação dos microdados ANTIGOS do 9º ano do Ensino Fundamental (2015)
saeb_15_dta <- haven::read_dta(caminho_saeb_15_dta)
saeb_15_dta <- setDT(saeb_15_dta)
# Importação dos microdados ANTIGOS do 9º ano do Ensino Fundamental (2017)
saeb_17_dta <- haven::read_dta(caminho_saeb_17_dta)
saeb_17_dta <- setDT(saeb_17_dta)
# Importação dos microdados ANTIGOS do 9º ano do Ensino Fundamental (2019)
saeb_19_dta <- haven::read_dta(caminho_saeb_19_dta)
saeb_19_dta <- setDT(saeb_19_dta)

# Importação dos dados utilizando data.table 
saeb_9_ef_19 <- data.table::fread(caminho_saeb_9_ef_19, encoding="Latin-1")
# Importação dos dados utilizando data.table 
saeb_9_ef_21 <- data.table::fread(caminho_saeb_9_ef_21, encoding="Latin-1")
# Importação dos dados utilizando data.table 
saeb_9_ef_23 <- data.table::fread(caminho_saeb_9_ef_23, encoding="Latin-1")


saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2013 com o Censo da 
  # Educação Básica 2013 finalizado
  in_situacao_censo == 1 &
    # Escola pública
    in_publica == 1 &
    # 9º ano do Ensino Fundamental
    id_serie == 9,
  .(
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(proficiencia_mt_saeb, na.rm=T)/sum(peso_aluno_mt, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(proficiencia_lp_saeb, na.rm=T)/sum(peso_aluno_lp, na.rm=T)
  )
]