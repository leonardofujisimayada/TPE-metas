#=======================
#= Importa bibliotecas =
#=======================

library(here)          # facilita inclusão de caminho
library(writexl)       # exporta arquivo para Excel
library(readxl)        # importa arquivo Excel
library(censobr)       # importa dados do Censo Demográfico
library(arrow)         # trabalha com dados parquet
library(dplyr)         # permite manipulação de dados
library(data.table)    # permite importação de dados volumosos

#=====================================================
#= Importa dados dos subgrpos (tamanho da população) = 
#=====================================================

# Dados dos subgrupos do TPE 
tb_subgrupos <- readxl::read_xlsx(path = here("dados", "TB_SUBGRUPOS.xlsx"))

#==========================
#= Importa dados do Censo = 
#==========================

# 2000 

# População de 2000
# censobr::data_dictionary(year = 2000, dataset = 'population', showProgress = TRUE)

# UF: V0102
# Código do município: V0103
# Peso amostral: P001
# Idade em anos: V4752
# Cor ou raça: V0408

pop_2000 <- read_population(
  year = 2000,
  columns = c('V0103', 'P001', 'V4752', 'V0408'),
  showProgress = FALSE
)

# 2010 

# População de 2010
#censobr::data_dictionary(year = 2010, dataset = 'population', showProgress = TRUE)

# UF: V0001
# Código do município: V0002
# Peso amostral: V0010
# Idade em anos: V6036
# Cor ou raça: V0606

pop_2010 <- read_population(
  year = 2010,
  columns = c('V0010', 'V0001', 'V0002', 'V6036', 'V0606', 'V0601', 'V6036'),
  add_labels = 'pt',
  showProgress = FALSE
)

# 2022

# População de 2022
censobr::data_dictionary(year = 2022, dataset = 'tracts', showProgress = TRUE)

pop_2022_pessoas <- read_tracts(
  year = 2022,
  dataset = 'Pessoas', 
  showProgress = FALSE
)

pop_2022_basico <- read_tracts(
  year = 2022,
  dataset = 'Basico', 
  showProgress = FALSE
)

#==========================================
#= Importa dados do Censo Escolar de 2022 = 
#==========================================

# Microdados do Censo Escolar de 2022
dt_censo_2022 <- data.table::fread("C:/Users/leona/OneDrive/Desktop/integral/dados/Microdados do Censo Escolar da Educação Básica 2022/dados/microdados_ed_basica_2022.csv", encoding = "Latin-1")

dt_censo_2022 %>% 
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) %>% 
  summarise(
    MAT_03 = sum(QT_MAT_BAS_0_3, na.rm=T), 
    MAT_BAS_TOTAL = sum(QT_MAT_BAS, na.rm=T),
    MAT_BAS_BRANCA_AMARELA = sum(QT_MAT_BAS_BRANCA, na.rm=T) + sum(QT_MAT_BAS_AMARELA, na.rm=T),
    MAT_BAS_PPI = sum(QT_MAT_BAS_PRETA, na.rm=T) + sum(QT_MAT_BAS_PARDA, na.rm=T) + sum(QT_MAT_BAS_INDIGENA, na.rm=T),
    P_BRANCA_AMARELA = MAT_BAS_BRANCA_AMARELA/MAT_BAS_TOTAL,
    P_PPI = MAT_BAS_PPI/MAT_BAS_TOTAL
  ) %>% 
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, MAT_03, P_BRANCA_AMARELA, P_PPI
  )

#==================================
#= Cálculo agregado por município = 
#==================================

# 2000 

# Criação de código de município e de variável que agrega raça/cor
pop_2000 <- pop_2000 %>%
  rename(
    CO_MUNICIPIO = V0103
  ) %>% 
  filter(
    V4752 < 20
  ) %>% 
  mutate(
    RACA_COR = dplyr::case_when(
      V0408 %in% c(1, 3)     ~ "Branca/Amarela",
      V0408 %in% c(2, 4, 5)  ~ "PPI",
      V0408 == 9             ~ "Ignorado"
    ),
    FAIXA_ETARIA = dplyr::case_when(
      V4752 <= 03               ~ "00-03",
      V4752 >= 04 & V4752 < 06  ~ "04-05",
      V4752 >= 07 & V4752 < 15  ~ "06-14",
      V4752 >= 15 & V4752 < 20  ~ "15-19"
    )
  )

# Cálculo de população por raça/cor e município
pop_2000 <- pop_2000  %>% 
  group_by(CO_MUNICIPIO, RACA_COR, FAIXA_ETARIA) %>% 
  summarise(pop_count = sum(P001)) %>% 
  collect() %>% 
  filter(!(is.na(CO_MUNICIPIO)))

# 2010

# Criação de código de município e de variável que agrega raça/cor
pop_2010 <- pop_2010 %>% 
  filter(
    V6036 < 20
  ) %>% 
  mutate(
    CO_MUNICIPIO = paste0(V0001, V0002),
    RACA_COR = dplyr::case_when(
      V0606 %in% c('Branca', 'Amarela')           ~ "Branca/Amarela",
      V0606 %in% c('Preta', 'Parda', 'Indígena')  ~ "PPI",
      V0606 == 'Ignorado'                         ~ "Ignorado",
      .default = NA_character_
    ),
    FAIXA_ETARIA = dplyr::case_when(
      V6036 <= 03               ~ "00-03",
      V6036 >= 04 & V6036 < 06  ~ "04-05",
      V6036 >= 07 & V6036 < 15  ~ "06-14",
      V6036 >= 15 & V6036 < 20  ~ "15-19"
    )
  )

# Cálculo de população por raça/cor e município
pop_2010 <- pop_2010  %>% 
  group_by(CO_MUNICIPIO, RACA_COR, FAIXA_ETARIA) %>% 
  summarise(pop_count = sum(V0010)) %>% 
  collect()

# 2022
pop_2022_pessoas <- pop_2022_pessoas %>% 
  select(
    code_state      ,  # Código da Unidade da Federação
    code_muni       ,  # Código do Município
    code_tract      ,
    raca_V01317     ,
    raca_V01318     ,
    raca_V01319     ,
    raca_V01320     ,
    raca_V01321     
  ) %>% 
  group_by(code_state, code_muni, code_tract) %>% 
  summarise(
    pop_branca      =     sum(raca_V01317, na.rm=T), 
    pop_preta       =     sum(raca_V01318, na.rm=T),
    pop_amarela     =     sum(raca_V01319, na.rm=T),
    pop_parda       =     sum(raca_V01320, na.rm=T),
    pop_indigena    =     sum(raca_V01321, na.rm=T)
  ) %>% 
  collect()

pop_2022_basico <- pop_2022_basico %>% 
  select(
    code_state      ,  # Código da Unidade da Federação
    code_muni       ,  # Código do Município
    code_tract      ,
    V0001
  ) %>% 
  group_by(code_state, code_muni, code_tract) %>% 
  summarise(
    pop_total      =     sum(V0001, na.rm=T)
  ) %>% 
  collect()
  
# Merge de arquivos de 2022
pop_2022 <- merge(x=pop_2022_basico, y=pop_2022_pessoas, by = c("code_state", "code_muni", "code_tract"))


  