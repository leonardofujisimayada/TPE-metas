#=================
#= Configurações = 
#=================

# Importação dos dados

# Dados de acesso de crianças de 0 a 3 anos 
df_acesso03 <- readxl::read_xlsx(
  path = here("dados", "20260120_acesso_conclusao.xlsx"), 
  sheet = "acesso_0_3"
)

# Dados de conclusão de jovens de 16 anos no Ensino Fundamental
df_conclusao16_PPI <- readxl::read_xlsx(
  path = here("dados", "20260120_acesso_conclusao.xlsx"), 
  sheet = "conclusao_16_PPI"
)
df_conclusao16_renda <- readxl::read_xlsx(
  path = here("dados", "20260120_acesso_conclusao.xlsx"), 
  sheet = "conclusao_16_renda"
)

# Dados de conclusão de jovens de 19 anos no Ensino Médio
df_conclusao19_PPI <- readxl::read_xlsx(
  path = here("dados", "20260120_acesso_conclusao.xlsx"), 
  sheet = "conclusao_19_PPI"
)
df_conclusao19_renda <- readxl::read_xlsx(
  path = here("dados", "20260120_acesso_conclusao.xlsx"), 
  sheet = "conclusao_19_renda"
)

#===========================================================
#= Preparação dos dados de diferença em pontos percentuais = 
#===========================================================

# Preparação dos dados de acesso 0-3 anos na Educação Infantil
df_acesso03_tratada <- reshape2::dcast(
  data = df_acesso03, formula = UF+Ano~quintil, value.var = "Frequenta") %>% 
  mutate(DIFF_QUINTIL = `5º`-`1º`) %>% 
  select(UF, Ano, DIFF_QUINTIL) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF_QUINTIL) %>% 
  mutate(
    indicador = "Diferença entre maior e menor quintil de renda na taxa de atendimento de crianças 0-3 anos na Educação Infantil"
  )
  
# Preparação dos dados de 16 PPI
df_conclusao16_PPI_tratada <- reshape2::dcast(
  data = df_conclusao16_PPI, formula = UF+Ano~grupo, value.var = "media") %>% 
  mutate(DIFF_RACACOR = Brancos-PPI) %>% 
  select(UF, Ano, DIFF_RACACOR) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF_RACACOR) %>% 
  mutate(
    indicador = "Diferença entre brancos/amarelos e pretos/pardos/indígenas no percentual da população de 16 anos com pelo menos o Ensino Fundamental concluído"
  )

# Preparação dos dados de 16 renda
df_conclusao16_renda_tratada <- reshape2::dcast(
  data = df_conclusao16_renda, formula = UF+Ano~quintil, value.var = "media") %>% 
  mutate(DIFF_RENDA = Q5-Q1) %>% 
  select(UF, Ano, DIFF_RENDA) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF_RENDA) %>% 
  mutate(
    indicador = "Diferença entre maior e menor quintil de renda no percentual da população de 16 anos com pelo menos o Ensino Fundamental concluído"
  )

# Junção de dados
df_conclusao16_tratada <- rbind(df_conclusao16_PPI_tratada, df_conclusao16_renda_tratada)

# Preparação dos dados de 19 PPI
df_conclusao19_PPI_tratada <- reshape2::dcast(
  data = df_conclusao19_PPI, formula = UF+Ano~grupo, value.var = "media") %>% 
  mutate(DIFF_RACACOR = Brancos-PPI) %>% 
  select(UF, Ano, DIFF_RACACOR) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF_RACACOR) %>% 
  mutate(
    indicador = "Diferença entre brancos/amarelos e pretos/pardos/indígenas no percentual da população de 19 anos com pelo menos o Ensino Médio concluído"
  )

# Preparação dos dados de 19 renda
df_conclusao19_renda_tratada <- reshape2::dcast(
  data = df_conclusao19_renda, formula = UF+Ano~quintil, value.var = "media") %>% 
  mutate(DIFF_RENDA = Q5-Q1) %>% 
  select(UF, Ano, DIFF_RENDA) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF_RENDA) %>% 
  mutate(
    indicador = "Diferença entre maior e menor quintil de renda no percentual da população de 19 anos com pelo menos o Ensino Médio concluído"
  )

# Junção de dados
df_conclusao19_tratada <- rbind(df_conclusao19_PPI_tratada, df_conclusao19_renda_tratada)

# Junção final de dados de conclusão
df_conclusao_tratada <- rbind(df_conclusao16_tratada, df_conclusao19_tratada)

# Remoção de arquivos não utilizados
rm(
  df_acesso03, df_conclusao16_PPI, df_conclusao16_PPI_tratada, 
  df_conclusao16_renda, df_conclusao16_renda_tratada, df_conclusao16_tratada,
  df_conclusao19_PPI, df_conclusao19_PPI_tratada, df_conclusao19_renda, 
  df_conclusao19_renda_tratada, df_conclusao19_tratada
)

#=============================================
#= Preparação dos dados em valores absolutos = 
#=============================================



