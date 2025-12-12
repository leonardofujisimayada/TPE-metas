#==================================================
#= 1. Análise de proficiência por raça/cor - 2023 =
#==================================================

saeb_9_ef_23_racacor <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é a sua cor ou 
    # raça?
    RACA_COR = case_when(
      TX_RESP_Q04 == "*" ~ "Nulo",
      TX_RESP_Q04 == "." ~ "Branco",
      TX_RESP_Q04 == "A" ~ "Branca.",
      TX_RESP_Q04 == "B" ~ "Preta.",
      TX_RESP_Q04 == "C" ~ "Parda.",
      TX_RESP_Q04 == "D" ~ "Amarela.",
      TX_RESP_Q04 == "E" ~ "Indígena.",
      TX_RESP_Q04 == "F" ~ "Não quero declarar.",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_MT, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_LP, na.rm=T)
  ),
  # Agrupamento por raça/cor
  by = "TX_RESP_Q04"
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, RACA_COR, MEDIA_MT, MEDIA_LP
  )
]

#================================================================
#= 1.1. Análise de proficiência por raça/cor e município - 2023 =
#================================================================

saeb_9_ef_23_racacor_municipio <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é a sua cor ou 
    # raça?
    RACA_COR = case_when(
      TX_RESP_Q04 == "*" ~ "Nulo",
      TX_RESP_Q04 == "." ~ "Branco",
      TX_RESP_Q04 == "A" ~ "Branca.",
      TX_RESP_Q04 == "B" ~ "Preta.",
      TX_RESP_Q04 == "C" ~ "Parda.",
      TX_RESP_Q04 == "D" ~ "Amarela.",
      TX_RESP_Q04 == "E" ~ "Indígena.",
      TX_RESP_Q04 == "F" ~ "Não quero declarar.",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_MT, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_LP, na.rm=T)
  ),
  # Agrupamento por raça/cor
  by = c("ID_MUNICIPIO", "TX_RESP_Q04")
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, ID_MUNICIPIO, RACA_COR, MEDIA_MT, MEDIA_LP
  )
]

#==============================================================
#= 2. Análise de proficiência por nível socioeconômico - 2023 =
#==============================================================

saeb_9_ef_23_nse <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1 &
    # Indicador para cálculo do INSE (São considerados válidos os estudantes 
    # que responderam pelo menos 8 itens, dentre os 17 utilizados para o 
    # cálculo do indicador)
    IN_INSE == 1,
  .(
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T)
  ),
  # Agrupamento por nível socioeconômico
  by = "NU_TIPO_NIVEL_INSE"
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, NU_TIPO_NIVEL_INSE, MEDIA_MT, MEDIA_LP
  )
]

#============================================================================
#= 2.2. Análise de proficiência por nível socioeconômico e município - 2023 =
#============================================================================

saeb_9_ef_23_nse_municipio <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1 &
    # Indicador para cálculo do INSE (São considerados válidos os estudantes 
    # que responderam pelo menos 8 itens, dentre os 17 utilizados para o 
    # cálculo do indicador)
    IN_INSE == 1,
  .(
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T)
  ),
  # Agrupamento por nível socioeconômico
  by = c("ID_MUNICIPIO", "NU_TIPO_NIVEL_INSE")
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, ID_MUNICIPIO, NU_TIPO_NIVEL_INSE, MEDIA_MT, MEDIA_LP
  )
]

#==============================================
#= 3. Análise de proficiência por sexo - 2023 =
#==============================================

saeb_9_ef_23_sexo <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é o seu sexo? 
    SEXO = case_when(
      TX_RESP_Q01 == "*" ~ "Nulo",
      TX_RESP_Q01 == "." ~ "Branco",
      TX_RESP_Q01 == "A" ~ "Masculino.",
      TX_RESP_Q01 == "B" ~ "Feminino.",
      TX_RESP_Q01 == "C" ~ "Não quero declarar.",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T)
  ),
  # Agrupamento por sexo
  by = "TX_RESP_Q01"
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, SEXO, MEDIA_MT, MEDIA_LP
  )
]

#============================================================
#= 3.1. Análise de proficiência por sexo e município - 2023 =
#============================================================

saeb_9_ef_23_sexo_municipio <- saeb_9_ef_23[
  # Consistente entre os dados da aplicação do Saeb 2023 com o Censo da 
  # Educação Básica 2023 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é o seu sexo? 
    SEXO = case_when(
      TX_RESP_Q01 == "*" ~ "Nulo",
      TX_RESP_Q01 == "." ~ "Branco",
      TX_RESP_Q01 == "A" ~ "Masculino.",
      TX_RESP_Q01 == "B" ~ "Feminino.",
      TX_RESP_Q01 == "C" ~ "Não quero declarar.",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_INSE, na.rm=T)
  ),
  # Agrupamento por sexo
  by = c("ID_MUNICIPIO", "TX_RESP_Q01")
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2023, ANO_SERIE = 9, ID_MUNICIPIO, SEXO, MEDIA_MT, MEDIA_LP
  )
]

#==========================
#= 4. Exportação de dados =
#==========================

# Criação de lista com dataframes
lista_dfs <- list(
  TB_NSE_BRASIL        = saeb_9_ef_23_nse,
  TB_NSE_MUNICIPIO     = saeb_9_ef_23_nse_municipio,
  TB_RACACOR_BRASIL    = saeb_9_ef_23_racacor,
  TB_RACACOR_MUNICIPIO = saeb_9_ef_23_racacor_municipio,
  TB_SEXO_BRASIL       = saeb_9_ef_23_sexo,
  TB_SEXO_MUNICIPIO    = saeb_9_ef_23_sexo_municipio
)

# Salva tudo em um único arquivo Excel
write_xlsx(lista_dfs, path = here("dados", "TB_DESIGUALDADE_2023.xlsx"))


