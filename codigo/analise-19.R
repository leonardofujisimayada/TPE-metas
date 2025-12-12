#==================================================
#= 1. Análise de proficiência por raça/cor - 2019 =
#==================================================

saeb_9_ef_19_racacor <- saeb_9_ef_19[
  # Consistente entre os dados da aplicação do Saeb 2019 com o Censo da 
  # Educação Básica 2019 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é a sua cor ou 
    # raça?
    RACA_COR = case_when(
      TX_RESP_Q002 == "A" ~ "Branca.",
      TX_RESP_Q002 == "B" ~ "Preta.",
      TX_RESP_Q002 == "C" ~ "Parda.",
      TX_RESP_Q002 == "D" ~ "Amarela.",
      TX_RESP_Q002 == "E" ~ "Indígena.",
      TX_RESP_Q002 == "F" ~ "Não quero declarar.",
      TX_RESP_Q002 == "*" ~ "Nulo",
      TX_RESP_Q002 == "." ~ "Branco",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_MT, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_LP, na.rm=T)
  ),
  # Agrupamento por raça/cor
  by = "TX_RESP_Q002"
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2019, ANO_SERIE = 9, RACA_COR, MEDIA_MT, MEDIA_LP
  )
]

#================================================================
#= 1.1. Análise de proficiência por raça/cor e município - 2019 =
#================================================================

saeb_9_ef_19_racacor_municipio <- saeb_9_ef_19[
  # Consistente entre os dados da aplicação do Saeb 2019 com o Censo da 
  # Educação Básica 2019 finalizado
  IN_SITUACAO_CENSO == 1 &
    # Escola pública
    IN_PUBLICA == 1,
  .(
    # Inclusão das opções de resposta para a pergunta: Qual é a sua cor ou 
    # raça?
    RACA_COR = case_when(
      TX_RESP_Q002 == "A" ~ "Branca.",
      TX_RESP_Q002 == "B" ~ "Preta.",
      TX_RESP_Q002 == "C" ~ "Parda.",
      TX_RESP_Q002 == "D" ~ "Amarela.",
      TX_RESP_Q002 == "E" ~ "Indígena.",
      TX_RESP_Q002 == "F" ~ "Não quero declarar.",
      TX_RESP_Q002 == "*" ~ "Nulo",
      TX_RESP_Q002 == "." ~ "Branco",
      .default = NA
    ),
    # Média ponderada da proficiência em matemática
    MEDIA_MT = sum(PROFICIENCIA_MT_SAEB, na.rm=T)/sum(PESO_ALUNO_MT, na.rm=T),
    # Média ponderada da proficiência em língua portuguesa
    MEDIA_LP = sum(PROFICIENCIA_LP_SAEB, na.rm=T)/sum(PESO_ALUNO_LP, na.rm=T)
  ),
  # Agrupamento por raça/cor
  by = c("ID_MUNICIPIO", "TX_RESP_Q002")
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2019, ANO_SERIE = 9, ID_MUNICIPIO, RACA_COR, MEDIA_MT, MEDIA_LP
  )
]

#==========================
#= 2. Exportação de dados =
#==========================

# Criação de lista com dataframes
lista_dfs <- list(
  TB_RACACOR_BRASIL    = saeb_9_ef_19_racacor,
  TB_RACACOR_MUNICIPIO = saeb_9_ef_19_racacor_municipio
)

# Salva tudo em um único arquivo Excel
write_xlsx(lista_dfs, path = here("dados", "TB_DESIGUALDADE_2019.xlsx"))

#=======================================================================
#= 3. Análise de proficiência por raça/cor - 2019 (microdados antigos) =
#=======================================================================

saeb_9_ef_19_racacor_dta <- saeb_19_dta[
  # Consistente entre os dados da aplicação do Saeb 2019 com o Censo da 
  # Educação Básica 2019 finalizado
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
  ),
  # Agrupamento por raça/cor
  by = "cor_raca"
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2019, ANO_SERIE = 9, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#====================================================================================
#= 3.1 Análise de proficiência por raça/cor e município - 2019 (microdados antigos) =
#====================================================================================

saeb_9_ef_19_racacor_municipio_dta <- saeb_19_dta[
  # Consistente entre os dados da aplicação do Saeb 2019 com o Censo da 
  # Educação Básica 2019 finalizado
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
  ),
  # Agrupamento por raça/cor
  by = c("id_municipio", "cor_raca")
][
  ,
  .(
    # Inclusão de variáveis de ano do Saeb e ano/série dos estudantes
    ANO_SAEB = 2019, ANO_SERIE = 9, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#==========================
#= 4. Exportação de dados =
#==========================

# Criação de lista com dataframes
lista_dfs <- list(
  TB_RACACOR_BRASIL    = saeb_9_ef_19_racacor_dta,
  TB_RACACOR_MUNICIPIO = saeb_9_ef_19_racacor_municipio_dta
)

# Salva tudo em um único arquivo Excel
write_xlsx(lista_dfs, path = here("dados", "TB_DESIGUALDADE_2019_COM_MUNICIPIO.xlsx"))
