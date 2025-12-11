#===========================================
#= 1. Análise de proficiência por raça/cor =
#===========================================

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

#=======================================================
#= 2. Análise de proficiência por nível socioeconômico =
#=======================================================

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






