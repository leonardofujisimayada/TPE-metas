#=======================================================================
#= 1. Análise de proficiência por raça/cor - 2015 (microdados antigos) =
#=======================================================================

saeb_9_ef_15_racacor_dta <- saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2015 com o Censo da 
  # Educação Básica 2015 finalizado
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
    ANO_SAEB = 2015, ANO_SERIE = 9, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#===========================================================================
#= 5º ano Análise de proficiência por raça/cor - 2015 (microdados antigos) =
#===========================================================================

saeb_5_ef_15_racacor_dta <- saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2015 com o Censo da 
  # Educação Básica 2015 finalizado
  in_situacao_censo == 1 &
    # Escola pública
    in_publica == 1 &
    # 9º ano do Ensino Fundamental
    id_serie == 5,
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
    ANO_SAEB = 2015, ANO_SERIE = 5, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#====================================================================================
#= 1.1 Análise de proficiência por raça/cor e município - 2015 (microdados antigos) =
#====================================================================================

saeb_9_ef_15_racacor_municipio_dta <- saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2015 com o Censo da 
  # Educação Básica 2015 finalizado
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
    ANO_SAEB = 2015, ANO_SERIE = 9, id_municipio, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#=======================================================================================
#= 5º ano Análise de proficiência por raça/cor e município - 2015 (microdados antigos) =
#=======================================================================================

saeb_5_ef_15_racacor_municipio_dta <- saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2015 com o Censo da 
  # Educação Básica 2015 finalizado
  in_situacao_censo == 1 &
    # Escola pública
    in_publica == 1 &
    # 9º ano do Ensino Fundamental
    id_serie == 5,
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
    ANO_SAEB = 2015, ANO_SERIE = 5, id_municipio, cor_raca, MEDIA_MT, MEDIA_LP
  )
]

#==========================
#= 2. Exportação de dados =
#==========================

# Criação de lista com dataframes
lista_dfs <- list(
  TB_RACACOR_BRASIL    = saeb_5_ef_15_racacor_dta,
  TB_RACACOR_MUNICIPIO = saeb_5_ef_15_racacor_municipio_dta
)

# Salva tudo em um único arquivo Excel
write_xlsx(lista_dfs, path = here("dados", "TB_DESIGUALDADE_2015_COM_MUNICIPIO.xlsx"))
