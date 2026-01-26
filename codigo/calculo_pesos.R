pesos_13 <- saeb_13_dta[
  # Consistente entre os dados da aplicação do Saeb 2013 com o Censo da 
  # Educação Básica 2013 finalizado
  in_situacao_censo == 1 &
    # Escola pública não federal
    in_publica == 1 & id_dependencia_adm != 1,
  .(
    # Média ponderada da proficiência em matemática
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)
  ),
  # Agrupamento por município
  by = c("id_municipio", "id_serie")
]

pesos_15 <- saeb_15_dta[
  # Consistente entre os dados da aplicação do Saeb 2013 com o Censo da 
  # Educação Básica 2013 finalizado
  in_situacao_censo == 1 &
    # Escola pública não federal
    in_publica == 1 & id_dependencia_adm != 1,
  .(
    # Média ponderada da proficiência em matemática
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)
  ),
  # Agrupamento por município
  by = c("id_municipio", "id_serie")
]

pesos_17 <- saeb_17_dta[
  # Consistente entre os dados da aplicação do Saeb 2013 com o Censo da 
  # Educação Básica 2013 finalizado
  in_situacao_censo == 1 &
    # Escola pública não federal
    in_publica == 1 & id_dependencia_adm != 1,
  .(
    # Média ponderada da proficiência em matemática
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)
  ),
  # Agrupamento por município
  by = c("id_municipio", "id_serie")
]

pesos_19 <- saeb_19_dta[
  # Consistente entre os dados da aplicação do Saeb 2013 com o Censo da 
  # Educação Básica 2013 finalizado
  in_situacao_censo == 1 &
    # Escola pública não federal
    in_publica == 1 & id_dependencia_adm != 1,
  .(
    # Média ponderada da proficiência em matemática
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)
  ),
  # Agrupamento por município
  by = c("id_municipio", "id_serie")
]

writexl::write_xlsx(x = pesos_13, path = "pesos_13.xlsx")
writexl::write_xlsx(x = pesos_15, path = "pesos_15.xlsx")
writexl::write_xlsx(x = pesos_17, path = "pesos_17.xlsx")
writexl::write_xlsx(x = pesos_19, path = "pesos_19.xlsx")




