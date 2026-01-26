saeb_12_em_17[
  ID_SERIE==12 & IN_SITUACAO_CENSO==1 & ID_DEPENDENCIA_ADM %in% c(1, 2, 3),
  .(
    RACACOR = case_when(
      TX_RESP_Q002 %in% c("A", "D") ~ "Branca/Amarela",
      TX_RESP_Q002 == "B" ~ "Preta",
      TX_RESP_Q002 == "C" ~ "Parda",
      TX_RESP_Q002 == "E" ~ "Indígena",
      TX_RESP_Q002 == "F" ~ "Não declarado",
      .default = TX_RESP_Q002 
    ),
    ESCOLARIDADE_MAE = case_when(
      TX_RESP_Q019 == "A" ~ "Nunca estudou.",
      TX_RESP_Q019 == "B" ~ "Não completou a 4.ª série/5.º ano.",
      TX_RESP_Q019 == "C" ~ "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano.",
      TX_RESP_Q019 == "D" ~ "Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio.",
      TX_RESP_Q019 == "E" ~ "Completou o Ensino Médio, mas não completou a Faculdade.",
      TX_RESP_Q019 == "F" ~ "Completou a Faculdade.",
      TX_RESP_Q019 == "G" ~ "Não sei.",
      .default = TX_RESP_Q019,
    ),
    PESO_ALUNO_MT, 
    PROFICIENCIA_MT_SAEB
  )
][
  ,
  .(
    #QT_TOTAL = sum(PESO_ALUNO_MT, na.rm=T),
    MEDIA_MT_SAEB = mean(PROFICIENCIA_MT_SAEB, na.rm=T)
  ),
  by="ESCOLARIDADE_MAE"
][
  , 
  .(ESCOLARIDADE_MAE, QT_TOTAL, P = round(100*(QT_TOTAL/sum(QT_TOTAL)), 2))
]

saeb_12_em_23[
  ID_SERIE==12 & IN_SITUACAO_CENSO==1 & IN_PUBLICA==1,
  .(
    RACACOR = case_when(
      TX_RESP_Q04 %in% c("A", "D") ~ "Branca/Amarela",
      TX_RESP_Q04 == "B" ~ "Preta",
      TX_RESP_Q04 == "C" ~ "Parda",
      TX_RESP_Q04 == "E" ~ "Indígena",
      TX_RESP_Q04 == "F" ~ "Não declarado",
      TX_RESP_Q04 == "*" ~ "Nulo",
      TX_RESP_Q04 == "." ~ "Branco",
      .default = TX_RESP_Q04 
    ),
    ESCOLARIDADE_MAE = case_when(
      TX_RESP_Q08 == "A" ~ "Não completou o 5º ano do Ensino Fundamental.",
      TX_RESP_Q08 == "B" ~ "Ensino Fundamental, até o 5º ano.",
      TX_RESP_Q08 == "C" ~ "Ensino Fundamental completo.",
      TX_RESP_Q08 == "D" ~ "Ensino Médio completo.",
      TX_RESP_Q08 == "E" ~ "Ensino Superior completo (faculdade ou graduação).",
      TX_RESP_Q08 == "F" ~ "Não sei.",
      TX_RESP_Q08 == "." ~ "Branco",
      TX_RESP_Q08 == "*" ~ "Nulo",
      .default = TX_RESP_Q08,
    ),
    PESO_ALUNO_MT,
    PROFICIENCIA_MT_SAEB
  )
][
  ,
  .(
    #QT_TOTAL = sum(PESO_ALUNO_MT, na.rm=T),
    MEDIA_MT_SAEB = mean(PROFICIENCIA_MT_SAEB, na.rm=T)
  ),
  by="ESCOLARIDADE_MAE"
][
  , 
  .(ESCOLARIDADE_MAE, QT_TOTAL, P = round(100*(QT_TOTAL/sum(QT_TOTAL)), 2))
]

