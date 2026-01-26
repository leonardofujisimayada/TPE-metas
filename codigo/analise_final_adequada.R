#====================
#= 5º ano EF - 2013 = 
#==================== 

# Cálculo de aprendizagem adequada
saeb_13_dta_aj <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = "Outros"
    )
  )
]

# Inclusão de dados de quintis
saeb_13_dta_aj <- merge(x=saeb_13_dta_aj, y=q_mun_13_5, by="id_municipio")
saeb_13_dta_aj <- saeb_13_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]

# Agrupamento por município (raça/cor)
saeb_13_dta_aj_racacor <- saeb_13_dta_aj[
  #cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_13_dta_aj_racacor_reshaped <- saeb_13_dta_aj_racacor %>%
  pivot_wider(
    names_from = cor_raca_aj,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Agrupamento por município (renda)
saeb_13_dta_aj_renda <- saeb_13_dta_aj[
  #!(is.na(quintil)),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T),
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_13_dta_aj_renda_reshaped <- saeb_13_dta_aj_renda %>%
  pivot_wider(
    names_from = quintil,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Tabela final
tb_2013_5ano <- merge(x=tb_subgrupos, y=saeb_13_dta_aj_racacor_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2013_5ano <- merge(x=tb_2013_5ano, y=saeb_13_dta_aj_renda_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2013_5ano <- tb_2013_5ano %>% 
  mutate(
    DIFF_RACA = round(`APR_ADEQ_Branca/Amarela`-`APR_ADEQ_Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`APR_ADEQ_Quintil superior`-`APR_ADEQ_Quintil inferior`, 4),
    ano = 2013,
    serie = 5
  )

# Remoção de tabelas não utilizadas
rm(saeb_13_dta_aj_renda, saeb_13_dta_aj_racacor, saeb_13_dta_aj)
gc()

#====================
#= 5º ano EF - 2015 = 
#====================

# Cálculo de aprendizagem adequada
saeb_15_dta_aj <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = "Outros"
    )
  )
]

# Inclusão de dados de quintis
saeb_15_dta_aj <- merge(x=saeb_15_dta_aj, y=q_mun_15_5, by="id_municipio")
saeb_15_dta_aj <- saeb_15_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_15_dta_aj_racacor <- saeb_15_dta_aj[
  #cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_15_dta_aj_racacor_reshaped <- saeb_15_dta_aj_racacor %>%
  pivot_wider(
    names_from = cor_raca_aj,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Agrupamento por município (renda)
saeb_15_dta_aj_renda <- saeb_15_dta_aj[
  #!(is.na(quintil)),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T),
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_15_dta_aj_renda_reshaped <- saeb_15_dta_aj_renda %>%
  pivot_wider(
    names_from = quintil,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Tabela final
tb_2015_5ano <- merge(x=tb_subgrupos, y=saeb_15_dta_aj_racacor_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2015_5ano <- merge(x=tb_2015_5ano, y=saeb_15_dta_aj_renda_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2015_5ano <- tb_2015_5ano %>% 
  mutate(
    DIFF_RACA = round(`APR_ADEQ_Branca/Amarela`-`APR_ADEQ_Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`APR_ADEQ_Quintil superior`-`APR_ADEQ_Quintil inferior`, 4),
    ano = 2015,
    serie = 5
  )

# Remoção de tabelas não utilizadas
rm(saeb_15_dta_aj_renda, saeb_15_dta_aj_racacor, saeb_15_dta_aj)
gc()

#====================
#= 5º ano EF - 2017 = 
#====================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = "Outros"
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17_5, by="id_municipio")
saeb_17_dta_aj <- saeb_17_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_17_dta_aj_racacor <- saeb_17_dta_aj[
  #cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor_reshaped <- saeb_17_dta_aj_racacor %>%
  pivot_wider(
    names_from = cor_raca_aj,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  #!(is.na(quintil)),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T),
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda_reshaped <- saeb_17_dta_aj_renda %>%
  pivot_wider(
    names_from = quintil,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Tabela final
tb_2017_5ano <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_5ano <- merge(x=tb_2017_5ano, y=saeb_17_dta_aj_renda_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2017_5ano <- tb_2017_5ano %>% 
  mutate(
    DIFF_RACA = round(`APR_ADEQ_Branca/Amarela`-`APR_ADEQ_Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`APR_ADEQ_Quintil superior`-`APR_ADEQ_Quintil inferior`, 4),
    ano = 2017,
    serie = 5
  )

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#====================
#= 5º ano EF - 2019 = 
#====================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = "Outros"
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19_5, by="id_municipio")
saeb_19_dta_aj <- saeb_19_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_19_dta_aj_racacor <- saeb_19_dta_aj[
  #cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor_reshaped <- saeb_19_dta_aj_racacor %>%
  pivot_wider(
    names_from = cor_raca_aj,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  #!(is.na(quintil)),
  ,
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T),
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda_reshaped <- saeb_19_dta_aj_renda %>%
  pivot_wider(
    names_from = quintil,
    values_from = c(APR_ADEQ, QT_ALUNO),
    names_sep = "_"
  )

# Tabela final
tb_2019_5ano <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_5ano <- merge(x=tb_2019_5ano, y=saeb_19_dta_aj_renda_reshaped, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2019_5ano <- tb_2019_5ano %>% 
  mutate(
    DIFF_RACA = round(`APR_ADEQ_Branca/Amarela`-`APR_ADEQ_Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`APR_ADEQ_Quintil superior`-`APR_ADEQ_Quintil inferior`, 4),
    ano = 2019,
    serie = 5
  )

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

addWorksheet(wb, "2013")
writeData(wb, "2013", tb_2013_5ano)

addWorksheet(wb, "2015")
writeData(wb, "2015", tb_2015_5ano)

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_5ano)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_5ano)

saveWorkbook(wb, "dados/20260118_resultado_5ano.xlsx", overwrite = TRUE)


#====================
#= 9º ano EF - 2013 = 
#==================== 

# Cálculo de aprendizagem adequada
saeb_13_dta_aj <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 300) & (proficiencia_lp_saeb >= 275), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_13_dta_aj <- merge(x=saeb_13_dta_aj, y=q_mun_13_9, by="id_municipio")
saeb_13_dta_aj <- saeb_13_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_13_dta_aj_racacor <- saeb_13_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_13_dta_aj_racacor <- reshape2::dcast(data = saeb_13_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_13_dta_aj_renda <- saeb_13_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_13_dta_aj_renda <- reshape2::dcast(data = saeb_13_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2013_9ano <- merge(x=tb_subgrupos, y=saeb_13_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2013_9ano <- merge(x=tb_2013_9ano, y=saeb_13_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2013_9ano <- tb_2013_9ano %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2013,
    serie = 9
  )

# Remoção de tabelas não utilizadas
rm(saeb_13_dta_aj_renda, saeb_13_dta_aj_racacor, saeb_13_dta_aj)
gc()

#====================
#= 9º ano EF - 2015 = 
#====================

# Cálculo de aprendizagem adequada
saeb_15_dta_aj <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 300) & (proficiencia_lp_saeb >= 275), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_15_dta_aj <- merge(x=saeb_15_dta_aj, y=q_mun_15_9, by="id_municipio")
saeb_15_dta_aj <- saeb_15_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_15_dta_aj_racacor <- saeb_15_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_15_dta_aj_racacor <- reshape2::dcast(data = saeb_15_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_15_dta_aj_renda <- saeb_15_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_15_dta_aj_renda <- reshape2::dcast(data = saeb_15_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2015_9ano <- merge(x=tb_subgrupos, y=saeb_15_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2015_9ano <- merge(x=tb_2015_9ano, y=saeb_15_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2015_9ano <- tb_2015_9ano %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2015,
    serie = 9
  ) %>% 
  select(
    CO_MUNICIPIO, DIFF_RACA, DIFF_RENDA, ano, serie, cluster
  )

# Remoção de tabelas não utilizadas
rm(saeb_15_dta_aj_renda, saeb_15_dta_aj_racacor, saeb_15_dta_aj)
gc()

#====================
#= 9º ano EF - 2017 = 
#====================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 300) & (proficiencia_lp_saeb >= 275), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17_9, by="id_municipio")
saeb_17_dta_aj <- saeb_17_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_17_dta_aj_racacor <- saeb_17_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor <- reshape2::dcast(data = saeb_17_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda <- reshape2::dcast(data = saeb_17_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2017_9ano <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_9ano <- merge(x=tb_2017_9ano, y=saeb_17_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2017_9ano <- tb_2017_9ano %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2017,
    serie = 9
  ) %>% 
  select(
    CO_MUNICIPIO, DIFF_RACA, DIFF_RENDA, ano, serie, cluster
  )

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#====================
#= 9º ano EF - 2019 = 
#====================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 300) & (proficiencia_lp_saeb >= 275), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19_9, by="id_municipio")
saeb_19_dta_aj <- saeb_19_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_19_dta_aj_racacor <- saeb_19_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor <- reshape2::dcast(data = saeb_19_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T), 
    QT_ALUNO = sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda <- reshape2::dcast(data = saeb_19_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2019_9ano <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_9ano <- merge(x=tb_2019_9ano, y=saeb_19_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2019_9ano <- tb_2019_9ano %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2019,
    serie = 9
  ) %>% 
  select(
    CO_MUNICIPIO, DIFF_RACA, DIFF_RENDA, ano, serie, cluster
  )

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

addWorksheet(wb, "2013")
writeData(wb, "2013", tb_2013_9ano)

addWorksheet(wb, "2015")
writeData(wb, "2015", tb_2015_9ano)

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_9ano)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_9ano)

saveWorkbook(wb, "dados/20260121_resultado_9ano.xlsx", overwrite = TRUE)


#======================
#= 3ª série EM - 2013 = 
#====================== 

# Cálculo de aprendizagem adequada
saeb_13_dta_aj <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 350) & (proficiencia_lp_saeb >= 300), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_13_dta_aj <- merge(x=saeb_13_dta_aj, y=q_mun_13_12, by="id_municipio")
saeb_13_dta_aj <- saeb_13_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_13_dta_aj_racacor <- saeb_13_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_13_dta_aj_racacor <- reshape2::dcast(data = saeb_13_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_13_dta_aj_renda <- saeb_13_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_13_dta_aj_renda <- reshape2::dcast(data = saeb_13_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2013_12serie <- merge(x=tb_subgrupos, y=saeb_13_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2013_12serie <- merge(x=tb_2013_12serie, y=saeb_13_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_13_dta_aj_renda, saeb_13_dta_aj_racacor, saeb_13_dta_aj)
gc()

#======================
#= 3ª série EM - 2015 = 
#======================

# Cálculo de aprendizagem adequada
saeb_15_dta_aj <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 350) & (proficiencia_lp_saeb >= 300), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_15_dta_aj <- merge(x=saeb_15_dta_aj, y=q_mun_15_12, by="id_municipio")
saeb_15_dta_aj <- saeb_15_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_15_dta_aj_racacor <- saeb_15_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_15_dta_aj_racacor <- reshape2::dcast(data = saeb_15_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_15_dta_aj_renda <- saeb_15_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_15_dta_aj_renda <- reshape2::dcast(data = saeb_15_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2015_12serie <- merge(x=tb_subgrupos, y=saeb_15_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2015_12serie <- merge(x=tb_2015_12serie, y=saeb_15_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_15_dta_aj_renda, saeb_15_dta_aj_racacor, saeb_15_dta_aj)
gc()

#======================
#= 3ª série EM - 2017 = 
#======================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm == 2,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 350) & (proficiencia_lp_saeb >= 300), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17_12, by="id_municipio")
saeb_17_dta_aj <- saeb_17_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_17_dta_aj_racacor <- saeb_17_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor <- reshape2::dcast(data = saeb_17_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda <- reshape2::dcast(data = saeb_17_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2017_12serie <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_12serie <- merge(x=tb_2017_12serie, y=saeb_17_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2017_12serie <- tb_2017_12serie %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2017,
    serie = 12
  ) %>% 
  select(
    CO_MUNICIPIO, DIFF_RACA, DIFF_RENDA, ano, serie, cluster
  )

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#======================
#= 3ª série EM - 2019 = 
#======================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm == 2,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 350) & (proficiencia_lp_saeb >= 300), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19_12, by="id_municipio")
saeb_19_dta_aj <- saeb_19_dta_aj[
  ,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ADEQ,
    cor_raca,
    cor_raca_aj,
    quintil = case_when(
      inse_aluno < q20 ~ "Quintil inferior",
      inse_aluno < q80 ~ "Quintil superior",
      .default = NA
    )
  )
]


# Agrupamento por município (raça/cor)
saeb_19_dta_aj_racacor <- saeb_19_dta_aj[
  cor_raca %in% c("Branca", "Amarela", "Preta", "Parda", "Indígena"),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor <- reshape2::dcast(data = saeb_19_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  !(is.na(quintil)),
  .(APR_ADEQ = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda <- reshape2::dcast(data = saeb_19_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2019_12serie <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_12serie <- merge(x=tb_2019_12serie, y=saeb_19_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

tb_2019_12serie <- tb_2019_12serie %>% 
  mutate(
    DIFF_RACA = round(`Branca/Amarela`-`Preta/Parda/Indígena`, 4),
    DIFF_RENDA = round(`Quintil superior`-`Quintil inferior`, 4),
    ano = 2019,
    serie = 12
  ) %>% 
  select(
    CO_MUNICIPIO, DIFF_RACA, DIFF_RENDA, ano, serie, cluster
  )

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

#addWorksheet(wb, "2013")
#writeData(wb, "2013", tb_2013_12serie)

#addWorksheet(wb, "2015")
#writeData(wb, "2015", tb_2015_12serie)

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_12serie)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_12serie)

saveWorkbook(wb, "dados/20260114_resultado_12serie.xlsx", overwrite = TRUE)

