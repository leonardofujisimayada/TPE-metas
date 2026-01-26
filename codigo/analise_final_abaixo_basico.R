#===========
#= QUINTIS =
#===========

# Cálculo dos quintis inferior e superior
q_mun_13 <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_15 <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_17 <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_19 <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

#====================
#= 5º ano EF - 2013 = 
#==================== 

# Cálculo de aprendizagem adequada
saeb_13_dta_aj <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 5,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 175) | (proficiencia_lp_saeb < 150), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_13_dta_aj <- merge(x=saeb_13_dta_aj, y=q_mun_13, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_13_dta_aj_racacor <- reshape2::dcast(data = saeb_13_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_13_dta_aj_renda <- saeb_13_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_13_dta_aj_renda <- reshape2::dcast(data = saeb_13_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2013_5ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_13_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2013_5ano_ABAIXO <- merge(x=tb_2013_5ano_ABAIXO, y=saeb_13_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_13_dta_aj_renda, saeb_13_dta_aj_racacor, saeb_13_dta_aj)
gc()

#====================
#= 5º ano EF - 2015 = 
#====================

# Cálculo de aprendizagem adequada
saeb_15_dta_aj <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 5,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 175) | (proficiencia_lp_saeb < 150), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_15_dta_aj <- merge(x=saeb_15_dta_aj, y=q_mun_15, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_15_dta_aj_racacor <- reshape2::dcast(data = saeb_15_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_15_dta_aj_renda <- saeb_15_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_15_dta_aj_renda <- reshape2::dcast(data = saeb_15_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2015_5ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_15_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2015_5ano_ABAIXO <- merge(x=tb_2015_5ano_ABAIXO, y=saeb_15_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_15_dta_aj_renda, saeb_15_dta_aj_racacor, saeb_15_dta_aj)
gc()

#====================
#= 5º ano EF - 2017 = 
#====================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 5,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 175) | (proficiencia_lp_saeb < 150), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor <- reshape2::dcast(data = saeb_17_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda <- reshape2::dcast(data = saeb_17_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2017_5ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_5ano_ABAIXO <- merge(x=tb_2017_5ano_ABAIXO, y=saeb_17_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#====================
#= 5º ano EF - 2019 = 
#====================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 5,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 175) | (proficiencia_lp_saeb < 150), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor <- reshape2::dcast(data = saeb_19_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda <- reshape2::dcast(data = saeb_19_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2019_5ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_5ano_ABAIXO <- merge(x=tb_2019_5ano_ABAIXO, y=saeb_19_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

addWorksheet(wb, "2013")
writeData(wb, "2013", tb_2013_5ano_ABAIXO)

addWorksheet(wb, "2015")
writeData(wb, "2015", tb_2015_5ano_ABAIXO)

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_5ano_ABAIXO)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_5ano_ABAIXO)

saveWorkbook(wb, "dados/20260114_resultado_5ano_abaixo_basico.xlsx", overwrite = TRUE)
#====================
#= 9º ano EF - 2013 = 
#==================== 

# Cálculo de aprendizagem adequada
saeb_13_dta_aj <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 225) | (proficiencia_lp_saeb < 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_13_dta_aj <- merge(x=saeb_13_dta_aj, y=q_mun_13, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_13_dta_aj_racacor <- reshape2::dcast(data = saeb_13_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_13_dta_aj_renda <- saeb_13_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_13_dta_aj_renda <- reshape2::dcast(data = saeb_13_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2013_9ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_13_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2013_9ano_ABAIXO <- merge(x=tb_2013_9ano_ABAIXO, y=saeb_13_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_13_dta_aj_renda, saeb_13_dta_aj_racacor, saeb_13_dta_aj)
gc()

#====================
#= 9º ano EF - 2015 = 
#====================

# Cálculo de aprendizagem adequada
saeb_15_dta_aj <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 225) | (proficiencia_lp_saeb < 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_15_dta_aj <- merge(x=saeb_15_dta_aj, y=q_mun_15, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_15_dta_aj_racacor <- reshape2::dcast(data = saeb_15_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_15_dta_aj_renda <- saeb_15_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_15_dta_aj_renda <- reshape2::dcast(data = saeb_15_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2015_9ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_15_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2015_9ano_ABAIXO <- merge(x=tb_2015_9ano_ABAIXO, y=saeb_15_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_15_dta_aj_renda, saeb_15_dta_aj_racacor, saeb_15_dta_aj)
gc()

#====================
#= 9º ano EF - 2017 = 
#====================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 225) | (proficiencia_lp_saeb < 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor <- reshape2::dcast(data = saeb_17_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda <- reshape2::dcast(data = saeb_17_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2017_9ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_9ano_ABAIXO <- merge(x=tb_2017_9ano_ABAIXO, y=saeb_17_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#====================
#= 9º ano EF - 2019 = 
#====================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 9,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 225) | (proficiencia_lp_saeb < 200), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor <- reshape2::dcast(data = saeb_19_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda <- reshape2::dcast(data = saeb_19_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2019_9ano_ABAIXO <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_9ano_ABAIXO <- merge(x=tb_2019_9ano_ABAIXO, y=saeb_19_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

addWorksheet(wb, "2013")
writeData(wb, "2013", tb_2013_9ano_ABAIXO)

addWorksheet(wb, "2015")
writeData(wb, "2015", tb_2015_9ano_ABAIXO)

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_9ano_ABAIXO)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_9ano_ABAIXO)

saveWorkbook(wb, "dados/20260114_resultado_9ano_abaixo_basico.xlsx", overwrite = TRUE)
#======================
#= 3ª série EM - 2017 = 
#======================

# Cálculo de aprendizagem adequada
saeb_17_dta_aj <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 12,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 275) | (proficiencia_lp_saeb < 250), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_17_dta_aj <- merge(x=saeb_17_dta_aj, y=q_mun_17, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_17_dta_aj_racacor <- reshape2::dcast(data = saeb_17_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_17_dta_aj_renda <- saeb_17_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_17_dta_aj_renda <- reshape2::dcast(data = saeb_17_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2017_12serie_ABAIXO <- merge(x=tb_subgrupos, y=saeb_17_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2017_12serie_ABAIXO <- merge(x=tb_2017_12serie_ABAIXO, y=saeb_17_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_17_dta_aj_renda, saeb_17_dta_aj_racacor, saeb_17_dta_aj)
gc()

#======================
#= 3ª série EM - 2019 = 
#======================

# Cálculo de aprendizagem adequada
saeb_19_dta_aj <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 12,
  .(
    id_prova_brasil,
    id_municipio,
    id_escola,
    inse_aluno,
    peso_aluno_mt,
    peso_aluno_lp,
    proficiencia_mt_saeb,
    proficiencia_lp_saeb,
    APR_ABAIXO = ifelse((proficiencia_mt_saeb < 275) | (proficiencia_lp_saeb < 250), 1 * peso_aluno_mt, 0),
    cor_raca,
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = cor_raca
    )
  )
]

# Inclusão de dados de quintis
saeb_19_dta_aj <- merge(x=saeb_19_dta_aj, y=q_mun_19, by="id_municipio")
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
    APR_ABAIXO,
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
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "cor_raca_aj")
]
saeb_19_dta_aj_racacor <- reshape2::dcast(data = saeb_19_dta_aj_racacor, formula = id_municipio ~ cor_raca_aj)

# Agrupamento por município (renda)
saeb_19_dta_aj_renda <- saeb_19_dta_aj[
  !(is.na(quintil)),
  .(APR_ABAIXO = sum(APR_ABAIXO, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by=c("id_municipio", "quintil")
]
saeb_19_dta_aj_renda <- reshape2::dcast(data = saeb_19_dta_aj_renda, formula = id_municipio ~ quintil)

# Tabela final
tb_2019_12serie_ABAIXO <- merge(x=tb_subgrupos, y=saeb_19_dta_aj_racacor, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)
tb_2019_12serie_ABAIXO <- merge(x=tb_2019_12serie_ABAIXO, y=saeb_19_dta_aj_renda, by.x='CO_MUNICIPIO', by.y='id_municipio', all.x=T)

# Remoção de tabelas não utilizadas
rm(saeb_19_dta_aj_renda, saeb_19_dta_aj_racacor, saeb_19_dta_aj)
gc()

# Exportação
wb <- createWorkbook()

addWorksheet(wb, "2017")
writeData(wb, "2017", tb_2017_12serie_ABAIXO)

addWorksheet(wb, "2019")
writeData(wb, "2019", tb_2019_12serie_ABAIXO)

saveWorkbook(wb, "dados/20260114_resultado_12serie_abaixo_basico.xlsx", overwrite = TRUE)