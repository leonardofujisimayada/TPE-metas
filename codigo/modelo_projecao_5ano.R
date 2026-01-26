#================================
#= 5º ano do Ensino Fundamental = 
#================================

#===============================================================
#= Filtro de estudantes do 5º ano do Ensino Fundamental - 2013 = 
#===============================================================

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

#===============================================================
#= Filtro de estudantes do 5º ano do Ensino Fundamental - 2015 = 
#===============================================================

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

#===============================================================
#= Filtro de estudantes do 5º ano do Ensino Fundamental - 2017 = 
#===============================================================

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

#===============================================================
#= Filtro de estudantes do 5º ano do Ensino Fundamental - 2019 = 
#===============================================================


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

#========================================================================
#= Preparação dos dados do 5º ano do Ensino Fundamental para a projeção = 
#========================================================================

# Junção dos dados do 5º ano
tb_5ano <- rbind(tb_2013_5ano, tb_2015_5ano, tb_2017_5ano, tb_2019_5ano)
qt_cluster_mun <- tb_5ano %>% filter(ano==2019) %>% group_by(ano, cluster) %>% reframe(qt_aluno=sum(`QT_ALUNO_Preta/Parda/Indígena`, na.rm=T)+sum(`APR_ADEQ_Branca/Amarela`, na.rm=T)+sum(APR_ADEQ_Outros, na.rm=T))

# Exclusão dos municípios que não possuem dados de aprendizagem para o 5º ano
# Eram 5.568 municípios
# Após exclusão dos municípios sem estudantes do 5º ano em 2013 sobraram 5.343

#============================
#= Projeção raça/cor 5º ano =
#============================

tb_5ano_sna <- tb_5ano %>% 
  filter(!(is.na(`QT_ALUNO_Preta/Parda/Indígena`)) & !(is.na(`QT_ALUNO_Branca/Amarela`)) & !(is.na(QT_ALUNO_Outros))) %>% 
  rename(tx_adq_ppi=`APR_ADEQ_Preta/Parda/Indígena`, tx_adq_brancos=`APR_ADEQ_Branca/Amarela`, tx_adq_outros=APR_ADEQ_Outros, tx_adq_q5=`APR_ADEQ_Quintil superior`, tx_adq_q1=`APR_ADEQ_Quintil inferior`, tx_adq_qoutros=APR_ADEQ_NA, diff_raca=DIFF_RACA, diff_renda=DIFF_RENDA, qt_ppi=`QT_ALUNO_Preta/Parda/Indígena`, qt_brancos=`QT_ALUNO_Branca/Amarela`, qt_outros=QT_ALUNO_Outros) %>%
  mutate(peso_ppi = qt_ppi/(qt_ppi+qt_brancos+qt_outros), peso_brancos = qt_brancos/(qt_ppi+qt_brancos+qt_outros), peso_outros = qt_outros/(qt_ppi+qt_brancos+qt_outros)) %>% 
  select(CO_MUNICIPIO, ano, cluster, tx_adq_ppi, tx_adq_brancos, qt_ppi, qt_brancos, qt_outros, peso_ppi, peso_brancos, peso_outros, diff_raca)

# Cálculo dos fatores de redistribuição por cluster
tb_5ano_sna_racacor <- tb_5ano_sna %>% 
  filter(diff_raca > 0) %>% 
  pivot_wider(id_cols = c(CO_MUNICIPIO, cluster), names_prefix = "tx_", names_from = ano, values_from = diff_raca) %>% 
  mutate(referencia = ifelse((tx_2019 < tx_2017), 1, 0)) %>% 
  filter(referencia == 1) %>%
  select(CO_MUNICIPIO, cluster, tx_2017, tx_2019) %>% 
  group_by(cluster) %>%
  arrange(tx_2019-tx_2017) %>% 
  slice_min(order_by = tx_2019-tx_2017, prop = 0.5, with_ties = FALSE) %>% 
  reframe(tx_2017=median(tx_2017, na.rm = TRUE), tx_2019=median(tx_2019, na.rm = TRUE), qt_municípios=n()) %>% 
  mutate(diff=abs(tx_2019-tx_2017))
tb_5ano_sna_racacor <- tb_5ano_sna_racacor %>% left_join(qt_cluster_mun, by="cluster") 

# Fator de redistribuição para o Brasil
total_alunos <- tb_5ano %>% filter(ano==2019) %>% group_by(cluster) %>% reframe(qt_aluno_total = sum(`QT_ALUNO_Branca/Amarela`, na.rm=T)+sum(`QT_ALUNO_Preta/Parda/Indígena`, na.rm=T)+sum(QT_ALUNO_Outros, na.rm=T))
tb_5ano_sna_racacor <- tb_5ano_sna_racacor %>% left_join(total_alunos, by="cluster") 

fator_redistribuicao_brasil <- tb_5ano_sna_racacor %>%
  summarise(
    diff_brasil =
      sum(diff * qt_aluno_total, na.rm = TRUE) /
      sum(qt_aluno_total, na.rm = TRUE)
  ) %>%
  pull(diff_brasil)
fator_redistribuicao_brasil

# Pesos de cada raça/cor
pesos_racacor <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(2, 3),
  .(
    cor_raca_aj = case_when(
      cor_raca %in% c("Branca", "Amarela") ~ "Branca/Amarela",
      cor_raca %in% c("Preta", "Parda", "Indígena") ~ "Preta/Parda/Indígena",
      .default = "Outros"
    ),
    peso_aluno_mt
  )
][
  ,
  .(qt_aluno_total = sum(peso_aluno_mt, na.rm=T)),
  by="cor_raca_aj"
][,
  .(cor_raca_aj, qt_aluno_total = qt_aluno_total/sum(qt_aluno_total))
]

# Dados fixos
w_ppi <- as.numeric(subset(pesos_racacor, cor_raca_aj == "Preta/Parda/Indígena")$qt_aluno_total)
w_ba  <- as.numeric(subset(pesos_racacor, cor_raca_aj == "Branca/Amarela")$qt_aluno_total)
w_out <- as.numeric(subset(pesos_racacor, cor_raca_aj == "Outros")$qt_aluno_total)

ponto_partida_23 <- saeb_5_ef_23 %>% 
  filter(IN_PUBLICA==1 & IN_SITUACAO_CENSO==1) %>% 
  mutate(cor_raca = case_when(TX_RESP_Q04 %in% c("A", "D") ~ "Brancos", TX_RESP_Q04 %in% c("B", "C", "E") ~ "PPI", .default = "Outros"),
         APR_ADEQ = ifelse((PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200), 1 * PESO_ALUNO_MT, 0)) %>% 
  group_by(cor_raca) %>% 
  reframe(tx=sum(APR_ADEQ, na.rm=T) / sum(PESO_ALUNO_MT, na.rm=T))

# Valores iniciais (2023)
P0 <- as.numeric(subset(ponto_partida_23, cor_raca == "PPI")$tx)
B0 <- as.numeric(subset(ponto_partida_23, cor_raca == "Brancos")$tx)
O0 <- as.numeric(subset(ponto_partida_23, cor_raca == "Outros")$tx)

# Preparar ganhos do Brasil
df_brasil_proj <- df_brasil_proj %>%
  arrange(ano) %>%
  mutate(
    ganho_total = tx_brasil - lag(tx_brasil)
  )

# Teto 100%
aplicar_teto_redistribuir <- function(vals, pesos, teto = 1) {
  
  pesos <- pesos[names(vals)]
  
  excedente <- sum(pmax(vals - teto, 0) * pesos, na.rm = TRUE)
  vals <- pmin(vals, teto)
  
  if (excedente > 0) {
    elegiveis <- which(vals < teto - 1e-12)
    
    if (length(elegiveis) > 0) {
      soma_pesos <- sum(pesos[elegiveis])
      
      for (i in elegiveis) {
        nm <- names(vals)[i]
        vals[nm] <- vals[nm] + excedente / soma_pesos
      }
    }
  }
  
  vals
}

# Projeção iterativa
proj_racacor <- accumulate(
  .x = seq_len(nrow(df_brasil_proj)),
  .init = tibble(
    ano = df_brasil_proj$ano[1],
    tx_ppi = P0,
    tx_ba  = B0,
    tx_outros = O0
  ),
  .f = function(prev, i) {
    
    if (i == 1) return(prev)
    
    deltaT <- df_brasil_proj$ganho_total[i]
    T_next <- df_brasil_proj$tx_brasil[i]
    
    # --------------------------------------------------
    # REGIME 3 — Pós-igualdade (todos crescem iguais)
    # --------------------------------------------------
    if (abs(prev$tx_ppi - prev$tx_ba) < 1e-10) {
      
      delta_level <- deltaT / (w_ppi + w_ba + w_out)
      
      vals <- c(
        tx_ppi = prev$tx_ppi + delta_level,
        tx_ba  = prev$tx_ba  + delta_level,
        tx_outros = prev$tx_outros + delta_level
      )
      
      vals <- aplicar_teto_redistribuir(
        vals,
        pesos = c(tx_ppi = w_ppi, tx_ba = w_ba, tx_outros = w_out)
      )
      
      return(tibble(
        ano = df_brasil_proj$ano[i],
        tx_ppi = as.numeric(vals["tx_ppi"]),
        tx_ba  = as.numeric(vals["tx_ba"]),
        tx_outros = as.numeric(vals["tx_outros"])
      ))
    }
    
    # --------------------------------------------------
    # REGIME 1 — Desigual com bônus (PPI favorecido)
    # Crescimento proporcional ao peso
    # --------------------------------------------------
    
    # crescimento base em nível
    delta_base <- deltaT / (w_ppi + w_ba + w_out)
    
    dP_eq <- delta_base
    dB_eq <- delta_base
    dO_eq <- delta_base
    
    # bônus crescente do PPI
    #kappa_t <- df_brasil_proj$kappa_t[i]
    d_t <- fator_redistribuicao_brasil * deltaT
    
    dP_des <- dP_eq + d_t
    custo  <- w_ppi * d_t
    
    # financiamento proporcional ao peso
    peso_outros <- w_ba + w_out
    
    dB_des <- dB_eq - (custo * (w_ba / peso_outros)) / w_ba
    dO_des <- dO_eq - (custo * (w_out / peso_outros)) / w_out
    
    # valores candidatos
    vals <- c(
      tx_ppi = prev$tx_ppi + dP_des,
      tx_ba  = prev$tx_ba  + dB_des,
      tx_outros = prev$tx_outros + dO_des
    )
    
    vals <- aplicar_teto_redistribuir(
      vals,
      pesos = c(tx_ppi = w_ppi, tx_ba = w_ba, tx_outros = w_out)
    )
    
    P_cand <- as.numeric(vals["tx_ppi"])
    B_cand <- as.numeric(vals["tx_ba"])
    O_cand <- as.numeric(vals["tx_outros"])
    
    # --------------------------------------------------
    # REGIME 2 — Encostar sem ultrapassar (PPI = BA)
    # --------------------------------------------------
    if (!is.na(P_cand) && !is.na(B_cand) && P_cand >= B_cand) {
      
      x <- (T_next - w_out * O_cand) / (w_ppi + w_ba)
      
      vals <- c(
        tx_ppi = x,
        tx_ba  = x,
        tx_outros = O_cand
      )
      
      vals <- aplicar_teto_redistribuir(
        vals,
        pesos = c(tx_ppi = w_ppi, tx_ba = w_ba, tx_outros = w_out)
      )
      
      return(tibble(
        ano = df_brasil_proj$ano[i],
        tx_ppi = as.numeric(vals["tx_ppi"]),
        tx_ba  = as.numeric(vals["tx_ba"]),
        tx_outros = as.numeric(vals["tx_outros"])
      ))
    }
    
    # --------------------------------------------------
    # Continua no regime desigual
    # --------------------------------------------------
    tibble(
      ano = df_brasil_proj$ano[i],
      tx_ppi = P_cand,
      tx_ba  = B_cand,
      tx_outros = O_cand
    )
  }
) %>%
  bind_rows() %>%
  slice(-1)

# Brasil fecha
proj_racacor %>%
  left_join(df_brasil_proj, by = "ano") %>%
  mutate(
    brasil_calc = w_ppi*tx_ppi + w_ba*tx_ba + w_out*tx_outros,
    erro = brasil_calc - tx_brasil
  )

proj_racacor

# Inclusão do histórico

df_historico <- saeb_racacor_5 %>% 
  filter(ano != 2023) %>% 
  pivot_wider(id_cols=ano, names_from=cor_raca, values_from = p) %>% 
  rename(tx_ba = Brancos, tx_ppi = PPI, tx_outros = Outros)

df_raca_proj <- rbind(proj_racacor, df_historico)
df_raca_proj <- df_raca_proj %>% select(ano, tx_ppi, tx_ba, tx_outros) %>% arrange(ano)

proj_brasil_grupos <- df_raca_proj %>% 
  pivot_longer(cols=c(tx_ppi, tx_ba, tx_outros), names_to = "grupo") %>% 
  mutate(periodo = ifelse(ano < 2025, "Valor observado", "Projeção"), value=value*100) %>% 
  rename(tx_brasil=value) 


#=========================
#= Projeção renda 5º ano =
#=========================

tb_5ano_sna <- tb_5ano %>% 
  filter(!(is.na(`QT_ALUNO_Quintil inferior`)) & !(is.na(`QT_ALUNO_Quintil superior`)) & !(is.na(QT_ALUNO_NA))) %>% 
  rename(tx_adq_ppi=`APR_ADEQ_Preta/Parda/Indígena`, tx_adq_brancos=`APR_ADEQ_Branca/Amarela`, tx_adq_outros=APR_ADEQ_Outros, tx_adq_q5=`APR_ADEQ_Quintil superior`, tx_adq_q1=`APR_ADEQ_Quintil inferior`, tx_adq_qoutros=APR_ADEQ_NA, diff_raca=DIFF_RACA, diff_renda=DIFF_RENDA, qt_ppi=`QT_ALUNO_Preta/Parda/Indígena`, qt_brancos=`QT_ALUNO_Branca/Amarela`, qt_outros=QT_ALUNO_Outros, qt_q1 = `QT_ALUNO_Quintil inferior`, qt_q5 = `QT_ALUNO_Quintil superior`, qt_qoutros = QT_ALUNO_NA) %>%
  mutate(
    peso_q1 = qt_q1/(qt_q1+qt_q5+qt_qoutros), 
    peso_q5 = qt_q1/(qt_q1+qt_q5+qt_qoutros),
    peso_outros = qt_qoutros/(qt_q1+qt_q5+qt_outros)
  ) %>% 
  select(CO_MUNICIPIO, ano, cluster, tx_adq_q1, tx_adq_q5, tx_adq_qoutros, qt_q1, qt_q5, qt_qoutros, peso_q1, peso_q5, peso_outros, diff_renda)

# Cálculo dos fatores de redistribuição por cluster
tb_5ano_sna_renda <- tb_5ano_sna %>% 
  filter(diff_renda > 0) %>% 
  pivot_wider(id_cols = c(CO_MUNICIPIO, cluster), names_prefix = "tx_", names_from = ano, values_from = diff_renda) %>% 
  mutate(referencia = ifelse((tx_2019 < tx_2017), 1, 0)) %>% 
  filter(referencia == 1) %>%
  select(CO_MUNICIPIO, cluster, tx_2017, tx_2019) %>% 
  group_by(cluster) %>%
  arrange(tx_2019-tx_2017) %>% 
  slice_min(order_by = tx_2019-tx_2017, prop = 0.5, with_ties = FALSE) %>% 
  reframe(tx_2017=median(tx_2017, na.rm = TRUE), tx_2019=median(tx_2019, na.rm = TRUE), qt_municípios=n()) %>% 
  mutate(diff=abs(tx_2019-tx_2017))
tb_5ano_sna_renda <- tb_5ano_sna_renda %>% left_join(qt_cluster_mun, by="cluster") 

# Fator de redistribuição para o Brasil
total_alunos <- tb_5ano %>% filter(ano==2019) %>% group_by(cluster) %>% reframe(qt_aluno_total = sum(`QT_ALUNO_Quintil inferior`, na.rm=T)+sum(`QT_ALUNO_Quintil superior`, na.rm=T)+sum(QT_ALUNO_NA, na.rm=T))
tb_5ano_sna_renda <- tb_5ano_sna_renda %>% left_join(total_alunos, by="cluster") 

fator_redistribuicao_brasil <- tb_5ano_sna_renda %>%
  summarise(
    diff_brasil =
      sum(diff * qt_aluno_total, na.rm = TRUE) /
      sum(qt_aluno_total, na.rm = TRUE)
  ) %>%
  pull(diff_brasil)
fator_redistribuicao_brasil

# Pesos de cada grupo de renda
saeb_19_dta <- merge(x=saeb_19_dta, y=q_mun_19_5, by="id_municipio")
qs <- quantile(
  saeb_19_dta[
    in_situacao_censo == 1 & 
      id_serie == 5 & 
      id_dependencia_adm %in% c(2, 3) & 
      !is.na(inse_aluno),
    inse_aluno
  ],
  probs = c(0.2, 0.8),
  na.rm = TRUE
)
pesos_renda <- saeb_19_dta[
  in_situacao_censo == 1 & 
    id_serie == 5 & 
    id_dependencia_adm %in% c(2, 3) & 
    !is.na(inse_aluno),
  .(
    grupo_renda = fifelse(
      inse_aluno < qs[1], "Quintil inferior",
      fifelse(
        inse_aluno < qs[2], "Outros",
        "Quintil superior"
      )
    )
  )
][
  ,
  .(QT_TOTAL = .N),
  by = grupo_renda
][
  ,
  .(grupo_renda, peso = QT_TOTAL / sum(QT_TOTAL))
]


# Dados fixos
w_q1 <- as.numeric(subset(pesos_renda, grupo_renda == "Quintil inferior")$peso)
w_q5  <- as.numeric(subset(pesos_renda, grupo_renda == "Quintil superior")$peso)
w_outros <- as.numeric(subset(pesos_renda, grupo_renda == "Outros")$peso)

ponto_partida_23 <- saeb_5_ef_23 %>% 
  filter(IN_PUBLICA==1 & IN_SITUACAO_CENSO==1) %>% 
  mutate(cor_raca = case_when(TX_RESP_Q04 %in% c("A", "D") ~ "Brancos", TX_RESP_Q04 %in% c("B", "C", "E") ~ "PPI", .default = "Outros"),
         APR_ADEQ = ifelse((PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200), 1 * PESO_ALUNO_MT, 0)) %>% 
  group_by(cor_raca) %>% 
  reframe(tx=sum(APR_ADEQ, na.rm=T) / sum(PESO_ALUNO_MT, na.rm=T))

quintis_inse <- with(
  saeb_5_ef_23 %>% filter(IN_INSE == 1),
  Hmisc::wtd.quantile(
    x = INSE_ALUNO,
    weights = PESO_ALUNO_INSE,
    probs = c(0.2, 0.8), # inferior e superior
    na.rm = TRUE
  )
)
ponto_partida_23 <- saeb_5_ef_23 %>%
  filter(
    IN_PUBLICA == 1,
    IN_SITUACAO_CENSO == 1,
    IN_INSE == 1
  ) %>%
  mutate(
    grupo_inse = case_when(
      INSE_ALUNO <= quintis_inse[1] ~ "Quintil inferior",
      INSE_ALUNO >= quintis_inse[2] ~ "Quintil superior",
      TRUE ~ "Demais quintis"
    ),
    APR_ADEQ = ifelse(
      (PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200),
      1,
      0
    )
  )
ponto_partida_23 <- ponto_partida_23 %>%
  group_by(grupo_inse) %>%
  summarise(
    tx = sum(APR_ADEQ * PESO_ALUNO_INSE, na.rm = TRUE) /
      sum(PESO_ALUNO_INSE, na.rm = TRUE),
    .groups = "drop"
  )

# Valores iniciais (2023)
P0 <- as.numeric(subset(ponto_partida_23, grupo_inse == "Quintil inferior")$tx)
B0 <- as.numeric(subset(ponto_partida_23, grupo_inse == "Quintil superior")$tx)
O0 <- as.numeric(subset(ponto_partida_23, grupo_inse == "Demais quintis")$tx)

# Preparar ganhos do Brasil
df_brasil_proj <- df_brasil_proj %>%
  arrange(ano) %>%
  mutate(
    ganho_total = tx_brasil - lag(tx_brasil)
  )

# Teto 100%
aplicar_teto_redistribuir <- function(vals, pesos, teto = 1) {
  
  pesos <- pesos[names(vals)]
  
  excedente <- sum(pmax(vals - teto, 0) * pesos, na.rm = TRUE)
  vals <- pmin(vals, teto)
  
  if (excedente > 0) {
    elegiveis <- which(vals < teto - 1e-12)
    
    if (length(elegiveis) > 0) {
      soma_pesos <- sum(pesos[elegiveis])
      
      for (i in elegiveis) {
        nm <- names(vals)[i]
        vals[nm] <- vals[nm] + excedente / soma_pesos
      }
    }
  }
  
  vals
}

# Projeção iterativa
proj_renda <- accumulate(
  .x = seq_len(nrow(df_brasil_proj)),
  .init = tibble(
    ano = df_brasil_proj$ano[1],
    tx_q1 = P0,
    tx_q5  = B0,
    tx_outros = O0
  ),
  .f = function(prev, i) {
    
    if (i == 1) return(prev)
    
    deltaT <- df_brasil_proj$ganho_total[i]
    T_next <- df_brasil_proj$tx_brasil[i]
    
    # --------------------------------------------------
    # REGIME 3 — Pós-igualdade (todos crescem iguais)
    # --------------------------------------------------
    if (abs(prev$tx_q1 - prev$tx_q5) < 1e-10) {
      
      delta_level <- deltaT / (w_q1 + w_q5 + w_outros)
      
      vals <- c(
        tx_q1 = prev$tx_q1 + delta_level,
        tx_q5  = prev$tx_q5  + delta_level,
        tx_outros = prev$tx_outros + delta_level
      )
      
      vals <- aplicar_teto_redistribuir(
        vals,
        pesos = c(tx_q1 = w_q1, tx_q5 = w_q5, tx_outros = w_outros)
      )
      
      return(tibble(
        ano = df_brasil_proj$ano[i],
        tx_q1 = as.numeric(vals["tx_q1"]),
        tx_q5  = as.numeric(vals["tx_q5"]),
        tx_outros = as.numeric(vals["tx_outros"])
      ))
    }
    
    # --------------------------------------------------
    # REGIME 1 — Desigual com bônus (Q1 favorecido)
    # Crescimento proporcional ao peso
    # --------------------------------------------------
    
    # crescimento base em nível
    delta_base <- deltaT / (w_q1 + w_q5 + w_outros)
    
    dP_eq <- delta_base
    dB_eq <- delta_base
    dO_eq <- delta_base
    
    # bônus crescente do PPI
    d_t <- fator_redistribuicao_brasil * deltaT
    
    dP_des <- dP_eq + d_t
    custo  <- w_q1 * d_t
    
    # financiamento proporcional ao peso
    peso_outros <- w_q5 + w_outros
    
    dB_des <- dB_eq - (custo * (w_q5 / peso_outros)) / w_q5
    dO_des <- dO_eq - (custo * (w_outros / peso_outros)) / w_outros
    
    # valores candidatos
    vals <- c(
      tx_q1 = prev$tx_q1 + dP_des,
      tx_q5  = prev$tx_q5  + dB_des,
      tx_outros = prev$tx_outros + dO_des
    )
    
    vals <- aplicar_teto_redistribuir(
      vals,
      pesos = c(tx_q1 = w_q1, tx_q5 = w_q5, tx_outros = w_outros)
    )
    
    P_cand <- as.numeric(vals["tx_q1"])
    B_cand <- as.numeric(vals["tx_q5"])
    O_cand <- as.numeric(vals["tx_outros"])
    
    # --------------------------------------------------
    # REGIME 2 — Encostar sem ultrapassar (PPI = BA)
    # --------------------------------------------------
    if (!is.na(P_cand) && !is.na(B_cand) && P_cand >= B_cand) {
      
      x <- (T_next - w_outros * O_cand) / (w_q1 + w_q5)
      
      vals <- c(
        tx_q1 = x,
        tx_q5  = x,
        tx_outros = O_cand
      )
      
      vals <- aplicar_teto_redistribuir(
        vals,
        pesos = c(tx_q1 = w_ppi, tx_q5 = w_ba, tx_outros = w_out)
      )
      
      return(tibble(
        ano = df_brasil_proj$ano[i],
        tx_q1 = as.numeric(vals["tx_q1"]),
        tx_q5  = as.numeric(vals["tx_q5"]),
        tx_outros = as.numeric(vals["tx_outros"])
      ))
    }
    
    # --------------------------------------------------
    # Continua no regime desigual
    # --------------------------------------------------
    tibble(
      ano = df_brasil_proj$ano[i],
      tx_q1 = P_cand,
      tx_q5  = B_cand,
      tx_outros = O_cand
    )
  }
) %>%
  bind_rows() %>%
  slice(-1)

# Brasil fecha
proj_renda %>%
  left_join(df_brasil_proj, by = "ano") %>%
  mutate(
    brasil_calc = w_q1*tx_q1 + w_q5*tx_q5 + w_outros*tx_outros,
    erro = brasil_calc - tx_brasil
  )

proj_renda

# Inclusão do histórico

df_historico <- saeb_renda_5 %>% 
  filter(ano != 2023) %>% 
  pivot_wider(id_cols=ano, names_from=grupo_renda, values_from = p) %>% 
  rename(tx_q5 = `Quintil superior`, tx_q1 = `Quintil inferior`, tx_outros = Outros)

df_renda_proj <- rbind(proj_renda, df_historico)
df_renda_proj <- df_renda_proj %>% select(ano, tx_q1, tx_q5, tx_outros) %>% arrange(ano)

proj_brasil_grupos <- df_renda_proj %>% 
  pivot_longer(cols=c(tx_q1, tx_q5, tx_outros), names_to = "grupo") %>% 
  mutate(periodo = ifelse(ano < 2025, "Valor observado", "Projeção"), value=value*100) %>% 
  rename(tx_brasil=value) 

#=========================
#= Projeção dos cenários =
#=========================

# Histórico raça/cor

saeb_13_racacor_5 <- saeb_13_dta %>% 
  filter(in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3)) %>% 
  mutate(APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),cor_raca = case_when(cor_raca %in% c("Branca", "Amarela") ~ "Brancos", cor_raca %in% c("Preta", "Parda", "Indígena") ~ "PPI", .default = "Outros")) %>% 
  group_by(cor_raca) %>% 
  reframe(p = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)) %>% 
  mutate(ano=2013)

# Brancos = 0.351 | PPI = 0.265 | Outros = 0.154| Diferença = 0.086 (8,6 p.p.)

saeb_15_racacor_5 <- saeb_15_dta %>% 
  filter(in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3)) %>% 
  mutate(APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0), cor_raca = case_when(cor_raca %in% c("Branca", "Amarela") ~ "Brancos", cor_raca %in% c("Preta", "Parda", "Indígena") ~ "PPI", .default = "Outros")) %>% 
  group_by(cor_raca) %>% 
  reframe(p = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)) %>% 
  mutate(ano=2015)

# Brancos = 0.392 | PPI = 0.313 | | Outros = 0.214 | Diferença = 0.079 (7,9 p.p.)

saeb_17_racacor_5 <- saeb_17_dta %>% 
  filter(in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3)) %>% 
  mutate(APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0), cor_raca = case_when(cor_raca %in% c("Branca", "Amarela") ~ "Brancos", cor_raca %in% c("Preta", "Parda", "Indígena") ~ "PPI", .default = "Outros")) %>% 
  group_by(cor_raca) %>% 
  reframe(p=sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)) %>% 
  mutate(ano=2017)

# Brancos = 0.459 | PPI = 0.364 | Outros = 0.283 | Diferença = 0.095 (9,5 p.p.)

saeb_19_racacor_5 <- saeb_19_dta %>% 
  filter(in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3)) %>% 
  mutate(APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0), cor_raca = case_when(cor_raca %in% c("Branca", "Amarela") ~ "Brancos", cor_raca %in% c("Preta", "Parda", "Indígena") ~ "PPI", .default = "Outros")) %>% 
  group_by(cor_raca) %>% 
  reframe(p = sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)) %>% 
  mutate(ano=2019)

# Brancos = 0.475 | PPI = 0.378 | Outros = 0.307 | Diferença = 0.097 (9,7 p.p.)

saeb_21_racacor_5 <- saeb_5_ef_21 %>% 
  filter(IN_PUBLICA==1 & IN_SITUACAO_CENSO==1) %>% 
  mutate(cor_raca = case_when(TX_RESP_Q04 %in% c("A", "D") ~ "Brancos", TX_RESP_Q04 %in% c("B", "C", "E") ~ "PPI", .default = "Outros"), APR_ADEQ = ifelse((PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200), 1 * PESO_ALUNO_MT, 0)) %>% 
  group_by(cor_raca) %>% 
  reframe(p=sum(APR_ADEQ, na.rm=T) / sum(PESO_ALUNO_MT, na.rm=T)) %>% 
  mutate(ano=2021)

# Brancos = 0.377 | PPI = 0.290 | Outros = 0.243 | Diferença = 0.087 (8,7 p.p.)

saeb_23_racacor_5 <- saeb_5_ef_23 %>% 
  filter(IN_PUBLICA==1 & IN_SITUACAO_CENSO==1) %>% 
  mutate(cor_raca = case_when(TX_RESP_Q04 %in% c("A", "D") ~ "Brancos", TX_RESP_Q04 %in% c("B", "C", "E") ~ "PPI", .default = "Outros"), APR_ADEQ = ifelse((PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200), 1 * PESO_ALUNO_MT, 0)) %>% 
  group_by(cor_raca) %>% 
  reframe(p=sum(APR_ADEQ, na.rm=T) / sum(PESO_ALUNO_MT, na.rm=T)) %>% 
  mutate(ano=2023)  

# Brancos = 0.448 | PPI = 0.349 | Outros = 0.282 | Diferença = 0.099 (9,9 p.p.)

saeb_racacor_5 <- rbind(saeb_13_racacor_5, saeb_15_racacor_5, saeb_17_racacor_5, saeb_19_racacor_5, saeb_21_racacor_5, saeb_23_racacor_5)


# Histórico renda

# 2013
qs_13_5 <- quantile(
  saeb_13_dta[
    in_situacao_censo == 1 & 
      id_serie == 5 & 
      id_dependencia_adm %in% c(2, 3) & 
      !is.na(inse_aluno),
    inse_aluno
  ],
  probs = c(0.2, 0.8),
  na.rm = TRUE
)
saeb_13_renda_5 <- saeb_13_dta[
  in_situacao_censo == 1 & 
    id_serie == 5 & 
    id_dependencia_adm %in% c(2, 3) & 
    !is.na(inse_aluno),
  .(
    peso_aluno_mt,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    grupo_renda = fifelse(
      inse_aluno < qs_13_5[1], "Quintil inferior",
      fifelse(
        inse_aluno < qs_13_5[2], "Outros",
        "Quintil superior"
      )
    )
  )
][
  ,
  .(p=sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by = grupo_renda
][
  ,
  .(grupo_renda, p, ano=2013)
]

# 2015
qs_15_5 <- quantile(
  saeb_15_dta[
    in_situacao_censo == 1 & 
      id_serie == 5 & 
      id_dependencia_adm %in% c(2, 3) & 
      !is.na(inse_aluno),
    inse_aluno
  ],
  probs = c(0.2, 0.8),
  na.rm = TRUE
)
saeb_15_renda_5 <- saeb_15_dta[
  in_situacao_censo == 1 & 
    id_serie == 5 & 
    id_dependencia_adm %in% c(2, 3) & 
    !is.na(inse_aluno),
  .(
    peso_aluno_mt,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    grupo_renda = fifelse(
      inse_aluno < qs_15_5[1], "Quintil inferior",
      fifelse(
        inse_aluno < qs_15_5[2], "Outros",
        "Quintil superior"
      )
    )
  )
][
  ,
  .(p=sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by = grupo_renda
][
  ,
  .(grupo_renda, p, ano=2015)
]

# 2017
qs_17_5 <- quantile(
  saeb_17_dta[
    in_situacao_censo == 1 & 
      id_serie == 5 & 
      id_dependencia_adm %in% c(2, 3) & 
      !is.na(inse_aluno),
    inse_aluno
  ],
  probs = c(0.2, 0.8),
  na.rm = TRUE
)
saeb_17_renda_5 <- saeb_17_dta[
  in_situacao_censo == 1 & 
    id_serie == 5 & 
    id_dependencia_adm %in% c(2, 3) & 
    !is.na(inse_aluno),
  .(
    peso_aluno_mt,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    grupo_renda = fifelse(
      inse_aluno < qs_17_5[1], "Quintil inferior",
      fifelse(
        inse_aluno < qs_17_5[2], "Outros",
        "Quintil superior"
      )
    )
  )
][
  ,
  .(p=sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by = grupo_renda
][
  ,
  .(grupo_renda, p, ano=2017)
]

# 2019
qs_19_5 <- quantile(
  saeb_19_dta[
    in_situacao_censo == 1 & 
      id_serie == 5 & 
      id_dependencia_adm %in% c(2, 3) & 
      !is.na(inse_aluno),
    inse_aluno
  ],
  probs = c(0.2, 0.8),
  na.rm = TRUE
)
saeb_19_renda_5 <- saeb_19_dta[
  in_situacao_censo == 1 & 
    id_serie == 5 & 
    id_dependencia_adm %in% c(2, 3) & 
    !is.na(inse_aluno),
  .(
    peso_aluno_mt,
    APR_ADEQ = ifelse((proficiencia_mt_saeb >= 225) & (proficiencia_lp_saeb >= 200), 1 * peso_aluno_mt, 0),
    grupo_renda = fifelse(
      inse_aluno < qs_19_5[1], "Quintil inferior",
      fifelse(
        inse_aluno < qs_19_5[2], "Outros",
        "Quintil superior"
      )
    )
  )
][
  ,
  .(p=sum(APR_ADEQ, na.rm=T) / sum(peso_aluno_mt, na.rm=T)),
  by = grupo_renda
][
  ,
  .(grupo_renda, p, ano=2019)
]

# 2021
qs_21_5 <- with(
  saeb_5_ef_21 %>% filter(IN_INSE == 1),
  Hmisc::wtd.quantile(
    x = INSE_ALUNO,
    weights = PESO_ALUNO_INSE,
    probs = c(0.2, 0.8), # inferior e superior
    na.rm = TRUE
  )
)
saeb_21_renda_5 <- saeb_5_ef_21 %>%
  filter(
    IN_PUBLICA == 1,
    IN_SITUACAO_CENSO == 1,
    IN_INSE == 1
  ) %>%
  mutate(
    grupo_renda = case_when(
      INSE_ALUNO <= quintis_inse[1] ~ "Quintil inferior",
      INSE_ALUNO >= quintis_inse[2] ~ "Quintil superior",
      TRUE ~ "Outros"
    ),
    APR_ADEQ = ifelse(
      (PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200),
      1,
      0
    )
  )
saeb_21_renda_5 <- saeb_21_renda_5 %>%
  group_by(grupo_renda) %>%
  summarise(
    p = sum(APR_ADEQ * PESO_ALUNO_INSE, na.rm = TRUE) /
      sum(PESO_ALUNO_INSE, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(ano=2021)

# 2023
qs_23_5 <- with(
  saeb_5_ef_23 %>% filter(IN_INSE == 1),
  Hmisc::wtd.quantile(
    x = INSE_ALUNO,
    weights = PESO_ALUNO_INSE,
    probs = c(0.2, 0.8), # inferior e superior
    na.rm = TRUE
  )
)
saeb_23_renda_5 <- saeb_5_ef_23 %>%
  filter(
    IN_PUBLICA == 1,
    IN_SITUACAO_CENSO == 1,
    IN_INSE == 1
  ) %>%
  mutate(
    grupo_renda = case_when(
      INSE_ALUNO <= quintis_inse[1] ~ "Quintil inferior",
      INSE_ALUNO >= quintis_inse[2] ~ "Quintil superior",
      TRUE ~ "Outros"
    ),
    APR_ADEQ = ifelse(
      (PROFICIENCIA_MT_SAEB >= 225) & (PROFICIENCIA_LP_SAEB >= 200),
      1,
      0
    )
  )
saeb_23_renda_5 <- saeb_23_renda_5 %>%
  group_by(grupo_renda) %>%
  summarise(
    p = sum(APR_ADEQ * PESO_ALUNO_INSE, na.rm = TRUE) /
      sum(PESO_ALUNO_INSE, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(ano=2023)

saeb_renda_5 <- rbind(saeb_13_renda_5, saeb_15_renda_5, saeb_17_renda_5, saeb_19_renda_5, saeb_21_renda_5, saeb_23_renda_5)
