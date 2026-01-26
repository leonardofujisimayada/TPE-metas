#====================
#= QUINTIS - 5º ano =
#====================

# Cálculo dos quintis inferior e superior
q_mun_13_5 <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_15_5 <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_17_5 <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_19_5 <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 5 & id_dependencia_adm %in% c(1, 2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

#====================
#= QUINTIS - 9º ano =
#====================

# Cálculo dos quintis inferior e superior
q_mun_13_9 <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_15_9 <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_17_9 <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_19_9 <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 9 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

#======================
#= QUINTIS - 3ª série =
#======================

# Cálculo dos quintis inferior e superior
q_mun_13_12 <- saeb_13_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_15_12 <- saeb_15_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_17_12 <- saeb_17_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]

# Cálculo dos quintis inferior e superior
q_mun_19_12 <- saeb_19_dta[
  in_situacao_censo == 1 & id_serie == 12 & id_dependencia_adm %in% c(2, 3),
  .(
    q20 = quantile(inse_aluno, probs = 0.2, na.rm = TRUE),
    q80 = quantile(inse_aluno, probs = 0.8, na.rm = TRUE)
  ),
  by = id_municipio
][
  !(is.na(q20)) & !(is.na(q80))
]
