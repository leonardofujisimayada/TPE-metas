#==========================
#= Configurações iniciais = 
#==========================

# Importação de bibliotecas
library(readxl)             # importa e trata arquivos .xlsx
library(tidyverse)
library(tibble)
library(scales)
library(geobr)
library(sf)
library(Hmisc)

# Importação de arquivos

# Dados da projeção
df_projecao_tpe <- readxl::read_xlsx("C:/Users/leona/Downloads/Cópia de Projeção _ Acesso à Creche.xlsx", sheet="BD Projeção")
df_projecao_tpe_demografico <- readxl::read_xlsx("C:/Users/leona/Downloads/Cópia de Projeção _ Acesso à Creche.xlsx", sheet="BD Demográfico")
df_projecao_tpe_demografico <- df_projecao_tpe_demografico %>% 
  select(CodIBGE, `Mat. 0-3 2024`, `Pop. 0-3 2024`) %>% rename(CO_MUNICIPIO=CodIBGE, QT_MAT_03_24=`Mat. 0-3 2024`, POP_03_24 = `Pop. 0-3 2024`)

# Inclusão dos dados de UF na base TPE
mun <- read_municipality(code_muni = "all", year = 2024, simplified = TRUE) 
mun <- mun %>% st_drop_geometry() %>% select(code_muni, name_state) %>% rename(CO_MUNICIPIO=code_muni, SG_UF=name_state)

# Dados da projeção do TPE
df_projecao_tpe <- df_projecao_tpe %>% 
  select(...1, `2024_Tx. Atendimento`, `2025_Tx. Atendimento`, `2026_Tx. Atendimento`, `2027_Tx. Atendimento`, `2028_Tx. Atendimento`, `2029_Tx. Atendimento`, `2030_Tx. Atendimento`, `2031_Tx. Atendimento`, `2032_Tx. Atendimento`, `2033_Tx. Atendimento`, `2034_Tx. Atendimento`, `2035_Tx. Atendimento`) %>% 
  rename(CO_MUNICIPIO=...1, tx_2024 = `2024_Tx. Atendimento`, tx_2025=`2025_Tx. Atendimento`, tx_2026=`2026_Tx. Atendimento`, tx_2027=`2027_Tx. Atendimento`, tx_2028=`2028_Tx. Atendimento`, tx_2029=`2029_Tx. Atendimento`, tx_2030=`2030_Tx. Atendimento`, tx_2031=`2031_Tx. Atendimento`, tx_2032=`2032_Tx. Atendimento`, tx_2033=`2033_Tx. Atendimento`, tx_2034=`2034_Tx. Atendimento`, tx_2035=`2035_Tx. Atendimento`) %>% 
  filter(!(is.na(CO_MUNICIPIO)) & CO_MUNICIPIO != "Código do Município") %>% 
  mutate(across(matches("^tx_20"),as.numeric), CO_MUNICIPIO = as.integer(CO_MUNICIPIO)) %>% 
  left_join(mun, by="CO_MUNICIPIO") %>% 
  left_join(df_projecao_tpe_demografico, by="CO_MUNICIPIO")

#===================================
#= Criação dos pesos por município = 
#===================================

base_long <- df_projecao_tpe %>%
  pivot_longer(
    cols = starts_with("tx_"),
    names_to = "ano",
    values_to = "tx"
  ) %>%
  mutate(
    ano = as.integer(sub("tx_", "", ano))
  )

proj_uf_total <- base_long %>%
  group_by(SG_UF, ano) %>%
  summarise(
    tx_uf = weighted.mean(tx, w = POP_03_24, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SG_UF, ano) %>% 
  mutate(tx_uf = tx_uf * 100)

proj_uf_total <- proj_uf_total %>%
  mutate(
    ano = paste0("tx_", ano)
  ) %>%
  pivot_wider(
    id_cols = SG_UF,
    names_from = ano,
    values_from = tx_uf
  ) %>%
  arrange(SG_UF) %>% 
  rename(UF=SG_UF)

rm(df_projecao_tpe, df_projecao_tpe_demografico, mun, base_long)

#==========================================================================
#= Dados históricos Pnad Contínua Educação e identificação de referências = 
#==========================================================================

# =========================================================
# PROJEÇÃO CRECHE 0–3 ANOS (Q1 cresce 1/3 do ganho * (1+bônus))
# bônus_q1 = diferença típica em pp (fração), usada como % do ganho
# =========================================================

# -----------------------------
# Dados
# -----------------------------
df_freq_03 <- readxl::read_xlsx(
  "C:/Users/leona/Downloads/frequencia_motivos_leo.xlsx",
  sheet = "freq_03_quintil"
) %>%
  mutate(
    cv_sim = (`SE - Sim` / Sim) * 100,
    cv_nao = (`SE - Não` / Não) * 100
  )

# -----------------------------
# Referência: bônus em pp (fração)
# (redução típica da desigualdade entre 2023->2024 nas UFs com queda)
# -----------------------------
df_freq_03_historico <- df_freq_03 %>% 
  mutate(
    quintil = case_when(
      quintil == "1º" ~ "Quintil inferior",
      quintil == "5º" ~ "Quintil superior",
      TRUE ~ "Demais quintis"
    )
  ) %>% 
  filter(Agregação == "UF") %>%
  group_by(Ano, UF, quintil) %>% 
  summarise(
    Frequenta = sum(Sim) / (sum(Sim) + sum(Não)),
    QT_TOTAL  = sum(Sim) + sum(Não),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    id_cols = c(Ano, UF),
    names_from = quintil,
    values_from = Frequenta
  ) %>% 
  mutate(DIFF = `Quintil superior` - `Quintil inferior`) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = Ano, values_from = DIFF) %>% 
  filter(`2024` < `2023`)

delta_pp_ref <- df_freq_03_historico %>%
  mutate(delta_pp = `2024` - `2023`) %>%
  arrange(delta_pp) %>% # mais negativo = melhor
  slice_head(n = 1) %>%
  summarise(delta_pp_ref = median(delta_pp, na.rm = TRUE)) %>%
  pull(delta_pp_ref)

delta_pp_ref_abs <- abs(delta_pp_ref)

# bônus em "percentual do ganho" (ex.: 0.001842963 = 0.1842963%)
bonus_q1 <- delta_pp_ref_abs

# -----------------------------
# Série por quintil (UF) + pesos + ponto de partida (2024)
# -----------------------------
df_freq_03_quintil <- df_freq_03 %>% 
  mutate(
    quintil = case_when(
      quintil == "1º" ~ "Quintil inferior",
      quintil == "5º" ~ "Quintil superior",
      TRUE ~ "Demais quintis"
    )
  ) %>% 
  filter(Agregação == "UF") %>%
  group_by(Ano, UF, quintil) %>% 
  summarise(
    Frequenta = sum(Sim) / (sum(Sim) + sum(Não)),
    QT_TOTAL  = sum(Sim) + sum(Não),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    id_cols = c(Ano, UF),
    names_from = quintil,
    values_from = c(Frequenta, QT_TOTAL)
  )

pesos_quintil_03 <- df_freq_03_quintil %>%
  filter(Ano == 2024) %>%
  summarise(
    qt_q1  = sum(`QT_TOTAL_Quintil inferior`, na.rm = TRUE),
    qt_q5  = sum(`QT_TOTAL_Quintil superior`, na.rm = TRUE),
    qt_out = sum(`QT_TOTAL_Demais quintis`, na.rm = TRUE)
  ) %>%
  mutate(
    total = qt_q1 + qt_q5 + qt_out,
    w_q1  = qt_q1 / total,
    w_q5  = qt_q5 / total,
    w_out = qt_out / total
  )

w_q1  <- pesos_quintil_03$w_q1
w_q5  <- pesos_quintil_03$w_q5
w_out <- pesos_quintil_03$w_out

ponto_partida_03 <- df_freq_03_quintil %>%
  filter(Ano == 2024) %>%
  summarise(
    Q1     = weighted.mean(`Frequenta_Quintil inferior`, `QT_TOTAL_Quintil inferior`, na.rm = TRUE),
    Q5     = weighted.mean(`Frequenta_Quintil superior`, `QT_TOTAL_Quintil superior`, na.rm = TRUE),
    Outros = weighted.mean(`Frequenta_Demais quintis`, `QT_TOTAL_Demais quintis`, na.rm = TRUE)
  )

Q1_0 <- ponto_partida_03$Q1
Q5_0 <- ponto_partida_03$Q5
O_0  <- ponto_partida_03$Outros

# -----------------------------
# Projeção Brasil (exógena)
# -----------------------------
df_brasil_03_proj <- tibble(
  Ano = 2025:2035,
  tx_brasil = c(0.41, 0.44, 0.47, 0.50, 0.53, 0.55, 0.57, 0.58, 0.59, 0.60, 0.60)
) %>%
  arrange(Ano) %>%
  mutate(ganho_total = tx_brasil - lag(tx_brasil))

# -----------------------------
# Teto 100% com redistribuição ponderada
# -----------------------------
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

# -----------------------------
# Projeção iterativa (bônus percentual do ganho)
# Regra:
#   delta_base = deltaT/3
#   dQ1 = delta_base * (1 + bonus_q1)
#   excesso = dQ1 - delta_base
#   Q5 e Outros financiam o excesso (proporcional ao bloco não-Q1), mas ainda crescem
# -----------------------------
proj_freq_03 <- accumulate(
  .x = seq_len(nrow(df_brasil_03_proj)),
  .init = tibble(
    Ano = df_brasil_03_proj$Ano[1],
    tx_q1 = Q1_0,
    tx_q5 = Q5_0,
    tx_outros = O_0
  ),
  .f = function(prev, i) {
    
    if (i == 1) return(prev)
    
    deltaT <- df_brasil_03_proj$ganho_total[i]
    T_next <- df_brasil_03_proj$tx_brasil[i]
    
    # Se deltaT for 0/NA, mantém
    if (is.na(deltaT) || abs(deltaT) < 1e-12) {
      return(tibble(
        Ano = df_brasil_03_proj$Ano[i],
        tx_q1 = prev$tx_q1,
        tx_q5 = prev$tx_q5,
        tx_outros = prev$tx_outros
      ))
    }
    
    # avanço igualitário (1/3 do ganho agregado)
    delta_base <- deltaT / 3
    
    # bônus em % do ganho (NÃO somar em nível!)
    dQ1 <- delta_base * (1 + bonus_q1)
    
    excesso <- dQ1 - delta_base
    ajuste_outros <- (w_q1 * excesso) / (w_q5 + w_out)
    
    dQ5 <- delta_base - ajuste_outros
    dO  <- delta_base - ajuste_outros
    
    vals <- c(
      tx_q1 = prev$tx_q1 + dQ1,
      tx_q5 = prev$tx_q5 + dQ5,
      tx_outros = prev$tx_outros + dO
    )
    
    # teto 100%
    vals <- aplicar_teto_redistribuir(
      vals,
      pesos = c(tx_q1 = w_q1, tx_q5 = w_q5, tx_outros = w_out),
      teto = 1
    )
    
    # fechamento exato no Brasil (corrige só erro numérico)
    brasil_calc <- w_q1*vals["tx_q1"] + w_q5*vals["tx_q5"] + w_out*vals["tx_outros"]
    erro <- as.numeric(brasil_calc - T_next)
    if (is.finite(erro) && abs(erro) > 1e-10) {
      corr <- erro / (w_q1 + w_q5 + w_out)
      vals["tx_q1"]     <- vals["tx_q1"]     - corr
      vals["tx_q5"]     <- vals["tx_q5"]     - corr
      vals["tx_outros"] <- vals["tx_outros"] - corr
      
      vals <- aplicar_teto_redistribuir(
        vals,
        pesos = c(tx_q1 = w_q1, tx_q5 = w_q5, tx_outros = w_out),
        teto = 1
      )
    }
    
    tibble(
      Ano = df_brasil_03_proj$Ano[i],
      tx_q1 = as.numeric(vals["tx_q1"]),
      tx_q5 = as.numeric(vals["tx_q5"]),
      tx_outros = as.numeric(vals["tx_outros"])
    )
  }
) %>%
  bind_rows() %>%
  slice(-1)

# Resultado
proj_freq_03

# (Opcional) checagem de fechamento
proj_freq_03 %>%
  left_join(df_brasil_03_proj, by = "Ano") %>%
  mutate(
    brasil_calc = w_q1*tx_q1 + w_q5*tx_q5 + w_out*tx_outros,
    erro = brasil_calc - tx_brasil
  )
