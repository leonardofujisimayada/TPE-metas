#==========================
#= Configurações iniciais = 
#==========================

# Importação de bibliotecas
library(readxl)             # importa e trata arquivos .xlsx
library(tidyverse)
library(tibble)
library(scales)

# Importação de arquivos

# Dados de creche por quintil - 0 a 3 anos
df_freq_03 <- readxl::read_xlsx("C:/Users/leona/Downloads/frequencia_motivos_leo.xlsx", sheet="freq_03_quintil")
df_freq_03 <- df_freq_03 %>% 
  mutate(
    cv_sim = ((`SE - Sim`) / Sim) * 100, cv_nao = ((`SE - Não`) / Não) * 100
  )


# Análise do histórico
df_freq_03_historico <- df_freq_03 %>% 
  filter(quintil %in% c("1º", "5º") & Agregação=="UF" & cv_sim <= 50) %>% 
  pivot_wider(id_cols = c(Ano, UF), names_from = "quintil", values_from = "Frequenta") %>% 
  mutate(DIFF = `5º`-`1º`) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = "Ano", values_from = "DIFF") %>% 
  mutate(referencia = case_when((`2024`<`2023`) ~ 1, .default = 0)) %>% 
  filter(referencia == 1) %>%
  select(-referencia)

df_freq_03_historico %>% 
  mutate(DIFF = `2024`-`2023`) %>%
  arrange(DIFF) %>% 
  head(1) %>% 
  summarise(median(`2023`), median(`2024`))

df_freq_03_quintil <- df_freq_03 %>% 
  filter(quintil %in% c("5º", "1º") & Agregação=="UF") %>% 
  mutate(Frequenta = Frequenta * 100) %>% 
  rename(grupo = quintil, media = Frequenta, total = Sim)

# Dados por raça/cor e quintil - 16 anos

z <- 1.96

df_conclusao_veloso_conc_16 <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_ef_16_")
df_conclusao_veloso_conc_16_ppi <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_ef_16_ppi")
df_conclusao_veloso_conc_16_ppi <- df_conclusao_veloso_conc_16_ppi %>% mutate(se = (media_upp - media_low) / (2 * z), cv = (se / media) * 100)

df_conclusao_veloso_conc_16_quintil <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_ef_16_quintil")
df_conclusao_veloso_conc_16_quintil <- df_conclusao_veloso_conc_16_quintil %>% 
  filter(quintil %in% c("Q5", "Q1")) %>% 
  rename(grupo = quintil)
df_conclusao_veloso_conc_16_quintil <- df_conclusao_veloso_conc_16_quintil %>% mutate(se = (media_upp - media_low) / (2 * z), cv = (se / media) * 100)

df_conclusao_veloso_conc_16_ppi_historico <- df_conclusao_veloso_conc_16_ppi %>% 
  filter(nivel=="UF" & cv <= 50) %>% 
  pivot_wider(id_cols = c(Ano, UF), names_from = "grupo", values_from = "media") %>% 
  mutate(DIFF = Brancos-PPI) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = "Ano", values_from = "DIFF") %>% 
  mutate(referencia = case_when((`2025`<`2024`) ~ 1, .default = 0)) %>% 
  filter(referencia == 1) %>%
  select(-referencia) 

df_conclusao_veloso_conc_16_ppi_historico %>%
  filter((`2024` > 0) & (`2025` > 0)) %>% 
  mutate(DIFF = `2025`-`2024`) %>%
  arrange(DIFF) %>% 
  head(5) %>% 
  summarise(median(`2024`), median(`2025`))

df_conclusao_veloso_conc_16_ppi_historico %>% 
  group_by(ano) %>% 
  reframe(median(value))

df_conclusao_veloso_conc_16_quintil_historico <- df_conclusao_veloso_conc_16_quintil %>% 
  filter(nivel=="UF" & cv <= 50) %>% 
  pivot_wider(id_cols = c(Ano, UF), names_from = "grupo", values_from = "media") %>% 
  mutate(DIFF = Q5-Q1) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = "Ano", values_from = "DIFF") %>% 
  mutate(referencia = case_when((`2025`<`2024`) ~ 1, .default = 0)) %>% 
  filter(referencia == 1) %>%
  select(-referencia) 

df_conclusao_veloso_conc_16_quintil_historico %>% 
  filter((`2024` > 0) & (`2025` > 0)) %>% 
  mutate(DIFF = `2025`-`2024`) %>%
  arrange(DIFF) %>% 
  head(5) %>% 
  summarise(median(`2024`), median(`2025`))

# Dados por raça/cor e quintil - 19 anos
z <- 1.96

df_conclusao_veloso_conc_19 <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_em_19_")

df_conclusao_veloso_conc_19_ppi <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_em_19_ppi")
df_conclusao_veloso_conc_19_ppi <- df_conclusao_veloso_conc_19_ppi %>% mutate(se = (media_upp - media_low) / (2 * z), cv = (se / media) * 100)

df_conclusao_veloso_conc_19_quintil <- readxl::read_xlsx("C:/Users/leona/Downloads/dados_conc_leo (1).xlsx", sheet="tx_conclusao_em_19_quintil")
df_conclusao_veloso_conc_19_quintil <- df_conclusao_veloso_conc_19_quintil %>% 
  filter(quintil %in% c("Q5", "Q1")) %>% 
  rename(grupo = quintil)
df_conclusao_veloso_conc_19_quintil <- df_conclusao_veloso_conc_19_quintil %>% mutate(se = (media_upp - media_low) / (2 * z), cv = (se / media) * 100)

df_conclusao_veloso_conc_19_ppi_historico <- df_conclusao_veloso_conc_19_ppi %>% 
  filter(nivel=="UF" & cv <= 50) %>% 
  pivot_wider(id_cols = c(Ano, UF), names_from = "grupo", values_from = "media") %>% 
  mutate(DIFF = Brancos-PPI) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = "Ano", values_from = "DIFF") %>% 
  mutate(referencia = case_when((`2025`<`2024`) ~ 1, .default = 0)) %>% 
  filter(referencia == 1) %>%
  select(-referencia) 

df_conclusao_veloso_conc_19_ppi_historico %>% 
  filter((`2024` > 0) & (`2025` > 0)) %>% 
  mutate(DIFF = `2025`-`2024`) %>%
  arrange(DIFF) %>% 
  head(5) %>% 
  summarise(median(`2024`), median(`2025`))

df_conclusao_veloso_conc_19_quintil_historico <- df_conclusao_veloso_conc_19_quintil %>% 
  filter(nivel=="UF" & cv <= 50) %>% 
  pivot_wider(id_cols = c(Ano, UF), names_from = "grupo", values_from = "media") %>% 
  mutate(DIFF = Q5-Q1) %>% 
  select(Ano, UF, DIFF) %>% 
  pivot_wider(id_cols = UF, names_from = "Ano", values_from = "DIFF") %>% 
  mutate(referencia = case_when((`2025`<`2024`) ~ 1, .default = 0)) %>% 
  filter(referencia == 1) %>%
  select(-referencia) 

df_conclusao_veloso_conc_19_quintil_historico %>% 
  filter((`2024` > 0) & (`2025` > 0)) %>% 
  mutate(DIFF = `2025`-`2024`) %>%
  arrange(DIFF) %>% 
  head(5) %>% 
  summarise(median(`2024`), median(`2025`))


# Dados da projeção
df_projecao_tpe <- readxl::read_xlsx("C:/Users/leona/Downloads/Cópia de Projeção _ Conclusão EF 16 Anos (1).xlsx", sheet="Projeção Conclusão")
df_projecao_tpe <- readxl::read_xlsx("C:/Users/leona/Downloads/Cópia de Projeção _ Conclusão EM 19 Anos.xlsx", sheet="Projeção Conclusão")

#========================
#= Preparação dos dados = 
#========================

# Só importa a base de 2024, que possui o último ano pré-projeção realizada
# pela equipe do TPE
base_grupos_2024 <- df_conclusao_veloso_conc_16_quintil %>%
  filter(Ano == 2024 & !(is.na(UF))) %>% select(UF, media, grupo, total) %>%
  rename(tx_2024=media,	n_2024=total) %>% group_by(UF) %>% reframe(UF, grupo, tx_2024, n_2024, peso_grupo = n_2024/sum(n_2024))

# Dados da projeção do TPE
proj_uf_total <- df_projecao_tpe %>% 
  select(...1, `2024.0...7`, `2025.0...9`, `2026...11`, `2027...13`, `2028...15`, `2029...17`, `2030...19`, `2031...21`, `2032...23`, `2033...25`, `2034...27`, `2035...29`) %>% 
  rename(UF=...1, tx_2024 = `2024.0...7`, tx_2025=`2025.0...9`, tx_2026=`2026...11`, tx_2027=`2027...13`, tx_2028=`2028...15`, tx_2029=`2029...17`, tx_2030=`2030...19`, tx_2031=`2031...21`, tx_2032=`2032...23`, tx_2033=`2033...25`, tx_2034=`2034...27`, tx_2035=`2035...29`) %>% 
  filter(UF != "UF") %>% 
  mutate(across(matches("^tx_20"),as.numeric))

# Tabela alvo por UF
alvo_uf <- proj_uf_total %>%
  pivot_longer(starts_with("tx_"), names_to = "ano", values_to = "tx_uf") %>%
  mutate(ano = as.integer(sub("tx_", "", ano))) %>%
  select(UF, ano, tx_uf)

# Regra de distribuição
regra_distribuicao_grupos <- base_grupos_2024 %>% 
  select(UF, grupo) %>% 
  mutate(share_crescimento = ifelse(grupo=="1º", 0.50+0.147, 1-(0.50+0.147)))
# Check de consistência
regra_distribuicao_grupos %>%
  group_by(UF) %>%
  summarise(soma = sum(share_crescimento), .groups = "drop") %>%
  summarise(min = min(soma), max = max(soma))

# Base para projeção
df_trabalho <- base_grupos_2024 %>%
  select(UF, grupo, tx_2024, peso_grupo) %>%
  left_join(regra_distribuicao_grupos, by = c("UF", "grupo"))
# Checks de consistência
colSums(is.na(df_trabalho))
df_trabalho %>%
  count(UF) %>%
  summarise(min = min(n), max = max(n))
df_trabalho %>%
  group_by(UF) %>%
  summarise(soma = sum(share_crescimento)) %>%
  summarise(min = min(soma), max = max(soma))

# Base de crescimento de UF em cada ano
delta_uf <- proj_uf_total %>%
  pivot_longer(
    cols = starts_with("tx_"),
    names_to = "ano",
    values_to = "tx"
  ) %>%
  mutate(
    ano = as.integer(sub("tx_", "", ano))
  ) %>%
  arrange(UF, ano) %>%
  group_by(UF) %>%
  mutate(
    delta_uf = tx - lag(tx)
  ) %>%
  ungroup() %>%
  filter(!is.na(delta_uf))   # remove 2024

# Checks de consistência
delta_uf %>%
  count(UF, ano) %>%
  summarise(max = max(n))
delta_uf %>%
  summarise(min_delta = min(delta_uf))
range(delta_uf$ano)

# Base de distribuição de crescimento por grupo
delta_grupo <- delta_uf %>%
  left_join(
    regra_distribuicao_grupos,
    by = "UF",
    relationship = "many-to-many"
  ) %>%
  mutate(
    delta_grupo = delta_uf * share_crescimento
  ) %>%
  select(UF, grupo, ano, delta_grupo)
delta_grupo %>%
  count(UF, ano) %>%
  summarise(min = min(n), max = max(n))
delta_grupo %>%
  group_by(UF, ano) %>%
  summarise(soma = sum(delta_grupo), .groups = "drop") %>%
  left_join(delta_uf, by = c("UF", "ano")) %>%
  summarise(max_erro = max(abs(soma - delta_uf)))

#===========================================
#= Projeção de Conclusão EF 16 anos por UF = 
#===========================================


# Ajuste 100%
aplicar_delta_com_teto <- function(tx_atual, delta) {
  
  tx_proposto <- tx_atual + delta
  
  excedente <- sum(pmax(tx_proposto - 100, 0))
  tx_novo <- pmin(tx_proposto, 100)
  
  if (excedente > 0) {
    idx <- which(tx_novo < 100)
    
    if (length(idx) > 0) {
      tx_novo[idx] <- pmin(tx_novo[idx] + excedente / length(idx), 100)
    }
  }
  
  tx_novo
}
# Fechamento por UF
fechar_para_alvo_uf <- function(tx, pesos, alvo, teto = 100, piso = 0, max_iter = 50, tol = 1e-8) {
  # tx, pesos: vetores (ex.: PPI e Brancos) da mesma UF
  # alvo: taxa da UF naquele ano (TPE)
  
  # garante limites
  tx <- pmin(pmax(tx, piso), teto)
  
  for (i in seq_len(max_iter)) {
    m_atual <- sum(tx * pesos)
    dif <- alvo - m_atual
    
    if (abs(dif) < tol) break
    
    if (dif > 0) {
      # precisamos AUMENTAR a média => distribuir por GAP até 100
      gap <- teto - tx
      if (sum(gap * pesos) < tol) break
      
      # aumento necessário em termos de média ponderada
      # distribuímos "incrementos de tx" ponderados pelos pesos e pelo gap
      w <- (gap * pesos)
      inc <- dif * (w / sum(w)) / pesos   # converte de "média ponderada" para "pontos de tx"
      tx <- pmin(tx + inc, teto)
      
    } else {
      # precisamos REDUZIR a média => distribuir por FOLGA até 0
      folga <- tx - piso
      if (sum(folga * pesos) < tol) break
      
      w <- (folga * pesos)
      dec <- dif * (w / sum(w)) / pesos   # dif é negativo
      tx <- pmax(tx + dec, piso)
    }
  }
  
  tx
}

tx_grupos <- df_trabalho %>%
  select(UF, grupo, tx_2024, peso_grupo) %>%
  mutate(ano = 2024) %>%
  rename(tx = tx_2024)

anos <- 2025:2035
for (ano_atual in anos) {
  
  delta_ano <- delta_grupo %>% filter(ano == ano_atual)
  
  tx_ano_anterior <- tx_grupos %>% filter(ano == ano_atual - 1)
  
  # 1) projeção preliminar (igual ao seu)
  tx_pre <- tx_ano_anterior %>%
    left_join(delta_ano, by = c("UF", "grupo")) %>%
    mutate(tx_pre = tx + delta_grupo) %>%
    group_by(UF) %>%
    mutate(tx_pre = aplicar_delta_com_teto(tx, delta_grupo)) %>%
    ungroup()
  
  # 2) FECHAMENTO: ajusta para bater com o alvo da UF no ano
  alvo_ano <- alvo_uf %>% filter(ano == ano_atual)
  
  tx_novo <- tx_pre %>%
    left_join(alvo_ano, by = "UF") %>%
    group_by(UF) %>%
    reframe(
      grupo = grupo,
      peso_grupo = peso_grupo,
      tx = fechar_para_alvo_uf(
        tx = tx_pre,
        pesos = peso_grupo,
        alvo = unique(tx_uf)
      )
    ) %>%
    mutate(ano = ano_atual)
  
  tx_grupos <- bind_rows(tx_grupos, tx_novo)
}

check_uf <- tx_grupos %>%
  left_join(alvo_uf, by = c("UF", "ano")) %>%
  group_by(UF, ano) %>%
  summarise(
    tx_uf_calc = sum(tx * peso_grupo),
    tx_uf_alvo = unique(tx_uf),
    erro = tx_uf_calc - tx_uf_alvo,
    .groups = "drop"
  )

summary(check_uf$erro)
max(abs(check_uf$erro))

tx_grupos %>% filter(UF == "SP", ano == 2035)


#===========================================
#= Projeção de Conclusão EF 16 anos Brasil = 
#===========================================

tx_grupos_com_peso <- tx_grupos %>%
  left_join(
    base_grupos_2024 %>% select(UF, grupo, n_2024),
    by = c("UF", "grupo")
  )

proj_brasil_grupos <- tx_grupos_com_peso %>%
  group_by(grupo, ano) %>%
  summarise(
    tx_brasil = weighted.mean(tx, w = n_2024),
    .groups = "drop"
  ) %>%
  arrange(grupo, ano)

#================
#= Visualização =
#================

library(ggrepel)

historico_brasil_grupos <- df_freq_03 %>% 
  filter(quintil %in% c("1º", "5º") & UF == "Brasil" & Ano < 2024) %>% 
  mutate(tx_brasil = Frequenta * 100) %>% 
  rename(ano = Ano, grupo=quintil) %>% 
  select(grupo, ano, tx_brasil)
proj_brasil_grupos <- rbind(proj_brasil_grupos, historico_brasil_grupos)
proj_brasil_grupos <- proj_brasil_grupos %>%
  mutate(periodo = ifelse(ano <= 2024, "Valor observado", "Projeção")) %>% 
  group_by(grupo) %>%
  complete(
    ano = 2020:2029
  ) %>%
  ungroup()


historico_df_conclusao_veloso_conc_16_ppi <- df_conclusao_veloso_conc_16_ppi %>% 
  filter(nivel == "BR" & Ano < 2024) %>% 
  rename(tx_brasil = media, ano = Ano) %>% 
  select(grupo, ano, tx_brasil)
proj_brasil_grupos <- rbind(proj_brasil_grupos, historico_df_conclusao_veloso_conc_16_ppi)
proj_brasil_grupos <- proj_brasil_grupos %>%
  mutate(periodo = ifelse(ano <= 2024, "Valor observado", "Projeção"))

historico_df_conclusao_veloso_conc_19_ppi <- df_conclusao_veloso_conc_19_ppi %>% 
  filter(nivel == "BR" & Ano < 2024) %>% 
  rename(tx_brasil = media, ano = Ano) %>% 
  select(grupo, ano, tx_brasil)
proj_brasil_grupos <- rbind(proj_brasil_grupos, historico_df_conclusao_veloso_conc_19_ppi)
proj_brasil_grupos <- proj_brasil_grupos %>%
  mutate(periodo = ifelse(ano <= 2024, "Valor observado", "Projeção"))

historico_df_conclusao_veloso_conc_16_quintil <- df_conclusao_veloso_conc_16_quintil %>% 
  filter(nivel == "BR" & Ano < 2024) %>% 
  rename(tx_brasil = media, ano = Ano) %>% 
  select(grupo, ano, tx_brasil)
proj_brasil_grupos <- rbind(proj_brasil_grupos, historico_df_conclusao_veloso_conc_16_quintil)
proj_brasil_grupos <- proj_brasil_grupos %>%
  mutate(periodo = ifelse(ano <= 2024, "Valor observado", "Projeção"))

historico_df_conclusao_veloso_conc_19_quintil <- df_conclusao_veloso_conc_19_quintil %>% 
  filter(nivel == "BR" & Ano < 2024) %>% 
  rename(tx_brasil = media, ano = Ano) %>% 
  select(grupo, ano, tx_brasil)
proj_brasil_grupos <- rbind(proj_brasil_grupos, historico_df_conclusao_veloso_conc_19_quintil)
proj_brasil_grupos <- proj_brasil_grupos %>%
  mutate(periodo = ifelse(ano <= 2024, "Valor observado", "Projeção"))


proj_brasil_grupos <- df_renda_proj %>% 
  pivot_longer(cols=c(tx_ppi, tx_ba, tx_outros), names_to = "grupo") %>% 
  mutate(periodo = ifelse(ano < 2025, "Valor observado", "Projeção"), value=value*100) %>% 
  rename(tx_brasil=value) 

proj_brasil_grupos_plot <- proj_brasil_grupos %>%
  mutate(
    tx_brasil = ifelse(periodo == "Ano sem dado", NA, tx_brasil)
  )


labels_extremos <- proj_brasil_grupos %>%
  filter(grupo != "tx_outros") %>% 
  group_by(grupo) %>%
  filter((ano %in% c(min(ano), max(ano))) | ano==2025) %>%
  ungroup() %>%
  mutate(
    label = label_percent(
      scale = 1,
      accuracy = 0.1,
      decimal.mark = ","
    )(tx_brasil)
  )

ggplot(
  proj_brasil_grupos,
  aes(
    x = as.factor(ano),
    y = tx_brasil,
    color = grupo,
    linetype = periodo,
    group = interaction(grupo, periodo)
  )
) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  
  geom_text_repel(
    data = labels_extremos,
    aes(label = label),
    size = 9,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.4,
    segment.color = NA,
    direction = "y",
    bg.color = "white",
    bg.r = 0.2,
    show.legend = FALSE
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::label_percent(scale = 1)
  ) +
  
  scale_color_manual(
    values = c(
      "tx_q5" = "#10266A",
      "tx_q1" = "#FA6041",
      "tx_outros" = "grey"
      #"tx_ppi" = "#10266A",
      #"tx_ba" = "#FA6041",
      #"tx_outros" = "grey"
    ),
    labels = c(
      "tx_q5" = "% aprendizagem adequada entre quintil superior",
      "tx_q1" = "% aprendizagem adequada entre quintil inferior",
      "tx_outros" = "% aprendizagem adequada entre demais quintis"
      #"tx_ppi" = "% aprendizagem adequada entre PPI",
      #"tx_ba" = "% aprendizagem adequada entre brancos/amarelos",
      #"tx_outros" = "% aprendizagem adequada entre não declarados"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Valor observado" = "solid",
      "Projeção" = "dashed"
    )
  ) +
  
  labs(
    subtitle = "Linha contínua: valores observados | Linha tracejada: projeção"
  ) +
  
  theme_minimal(base_size = 25) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 28),
    
    plot.subtitle = element_text(size = 28),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 28),
    
    panel.grid.major = element_line(
      color = "grey80",
      linetype = "dotted"
    ),
    panel.grid.minor = element_blank()
  ) +
  
  guides(
    color = guide_legend(nrow = 1, order = 1),
    linetype = "none"
  )


ggsave(
  "graficos/20260125_aprendizagem_3_renda.png",
  width = 30,
  height = 10,
  dpi = 300
)
