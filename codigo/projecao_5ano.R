#=========
#= Dados = 
#=========

options(scipen = 999)

dados5ano_13 <- readxl::read_xlsx("dados/20260118_resultado_5ano.xlsx", sheet = 1)
dados5ano_15 <- readxl::read_xlsx("dados/20260118_resultado_5ano.xlsx", sheet = 2)
dados5ano_17 <- readxl::read_xlsx("dados/20260118_resultado_5ano.xlsx", sheet = 3)
dados5ano_19 <- readxl::read_xlsx("dados/20260118_resultado_5ano.xlsx", sheet = 4)

dados5ano <- rbind(
  dados5ano_13, dados5ano_15, dados5ano_17, dados5ano_19
)

#========================
#= Referências Raça/Cor = 
#========================

# Preparação dos dados
dados5ano_racacor <- dados5ano %>% 
  mutate(DIFF_RACA=abs(DIFF_RACA)) %>% 
  select(-c(DIFF_RENDA, serie))
dados5ano_racacor_reshaped <- reshape2::dcast(
  data = dados5ano_racacor, 
  formula = CO_MUNICIPIO + cluster ~ ano, value.var = "DIFF_RACA"
)
dados5ano_racacor_reshaped_semna <- na.omit(dados5ano_racacor_reshaped)

# Verificação de municípios que reduziram a diferença em p.p. (raça/cor)
dados5ano_racacor_reshaped_semna_ref <- dados5ano_racacor_reshaped_semna %>% 
  filter(
    (`2015` < `2013`) & (`2017` < `2015`) & (`2019` < `2017`) 
  )
ref_racacor <- dados5ano_racacor_reshaped_semna_ref %>% 
  group_by(cluster) %>% 
  summarise(
    mediana_13 = median(`2013`), mediana_15 = median(`2015`), 
    mediana_17 = median(`2017`), mediana_19 = median(`2019`)
  )

#=====================
#= Referências Renda = 
#=====================

# Preparação dos dados
dados5ano_renda <- dados5ano %>% 
  mutate(DIFF_RENDA=abs(DIFF_RENDA)) %>% 
  select(-c(DIFF_RACA, serie))
dados5ano_renda_reshaped <- reshape2::dcast(
  data = dados5ano_renda, 
  formula = CO_MUNICIPIO + cluster ~ ano, value.var = "DIFF_RENDA"
)
dados5ano_renda_reshaped_semna <- na.omit(dados5ano_renda_reshaped)

# Verificação de municípios que reduziram a diferença em p.p. (raça/cor)
dados5ano_renda_reshaped_semna_ref <- dados5ano_renda_reshaped_semna %>% 
  filter(
    (`2015` < `2013`) & (`2017` < `2015`) & (`2019` < `2017`) 
  )
ref_renda <- dados5ano_renda_reshaped_semna_ref %>% 
  group_by(cluster) %>% 
  summarise(
    mediana_13 = median(`2013`), mediana_15 = median(`2015`), 
    mediana_17 = median(`2017`), mediana_19 = median(`2019`)
  )

#========================
#= Função para projeção = 
#========================

projecao_logistica_decrescente <- function(
    dados,
    anos_futuros,
    var_y,
    var_t        = "t_ajuste",
    var_cluster  = "cluster",
    L_min_global = 0.001,
    b_start      = 0.20,
    b_max        = 0.20
) {
  
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  dados %>%
    group_by(.data[[var_cluster]]) %>%
    nest() %>%
    mutate(
      
      # ====================
      # = AJUSTE DO MODELO =
      # ====================
      
      modelo = map(
        data,
        ~ {
          df <- .x %>%
            filter(
              !is.na(.data[[var_y]]),
              !is.na(.data[[var_t]])
            )
          
          # parâmetros auxiliares
          U_start  <- max(df[[var_y]])
          t0_start <- median(df[[var_t]])
          
          # tentativa logística decrescente
          mod_log <- tryCatch(
            nls(
              as.formula(
                paste0(
                  var_y,
                  " ~ L_min_global + (U - L_min_global) / (1 + exp(b * (",
                  var_t,
                  " - t0)))"
                )
              ),
              data = df,
              start = list(
                U  = U_start,
                b  = b_start,
                t0 = t0_start
              ),
              algorithm = "port",
              lower = c(
                U  = U_start * 0.8,
                b  = 0.01,
                t0 = min(df[[var_t]])
              ),
              upper = c(
                U  = 1,
                b  = b_max,
                t0 = max(df[[var_t]]) + 20
              ),
              control = nls.control(maxiter = 500, warnOnly = TRUE)
            ),
            error = function(e) NULL
          )
          
          # fallback: linear decrescente
          if (is.null(mod_log)) {
            m_lin <- lm(
              as.formula(paste0(var_y, " ~ ", var_t)),
              data = df
            )
            
            if (coef(m_lin)[[var_t]] >= 0) {
              m_lin <- lm(
                as.formula(paste0(var_y, " ~ 1")),
                data = df
              )
            }
            
            return(m_lin)
          }
          
          mod_log
        }
      ),
      
      # ==========
      # PROJEÇÃO =
      # ==========
      
      previsao = map(
        modelo,
        ~ {
          p <- predict(.x, newdata = anos_futuros)
          
          # limites naturais
          p <- pmax(p, L_min_global)
          p <- pmin(p, 1)
          
          tibble(
            ano = anos_futuros$ano,
            diff_proj = as.numeric(p)
          )
        }
      )
    ) %>%
    select(
      !!var_cluster,
      previsao
    ) %>%
    unnest(previsao)
}

#============
#= Projeção = 
#============

ano_base <- 2019
ano_inicio <- 2013

# Projeção raça/cor

# Preparação dos dados (long para wide)
ref_racacor_modelo <- reshape2::melt(data = ref_racacor, id.vars = "cluster", value.name = "diff_raca")
ref_racacor_modelo <- ref_racacor_modelo %>%
  mutate(
    ano = case_when(
      variable == "mediana_13" ~ 2013,
      variable == "mediana_15" ~ 2015,
      variable == "mediana_17" ~ 2017,
      variable == "mediana_19" ~ 2019,
      .default = NA
    )
  ) %>% 
  mutate(
    t_ajuste = ano - ano_inicio  
  ) %>% 
  select(-variable)
anos_futuros <- tibble(ano = c(2021, 2023, 2025, 2027, 2029, 2031, 2033, 2035)) %>%
  mutate(t_ajuste = ano - ano_inicio)

proj_racacor <- projecao_logistica_decrescente(
  dados = ref_racacor_modelo,
  anos_futuros = anos_futuros, 
  var_y = "diff_raca"
)

# Projeção renda

# Preparação dos dados (long para wide)
ref_renda_modelo <- reshape2::melt(data = ref_renda, id.vars = "cluster", value.name = "diff_renda")
ref_renda_modelo <- ref_renda_modelo %>%
  mutate(
    ano = case_when(
      variable == "mediana_13" ~ 2013,
      variable == "mediana_15" ~ 2015,
      variable == "mediana_17" ~ 2017,
      variable == "mediana_19" ~ 2019,
      .default = NA
    )
  ) %>% 
  mutate(
    t_ajuste = ano - ano_inicio  
  ) %>% 
  select(-variable)
anos_futuros <- tibble(ano = c(2021, 2023, 2025, 2027, 2029, 2031, 2033, 2035)) %>%
  mutate(t_ajuste = ano - ano_inicio)

proj_renda <- projecao_logistica_decrescente(
  dados = ref_renda_modelo,
  anos_futuros = anos_futuros,
  var_y = "diff_renda"
)

#================
#= Visualização = 
#================

# Clusters

# 0_50
# 50_100
# 100_500
# g46

cluster_selecionado <- "0_50"

ggplot(
  data = proj_racacor %>% filter(cluster==cluster_selecionado), aes(x=as.character(ano), y=diff_proj)
) +
  geom_point() +
  ylim(0, 0.1)

ggplot(
  data = proj_renda %>% filter(cluster==cluster_selecionado), aes(x=as.character(ano), y=diff_proj)
) +
  geom_point() +
  ylim(0, 0.1)

#====================================
#= Projeção dos dados por município =  
#====================================

# Dados conhecidos

# Pt = percentual projetado de estudantes com aprendizagem adequada (TPE)
# Delta t raça/cor = diferença em pontos percentuais entre dois grupos
# Delta t renda = diferença em pontos percentuais entre dois grupos

# N 1,t raça/cor = qt estudantes do grupo 1
# N 2,t raça/cor = qt estudantes do grupo 2
# N 0,t raça/cor = qt estudantes do grupo outros

# N 1,t renda = qt estudantes do grupo 1
# N 2,t renda = qt estudantes do grupo 2
# N 0,t renda = qt estudantes do grupo outros






