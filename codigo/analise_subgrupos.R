#=============================
#= Dados por subgrupo - 2013 =
#=============================

# Join 
saeb_13_dta <- saeb_13_dta %>% 
  filter(
    in_situacao_censo==1 &
      id_dependencia_adm==3
  ) %>% 
  left_join(
    tb_subgrupos, by=c("id_municipio"="CO_MUNICIPIO")
  )

# 50k - 0
saeb_13_dta_0_50 <- saeb_13_dta %>% 
  filter(PORTE == "50k - 0")
# 50k - 100k
saeb_13_dta_50_100 <- saeb_13_dta %>% 
  filter(PORTE == "100k - 50k")
# 100k - 500k
saeb_13_dta_100_500 <- saeb_13_dta %>% 
  filter(PORTE == "500k - 100k")
# G46
saeb_13_dta_G46 <- saeb_13_dta %>% 
  filter(PORTE == "G46 (Todos Pela Educação)")

# Exportação de dados
data.table::fwrite(
  x = saeb_13_dta_G46, 
  file = here("dados", "saeb_2013_G46.csv")
)

#=============================
#= Dados por subgrupo - 2015 =
#=============================

# Join 
saeb_15_dta <- saeb_15_dta %>% 
  filter(
    in_situacao_censo==1 &
      id_dependencia_adm==3
  ) %>% 
  left_join(
    tb_subgrupos, by=c("id_municipio"="CO_MUNICIPIO")
  )

# 50k - 0
saeb_15_dta_0_50 <- saeb_15_dta %>% 
  filter(PORTE == "50k - 0")
# 50k - 100k
saeb_15_dta_50_100 <- saeb_15_dta %>% 
  filter(PORTE == "100k - 50k")
# 100k - 500k
saeb_15_dta_100_500 <- saeb_15_dta %>% 
  filter(PORTE == "500k - 100k")
# G46
saeb_15_dta_G46 <- saeb_15_dta %>% 
  filter(PORTE == "G46 (Todos Pela Educação)")

# Exportação de dados
data.table::fwrite(
  x = saeb_15_dta_0_50, 
  file = here("dados", "saeb_2015_0_50.csv")
)

data.table::fwrite(
  x = saeb_15_dta_50_100, 
  file = here("dados", "saeb_2015_50_100.csv")
)

data.table::fwrite(
  x = saeb_15_dta_100_500, 
  file = here("dados", "saeb_2015_100_500.csv")
)

data.table::fwrite(
  x = saeb_15_dta_G46, 
  file = here("dados", "saeb_2015_g46.csv")
)

#=============================
#= Dados por subgrupo - 2017 =
#=============================

# Join 
saeb_17_dta <- saeb_17_dta %>% 
  filter(
    in_situacao_censo==1 &
      id_dependencia_adm==3
  ) %>% 
  left_join(
    tb_subgrupos, by=c("id_municipio"="CO_MUNICIPIO")
  )

# 50k - 0
saeb_17_dta_0_50 <- saeb_17_dta %>% 
  filter(PORTE == "50k - 0")
# 50k - 100k
saeb_17_dta_50_100 <- saeb_17_dta %>% 
  filter(PORTE == "100k - 50k")
# 100k - 500k
saeb_17_dta_100_500 <- saeb_17_dta %>% 
  filter(PORTE == "500k - 100k")
# G46
saeb_17_dta_G46 <- saeb_17_dta %>% 
  filter(PORTE == "G46 (Todos Pela Educação)")

# Exportação de dados
data.table::fwrite(
  x = saeb_17_dta_0_50, 
  file = here("dados", "saeb_2017_0_50.csv")
)

data.table::fwrite(
  x = saeb_17_dta_50_100, 
  file = here("dados", "saeb_2017_50_100.csv")
)

data.table::fwrite(
  x = saeb_17_dta_100_500, 
  file = here("dados", "saeb_2017_100_500.csv")
)

data.table::fwrite(
  x = saeb_17_dta_G46, 
  file = here("dados", "saeb_2017_g46.csv")
)

#=============================
#= Dados por subgrupo - 2019 =
#=============================

# Join 
saeb_19_dta <- saeb_19_dta %>% 
  filter(
    in_situacao_censo==1 &
      id_dependencia_adm==3
  ) %>% 
  left_join(
    tb_subgrupos, by=c("id_municipio"="CO_MUNICIPIO")
  )

# 50k - 0
saeb_19_dta_0_50 <- saeb_19_dta %>% 
  filter(PORTE == "50k - 0")
# 50k - 100k
saeb_19_dta_50_100 <- saeb_19_dta %>% 
  filter(PORTE == "100k - 50k")
# 100k - 500k
saeb_19_dta_100_500 <- saeb_19_dta %>% 
  filter(PORTE == "500k - 100k")
# G46
saeb_19_dta_G46 <- saeb_19_dta %>% 
  filter(PORTE == "G46 (Todos Pela Educação)")

# Exportação de dados
data.table::fwrite(
  x = saeb_19_dta_0_50, 
  file = here("dados", "saeb_2019_0_50.csv")
)

data.table::fwrite(
  x = saeb_19_dta_50_100, 
  file = here("dados", "saeb_2019_50_100.csv")
)

data.table::fwrite(
  x = saeb_19_dta_100_500, 
  file = here("dados", "saeb_2019_100_500.csv")
)

data.table::fwrite(
  x = saeb_19_dta_G46, 
  file = here("dados", "saeb_2019_g46.csv")
)

