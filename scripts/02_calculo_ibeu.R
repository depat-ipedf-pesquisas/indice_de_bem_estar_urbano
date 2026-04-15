#rm(list = ls())

source("R/utils.R")
source("R/indicadores.R")

# -------------------------------------------------------------------------
# CARREGAR BASE
# -------------------------------------------------------------------------

amostra <- readRDS("data/interim/amostra_2024.rds")

# -------------------------------------------------------------------------
# CALCULAR INDICADORES
# -------------------------------------------------------------------------

# D1
#O indicador D1 representa a média ponderada da qualidade do tempo de deslocamento,
#variando de 0 a 100, onde valores mais altos indicam maior concentração de indivíduos
#em tempos curtos de deslocamento.
var1_ra        <- var1_fun(amostra, RA)
var1_gruposra  <- var1_fun(amostra, grupos_ra)
var1_uf        <- var1_fun(amostra, A01uf)

# D2
var2_ra        <- var2_fun(amostra, RA)
var2_gruposra  <- var2_fun(amostra, grupos_ra)
var2_uf        <- var2_fun(amostra, A01uf)

#D3
var3_ra        <- var3_fun(amostra, RA)
var3_gruposra  <- var3_fun(amostra, grupos_ra)
var3_uf        <- var3_fun(amostra, A01uf)

#D4
var4_ra        <- var4_fun(amostra, RA)
var4_gruposra  <- var4_fun(amostra, grupos_ra)
var4_uf        <- var4_fun(amostra, A01uf)

#D5
var5_ra        <- var5_fun(amostra, RA)
var5_gruposra  <- var5_fun(amostra, grupos_ra)
var5_uf        <- var5_fun(amostra, A01uf)

#D6
var6_ra        <- var6_fun(amostra, RA)
var6_gruposra  <- var6_fun(amostra, grupos_ra)
var6_uf        <- var6_fun(amostra, A01uf)
#D7
var7_ra        <- var7_fun(amostra, RA)
var7_gruposra  <- var7_fun(amostra, grupos_ra)
var7_uf        <- var7_fun(amostra, A01uf)

#D8
var8_ra        <- var8_fun(amostra, RA)
var8_gruposra  <- var8_fun(amostra, grupos_ra)
var8_uf        <- var8_fun(amostra, A01uf)


#D9
var9_ra        <- var9_fun(amostra, RA)
var9_gruposra  <- var9_fun(amostra, grupos_ra)
var9_uf        <- var9_fun(amostra, A01uf)


#D10
var10_ra        <- var10_fun(amostra, RA)
var10_gruposra  <- var10_fun(amostra, grupos_ra)
var10_uf        <- var10_fun(amostra, A01uf)


#D11
var11_ra        <- var11_fun(amostra, RA)
var11_gruposra  <- var11_fun(amostra, grupos_ra)
var11_uf        <- var11_fun(amostra, A01uf)


#D12
var12_ra        <- var12_fun(amostra, RA)
var12_gruposra  <- var12_fun(amostra, grupos_ra)
var12_uf        <- var12_fun(amostra, A01uf)

#D13
var13_ra        <- var13_fun(amostra, RA)
var13_gruposra  <- var13_fun(amostra, grupos_ra)
var13_uf        <- var13_fun(amostra, A01uf)

#D14
var14_ra        <- var14_fun(amostra, RA)
var14_gruposra  <- var14_fun(amostra, grupos_ra)
var14_uf        <- var14_fun(amostra, A01uf)

#D15
var15_ra        <- var15_fun(amostra, RA)
var15_gruposra  <- var15_fun(amostra, grupos_ra)
var15_uf        <- var15_fun(amostra, A01uf)

#D16
var16_ra        <- var16_fun(amostra, RA)
var16_gruposra  <- var16_fun(amostra, grupos_ra)
var16_uf        <- var16_fun(amostra, A01uf)

#D17
var17_ra        <- var17_fun(amostra, RA)
var17_gruposra  <- var17_fun(amostra, grupos_ra)
var17_uf        <- var17_fun(amostra, A01uf)

#D18
var18_ra        <- var18_fun(amostra, RA)
var18_gruposra  <- var18_fun(amostra, grupos_ra)
var18_uf        <- var18_fun(amostra, A01uf)

#D19
var19_ra        <- var19_fun(amostra, RA)
var19_gruposra  <- var19_fun(amostra, grupos_ra)
var19_uf        <- var19_fun(amostra, A01uf)


## População ----

pop_ra <- amostra %>%       # Pop. por RA
  group_by(localidade, RA) %>% 
  summarise(n = survey_total())


pop_gruposra <- amostra %>% # pop. por Grupo de renda
  group_by(grupos_ra) %>% 
  summarise(n = survey_total())

pop_total <- amostra %>%       # Pop. Total
  summarise(n = survey_total(), 
            RA = "Total")


pop_all <- amostra %>%       # Pop. Goiás(52) e Distrito Federal(53)
  group_by(A01uf)%>%
  summarise(n = survey_total())%>%#está certo
  mutate(
    UF = case_when(
      A01uf == 52 ~ "Goiás",
      A01uf == 53 ~ "Distrito Federal"
    ))


saveRDS(list(
  var1_ra = var1_ra,
  var2_ra = var2_ra,
  var3_ra = var3_ra,
  var4_ra = var4_ra,
  var5_ra = var5_ra,
  var6_ra = var6_ra,
  var7_ra = var7_ra,
  var8_ra = var8_ra,
  var9_ra = var9_ra,
  var10_ra = var10_ra,
  var11_ra = var11_ra,
  var12_ra = var12_ra,
  var13_ra = var13_ra,
  var14_ra = var14_ra,
  var15_ra = var15_ra,
  var16_ra = var16_ra,
  var17_ra = var17_ra,
  var18_ra = var18_ra,
  var19_ra = var19_ra,
  
  var1_gruposra = var1_gruposra,
  var2_gruposra = var2_gruposra,
  var3_gruposra = var3_gruposra,
  var4_gruposra = var4_gruposra,
  var5_gruposra = var5_gruposra,
  var6_gruposra = var6_gruposra,
  var7_gruposra = var7_gruposra,
  var8_gruposra = var8_gruposra,
  var9_gruposra = var9_gruposra,
  var10_gruposra = var10_gruposra,
  var11_gruposra = var11_gruposra,
  var12_gruposra = var12_gruposra,
  var13_gruposra = var13_gruposra,
  var14_gruposra = var14_gruposra,
  var15_gruposra = var15_gruposra,
  var16_gruposra = var16_gruposra,
  var17_gruposra = var17_gruposra,
  var18_gruposra = var18_gruposra,
  var19_gruposra = var19_gruposra,
  
  var1_uf = var1_uf,
  var2_uf = var2_uf,
  var3_uf = var3_uf,
  var4_uf = var4_uf,
  var5_uf = var5_uf,
  var6_uf = var6_uf,
  var7_uf = var7_uf,
  var8_uf = var8_uf,
  var9_uf = var9_uf,
  var10_uf = var10_uf,
  var11_uf = var11_uf,
  var12_uf = var12_uf,
  var13_uf = var13_uf,
  var14_uf = var14_uf,
  var15_uf = var15_uf,
  var16_uf = var16_uf,
  var17_uf = var17_uf,
  var18_uf = var18_uf,
  var19_uf = var19_uf,
  
  pop_ra = pop_ra
), "data/processed/indicadores.rds")
