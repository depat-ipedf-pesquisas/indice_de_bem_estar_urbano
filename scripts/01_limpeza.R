#rm(list = ls())

source("R/utils.R")

# ----------------------------------------------------------------------------#
# CARREGAR DADOS
# ----------------------------------------------------------------------------#

#dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
#dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

# URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Domilícios
url_domi <- "https://pdad.ipe.df.gov.br/files/reports/domicilios.zip"
# URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Moradores
url_mora <- "https://pdad.ipe.df.gov.br/files/reports/moradores.zip"

pdad_mor_2024 <- download_e_leitura_csv(url_mora, "moradores.csv")

# TEMPORÁRIO (url com problema)
pdad_dom_2024 <- read_excel("data/raw/domicilios.xlsx")

dicionario_domicilios_ras <- read_excel("data/raw/dicionario_de_variaveis_pdada_2024_público.xlsx", sheet = 3) %>%
  rename(descricao = Coluna,
         codigo_rotulo = Valor,
         rotulo = `Descrição do valor`)

dicionario_domicilios_dom <- read_excel("data/raw/dicionario_de_variaveis_pdada_2024_público.xlsx", sheet = 1)
dicionario_domicilios_mor <- read_excel("data/raw/dicionario_de_variaveis_pdada_2024_público.xlsx", sheet = 2)


# Criação de grupos de renda RA


cte_gruposra <- dicionario_domicilios_ras[c(1:49), c(2,3)] %>%
  mutate(
    grupos_ra = case_when(
      rotulo %in% alto ~ 1,
      rotulo %in% medio_alto ~ 2,
      rotulo %in% medio ~ 3,
      rotulo %in% baixo ~ 4,
      rotulo %in% goias ~ 5,
      rotulo %in% rural ~ 6,
      TRUE ~ NA
    ),
    codigo_rotulo = as.integer(codigo_rotulo)
  ) %>%
  rename(codigo_RA = codigo_rotulo, RA = rotulo) %>%
  mutate(
    grupos_ra = factor(# Fatorizar grupos de renda RA
      grupos_ra,
      levels = 1:6,
      labels = c("Grupo 1", "Grupo 2", "Grupo 3",
                 "Grupo 4", "Goiás", "Área Rural")
    )
  )

pdad_dom_2024 <- pdad_dom_2024 %>% 
  left_join(cte_gruposra, by = c("localidade" = "codigo_RA"))

# ----------------------------------------------------------------------------#
# JOIN BASES
# ----------------------------------------------------------------------------#

# Consultar o índice das colunas de mesmo nome entre as bases de domicílios e moradores
# dessa forma é possível selecionar as colunas de interesse para realizar o JOIN

x <- which((names(pdad_dom_2024) %in% names(pdad_mor_2024 %>% select(-A01nficha)))) 

# Verificar quais são as colunas
duplicadas <- names(pdad_dom_2024)[x]


# Fazer o join das bases
# Mantém todas as linhas da base de moradores (left join)
# Remove as colunas duplicadas da base de domicílios antes de juntar
pdad <- pdad_mor_2024 %>%
  left_join(pdad_dom_2024 %>%
              select(-all_of(duplicadas)),
            by = c("A01nficha" = "A01nficha"))

# ----------------------------------------------------------------------------#
# PLANO AMOSTRAL
# ----------------------------------------------------------------------------#

# Declarar desenho amostral para 2024
amostra <- survey::svydesign(
  id      = ~ A01nficha,       # identificador
  strata  = ~ setor_distrito,  # estrato
  weights = ~ peso_mor,        # peso do morador
  nest    = TRUE,
  data    = pdad
)

# Converter para objeto srvyr
amostra <- srvyr::as_survey(amostra)

# ajuste nome
amostra$variables$RA[amostra$variables$RA == "SCIA"] <- "SCIA / Estrutural"

# ----------------------------------------------------------------------------#
# EXPORTAR BASE LIMPA
# ----------------------------------------------------------------------------#

saveRDS(amostra, "data/interim/amostra_2024.rds")
