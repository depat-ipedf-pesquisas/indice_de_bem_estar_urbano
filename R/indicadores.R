# -----------------------------------------------------------
# FUNÇÃO: calc_indicador
# -----------------------------------------------------------
# Objetivo:
# Calcular proporções (indicador), totais e coeficiente de variação
# para variáveis binárias (0/1), de forma padronizada.
#
# Saída:
# - variável de agrupamento (RA, grupos_ra, UF)
# - total categoria 0 (ex: "Caso não favorável")
# - total categoria 1 (ex: "Caso favorável")
# - indicador percentual (D_i)
# - coeficiente de variação (CV D_i)
#
# Argumentos:
# data       -> base survey (srvyr)
# indicador  -> variável binária (0/1)
# group_var  -> variável de agrupamento
# nome_ind   -> nome do indicador (ex: "D1", "D2", ...)
# label_1    -> nome da categoria 1
# label_0    -> nome da categoria 0
# -----------------------------------------------------------

calc_indicador <- function(data, indicador, group_var, 
                           nome_ind = "D1",
                           label_1 = "Sim",
                           label_0 = "Não") {
  
  indicador <- rlang::enquo(indicador)# capturando argumentos como expressões
  group_var <- rlang::enquo(group_var)#guarda o nome da variável
  
  data %>%
    dplyr::filter(!is.na(!!indicador)) %>% #!! serve para usar o argumento como expressão
    
    dplyr::group_by(!!group_var) %>% #agrupar
    
    dplyr::summarise(
      prop = srvyr::survey_mean(!!indicador, vartype = "cv", na.rm = TRUE), #coeficiente de variação, média de var binária é a proporção
      total_1 = srvyr::survey_total((!!indicador) == 1, na.rm = TRUE),
      total_0 = srvyr::survey_total((!!indicador) == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    dplyr::mutate(
      !!nome_ind := 100 * prop,
      !!paste0("CV ", nome_ind) := prop_cv
    ) %>%
    
    dplyr::rename(
      !!label_1 := total_1,
      !!label_0 := total_0
    ) %>%
    
    dplyr::select(
      !!group_var,
      dplyr::all_of(c(label_1, label_0, nome_ind, paste0("CV ", nome_ind)))
    )
}



# -------------------------------------------------------------------------
# só para a var1 que não é a proporção
# -------------------------------------------------------------------------

calc_indicador2 <- function(data, indicador, group_var, 
                            indicador_bin = NULL,
                            nome_ind = "D1",
                            label_1 = "Sim",
                            label_0 = "Não") {
  
  indicador <- rlang::enquo(indicador)
  group_var <- rlang::enquo(group_var)
  indicador_bin_quo <- rlang::enquo(indicador_bin)
  
  usa_bin <- !rlang::quo_is_null(indicador_bin_quo)
  
  data %>%
    dplyr::filter(!is.na(!!indicador)) %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::summarise(
      prop = srvyr::survey_mean(!!indicador, vartype = "cv", na.rm = TRUE),
      total_1 = if (usa_bin) srvyr::survey_total((!!indicador_bin_quo) == 1, na.rm = TRUE) else NA_real_,
      total_0 = if (usa_bin) srvyr::survey_total((!!indicador_bin_quo) == 0, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!nome_ind := 100 * prop,
      !!paste0("CV ", nome_ind) := prop_cv
    ) %>%
    dplyr::rename(
      !!label_1 := total_1,
      !!label_0 := total_0
    ) %>%
    dplyr::select(
      !!group_var,
      dplyr::all_of(c(label_1, label_0, nome_ind, paste0("CV ", nome_ind)))
    )
}

# -------------------------------------------------------------------------
# INDICADORES
# -------------------------------------------------------------------------

# D1 - Tempo de deslocamento (ponderado)
var1_fun <- function(data, group_var) {
  calc_indicador2(
    data = data %>%
      dplyr::mutate(
        
        # indicador ponderado (qualidade do deslocamento)
        ind_score = dplyr::case_when(
          I04 == 1 & I10 == 1 ~ 1.0,   # até 15 min
          I04 == 1 & I10 == 2 ~ 0.9,  # 15–30
          I04 == 1 & I10 == 3 ~ 0.8,   # 30–45
          I04 == 1 & I10 == 4 ~ 0.7,   # 45–60
          I04 == 1 & I10 >= 5 & I10 <= 9 ~ 0,  # >1h
          TRUE ~ NA_real_
        )
        ),
    # # usamos o score no índice
    indicador = ind_score,
    group_var = {{group_var}},
    nome_ind = "D1"
  )
}

# D2
## Pessoas residentes de domicílios cujo entorno é arborizado ----
var2_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind_arbor = case_when(
          B19_1 == 1 ~ 1,
          B19_1 == 2 ~ 0)
      ),
    indicador = ind_arbor,
    group_var = {{group_var}},
    nome_ind = "D2",
    label_1 = "Possui entorno arborizado",
    label_0 = "Não possui entorno arborizado"
  )
}

#D3
## Pessoas residentes de domicílios cujo entorno possui ruas que alagam ----
var3_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind_ruas = case_when(
          B18_3 == 2 ~ 1,  # não possui ruas que alagam
          B18_3 == 1 ~ 0   # possui ruas que alagam
        )
      ),
    indicador = ind_ruas,
    group_var = {{group_var}},
    nome_ind = "D3",
    label_1 =  "Não possui ruas que alagam",
    label_0 =  "Possui ruas que alagam"
  )
}

#D4
## Pessoas residentes de domicílios cujo entorno possui esgoto a céu aberto ----
var4_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B18_2 == 2 ~ 1,  # não possui esgoto a céu aberto
          B18_2 == 1 ~ 0   # possui esgoto a céu aberto
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D4",
    label_1 =  "Não possui esgotamento a céu aberto",
    label_0 =  "Possui esgotamento a céu aberto"
  )
}

#D5
## Não possui lixo acumulado (entulho) ----
var5_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind =  case_when(
          B18_1 == 2 ~ 1,  # não possui lixo acumulado
          B18_1 == 1 ~ 0   # possui lixo acumulado
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D5",
    label_1 =  "Não possui lixo acumulado (entulho)",
    label_0 =  "Possui lixo acumulado (entulho)"
  )
}

#D6
## Domicílio particular permanente ----
var6_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          (E04 == 1) & B01 == 1 ~ 1,  # Domicílio particular permanente
          (E04 == 1) & B01 == 2 ~ 0   # Domicílio improvisado
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D6",
    label_1 =  "Domicílio particular permanente",
    label_0 =  "Domicílio improvisado"
  )
}

#D7
##  Densidade domiciliar ----
var7_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        razaopesdom = A01npessoas / B11,  # densidade domiciliar
        ind = if_else(razaopesdom <= 2, 1, 0)  # 1 = ≤2 pessoas por dormitório, 0 = >2
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D7",
    label_1 =  "Até 2 pessoas por dormitório",
    label_0 =  "Mais de 2 pessoas por dormitório"
  )
}

#D8
##  Densidade morador/banheiro ----
var8_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      filter(B12 != 0) %>%  # evita divisão por zero
      mutate(
        razaopesbanheiro = A01npessoas / B12,
        ind = if_else(razaopesbanheiro <= 4, 1, 0)  # 1 = ≤4 pessoas por banheiro
        
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D8",
    label_1 =  "Até 4 pessoas por banheiro",
    label_0 =  "Mais de 4 pessoas por banheiro"
  )
}

#D9
## Material das paredes dos domicílios ----
var9_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B07 == 1 ~ 1,                       # Alvenaria com revestimento
          B07 %in% c(3,6) & B01 == 1 ~ 1,     # 3	Madeira para a construção (permanente) e 6 outros (permanente)
          TRUE ~ 0                             # Outros materiais(Alvenaria sem revestimento, Madeira aproveitada de tapumes, embalagens e andaimes, Lona, papelão,tecidos ou semelhantes)
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D9",
    label_1 =  "Material adequado nas paredes",
    label_0 =  "Material inadequado nas paredes"
  )
}

#D10
## Atendimento de água ----
var10_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B13 == 1 ~ 1,  # Rede geral CAESB
          B13 == 2 ~ 0       # não
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D10",
    label_1 =  "Água adequada (rede geral)",
    label_0 =  "Água inadequada"
  )
}

#D11
## Atendimento de esgoto ----
var11_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B14 == 1 ~ 1,  # Acesso à rede geral de esgoto
          B14 == 2 ~ 0       # Sem acesso
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D11",
    label_1 =  "Esgoto adequado (rede geral)",
    label_0 =  "Esgoto inadequado"
  )
}

#D12
## Coleta de lixo ----
var12_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B16_1 == 1 | B16 == 1 ~ 1,  # Possui coleta de lixo
          B16 == 2  ~ 0                    # Não possui
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D12",
    label_1 =  "Coleta de lixo",
    label_0 =  "Sem coleta de lixo"
  )
}

#D13
## Atendimento de energia ----
var13_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B15 == 1 | B15_1 == 1 ~ 1,  # Possui energia adequada (rede ou solar)
          #TRUE ~ 0
          B15 == 2 & B15_1 == 2 ~ 0   # Não possui
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D13",
    label_1 =  "Energia adequada",
    label_0 =  "Energia inadequada"
  )
}

#D14
## Iluminação pública ----
var14_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_3 == 1 ~ 1,  # Possui iluminação pública
          B17_3 == 2 ~ 0          # Não possui
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D14",
    label_1 =  "Iluminação pública",
    label_0 =  "Sem Iluminação pública"
  )
}

#D15
## Pavimentação ----
var15_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_1 == 1 ~ 1,  # Rua pavimentada/asfaltada
          B17_1 == 2 ~ 0          # Rua não pavimentada
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D15",
    label_1 =  "Pavimentação",
    label_0 =  "Sem Pavimentação"
  )
}

#D16
## Calçada ----
var16_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_2 == 1 ~ 1,  # Rua possui calçada
          B17_2 == 2 ~ 0          # Rua não possui calçada
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D16",
    label_1 =  "Calçada",
    label_0 =  "Sem Calçada"
  )
}
#D17
# RAMPA ----
var17_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_2_1 == 1 ~ 1,  # Possui rampa
          B17_2_1 == 2 ~ 0            # Não possui
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D17",
    label_1 =  "Rampa de acesso",
    label_0 =  "Sem Rampa de acesso"
  )
}

#D18
## Bueiro ou boca de lobo ----
var18_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_4 == 1 ~ 1,  # Possui bueiro/boca de lobo (drenagem)
          B17_4 == 2 ~ 0          # Não possui
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D18",
    label_1 =  "Bueiro ou boca de lobo",
    label_0 =  "Sem Bueiro ou boca de lobo"
  )
}

#D19
## Qualidade da calçada ----
var19_fun <- function(data, group_var) {
  calc_indicador(
    data = data %>%
      mutate(
        ind = case_when(
          B17_2 == 1 & (B17_2_2 %in% c(1,2)) ~ 1,  # Calçada presente e em qualidade adequada
          B17_2 == 1 & (B17_2_2 %in% c(3,4,5)) ~ 0 # em má qualidade
        )
      ),
    indicador = ind,
    group_var = {{group_var}},
    nome_ind = "D19",
    label_1 =  "Qualidade da calçada",
    label_0 =  "Má Qualidade da calçada"
  )
}