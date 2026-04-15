#rm(list = ls())

source("R/utils.R")
source("R/indicadores.R")

# carregar dados já calculados
amostra <- readRDS("data/interim/amostra_2024.rds")
dados <- readRDS("data/processed/indicadores.rds")

var1_ra <- dados$var1_ra
var2_ra <- dados$var2_ra
var3_ra <- dados$var3_ra
var4_ra <- dados$var4_ra
var5_ra <- dados$var5_ra
var6_ra <- dados$var6_ra
var7_ra <- dados$var7_ra
var8_ra <- dados$var8_ra
var9_ra <- dados$var9_ra
var10_ra <- dados$var10_ra
var11_ra <- dados$var11_ra
var12_ra <- dados$var12_ra
var13_ra <- dados$var13_ra
var14_ra <- dados$var14_ra
var15_ra <- dados$var15_ra
var16_ra <- dados$var16_ra
var17_ra <- dados$var17_ra
var18_ra <- dados$var18_ra
var19_ra <- dados$var19_ra

pop_ra  <- dados$pop_ra
# (IMPORTANTE)
# aqui você precisa já ter rodado o script 02_calculo_ibeu.R


dir.create("output/tabelas", recursive = TRUE, showWarnings = FALSE)


#--------------------------------------
#    ---   tabelas

tabela1_1 <- pop_ra %>%
  select(-n_se) %>%
  left_join(var1_ra) %>%
  left_join(var2_ra) %>%
  left_join(var3_ra) %>%
  left_join(var4_ra) %>%
  left_join(var5_ra) %>%
  left_join(var6_ra) %>%
  left_join(var7_ra) %>%
  left_join(var8_ra) %>%
  left_join(var9_ra) %>%
  left_join(var10_ra) %>%
  left_join(var11_ra) %>%
  left_join(var12_ra) %>%
  left_join(var13_ra) %>%
  left_join(var14_ra) %>%
  left_join(var15_ra) %>%
  left_join(var16_ra) %>%
  left_join(var17_ra) %>%
  left_join(var18_ra) %>%
  left_join(var19_ra) %>%
  ungroup() %>%
  select(-localidade)


df_DF <- tabela1_1[!tabela1_1$RA %in% goias & !tabela1_1$RA %in% rural,]
df_GO <- tabela1_1[tabela1_1$RA %in% goias,]

writexl::write_xlsx(
  list(DF = df_DF, GO = df_GO),
  path = "output/tabelas/tabela_proporcoes.xlsx"
)


#normalização

tabela2_1 <- tabela1_1 %>% 
  select(RA, n, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, 
         D11, D12, D13, D14, D15, D16, D17, D18, D19)

df_DF <- tabela2_1[!tabela2_1$RA %in% goias & !tabela2_1$RA %in% rural,]
df_GO <- tabela2_1[tabela2_1$RA %in% goias,]

tabela2_2_DF <- df_DF %>%
  mutate(across(3:21, ~ calculaMinMax(.)))

tabela2_2_GO <- df_GO %>%
  mutate(across(3:21, ~ calculaMinMax(.)))


tabela4_1_DF <- tabela2_2_DF %>% 
  mutate(Dim1 = D1, 
         Dim2 = (D2+D3+D4+D5)/4, 
         Dim3 = (D6+D7+D8+D9)/4, 
         Dim4 = (D10+D11+D12+D13)/4, 
         Dim5 = (D14+D15+D16+D17+D18+D19)/6, 
         IBEU = (Dim1+Dim2+Dim3+Dim4+Dim5)/5) %>% 
  select(RA, n, Dim1, Dim2, Dim3, Dim4, Dim5, IBEU)

tabela4_1_GO <- tabela2_2_GO %>% 
  mutate(Dim1 = D1, 
         Dim2 = (D2+D3+D4+D5)/4, 
         Dim3 = (D6+D7+D8+D9)/4, 
         Dim4 = (D10+D11+D12+D13)/4, 
         Dim5 = (D14+D15+D16+D17+D18+D19)/6, 
         IBEU = (Dim1+Dim2+Dim3+Dim4+Dim5)/5) %>% 
  select(RA, n, Dim1, Dim2, Dim3, Dim4, Dim5, IBEU)


writexl::write_xlsx(
  list(
    DF = tabela4_1_DF,
    GO = tabela4_1_GO
  ),
  path = "output/tabelas/IBEU_resultados.xlsx"
)

#indices

# tabela com cv
tabela2_3 <- tabela1_1 %>% 
  select(RA, n, D1,`CV D1`, D2,`CV D2`, D3,`CV D3`, D4,`CV D4`, D5,`CV D5`,
         D6,`CV D6`, D7,`CV D7`, D8,`CV D8`, D9,`CV D9`, D10,`CV D10`, 
         D11,`CV D11`, D12,`CV D12`, D13,`CV D13`, D14,`CV D14`, D15,`CV D15`,
         D16,`CV D16`, D17,`CV D17`, D18,`CV D18`, D19,`CV D19`)

# esse CV só vale para Di antes de calculaMinMax(.)))

#separando goias e df

df_DF <- tabela2_3[!tabela2_3$RA %in% goias & !tabela2_3$RA %in% rural,]
df_GO <- tabela2_3[tabela2_3$RA %in% goias,]

names(df_DF)<- c("RA", "n",  "D1 Tempo de deslocamento casa-trabalho", "CV D1",
                 "D2 Arborização no entorno do domicílio", "CV D2",
                 "D3 Ruas que alagam", "CV D3",  "D4 Esgoto a céu aberto no entorno do domicílio", "CV D4",
                 "D5 Entulho no entorno do domicílio", "CV D5", "D6 Espécie do domicílio", "CV D6",
                 "D7 Densidade domiciliar", "CV D7",  "D8 Densidade de banheiro", "CV D8",
                 "D9 Parede", "CV D9" , "D10 Atendimento de água","CV D10", "D11 Atendimento de esgoto","CV D11",
                 "D12 Coleta de lixo","CV D12", "D13 Atendimento de energia","CV D13",
                 "D14 Iluminação pública","CV D14" ,"D15 Pavimentação","CV D15", "D16 Calçada","CV D16",
                 "D17 Rampa","CV D17",
                 "D18 Bueiro ou boca de lobo","CV D18", "D19 Qualidade da calçada","CV D19")

names(df_GO)<-names(df_DF)

est_DF <- df_DF %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        media = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variavel", ".value"),
               names_sep = "_")

est_DF <- est_DF %>%
  mutate(
    across(
      where(is.numeric),
      ~format(round(.x, 3),
              nsmall = 3,
              decimal.mark = ",",
              big.mark = ".")
    )
  )%>%
  rbind(c("INDICES PADRONIZADOS","","","",""))

est_GO <- df_GO %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        media = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variavel", ".value"),
               names_sep = "_")

est_GO <- est_GO %>%
  mutate(
    across(
      where(is.numeric),
      ~format(round(.x, 3),
              nsmall = 3,
              decimal.mark = ",",
              big.mark = ".")
    )
  )%>%
  rbind(c("INDICES PADRONIZADOS","","","",""))



df_DF <- df_DF %>%
  mutate(across(seq(3,40,2), ~ calculaMinMax(.)))

df_GO <- df_GO %>%
  mutate(across(seq(3,40,2), ~ calculaMinMax(.)))


est_DF_2 <- df_DF %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        media = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variavel", ".value"),
               names_sep = "_")

est_DF_2 <- est_DF_2 %>%
  mutate(
    across(
      where(is.numeric),
      ~format(round(.x, 3),
              nsmall = 3,
              decimal.mark = ",",
              big.mark = ".")
    )
  )

estatisticas_DF <-  bind_rows(est_DF, est_DF_2)


est_GO_2 <- df_GO %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        media = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variavel", ".value"),
               names_sep = "_")

est_GO_2 <- est_GO_2 %>%
  mutate(
    across(
      where(is.numeric),
      ~format(round(.x, 3),
              nsmall = 3,
              decimal.mark = ",",
              big.mark = ".")
    )
  )

estatisticas_GO <-  bind_rows(est_GO, est_GO_2)





writexl::write_xlsx(
  list(
    DF = df_DF,
    GO = df_GO,
    #    Rural = df_Rural,
    Estatísticas_DF = estatisticas_DF,
    Estatísticas_GO = estatisticas_GO
  ),
  path = "output/tabelas/indices.xlsx"
)

# ----------------------------------------------------------------------------#
#                         GRAFICOS ----
# ----------------------------------------------------------------------------#

## Configurações dos gráficos ----
loadfonts(device = "win")

mais_cores <- c('#EA5757','#EEA236FF', '#7ACF8F', '#7AC6E0', '#357EBDFF', '#D190FC', '#C9C7C7', '#F1A0C0', '#EADD80')

cores <- c(
  "0.0 - 0.2" = "#EA5757",  # Vermelho
  "0.2 - 0.4" = "#EEA236FF",  # Laranja
  "0.4 - 0.6" = "#7ACF8F",  # verde
  "0.6 - 0.8" = "#7AC6E0",  # azul claro
  "0.8 - 1.0" = "#357EBDFF"   # azul escuro
)

tab_DF <- tabela4_1_DF
tab_GO <- tabela4_1_GO

# concentração alta em faixas intermediárias e baixa disperção
# reduz o poder discriminatório da classificação original.
#as Regiões Administrativas apresentam níveis relativamente próximos de bem-estar urbano,
#exigindo uma escala mais sensível para capturar diferenças relativas.


gerar_graficos_dim(tab_DF, "DF")
gerar_graficos_dim(tab_GO, "GO")




#DF
## IBEU - Class muito bom ----
grafico_bar_muito_bom<-grafico_barras_classificacao(tab_DF, "Muito bom",cor="#357EBDFF")
ggsave("output/figuras/Gráfico-Coluna-MuitoBomDF.png", plot = grafico_bar_muito_bom, width = 10, height = 20, dpi = 300)

## IBEU - Class bom ----
grafico_bar_bom <- grafico_barras_classificacao(tab_DF, "Bom",cor="#7AC6E0")
ggsave("output/figuras/Gráfico-Coluna-BomDF.png", plot = grafico_bar_bom, width = 10, height = 25, dpi = 300)
## IBEU - Class médio ----
grafico_bar_medio <- grafico_barras_classificacao(tab_DF, "Médio",cor="#7ACF8F")
ggsave("output/figuras/Gráfico-Coluna-MédioDF.png", plot = grafico_bar_medio, width = 10, height = 20, dpi = 300)
## IBEU - Class ruim ----
grafico_bar_ruim <- grafico_barras_classificacao(tab_DF, "Ruim",cor="#EEA236FF")
ggsave("output/figuras/Gráfico-Coluna-RuimDF.png", plot = grafico_bar_ruim, width = 10, height = 5, dpi = 300)
## IBEU - Class muito ruim ----
grafico_bar_muito_ruim <- grafico_barras_classificacao(tab_DF, "Muito ruim",cor="#EA5757")
ggsave("output/figuras/Gráfico-Coluna-MuitoRuimDF.png", plot = grafico_bar_muito_ruim, width = 10, height = 10, dpi = 300)



#GO
## IBEU - Class muito bom ----
grafico_bar_muito_bom<-grafico_barras_classificacao(tab_GO, "Muito bom",cor="#357EBDFF")
ggsave("output/figuras/Gráfico-Coluna-MuitoBomGO.png", plot = grafico_bar_muito_bom, width = 10, height = 5, dpi = 300)

## IBEU - Class bom ----
grafico_bar_bom <- grafico_barras_classificacao(tab_GO, "Bom",cor="#7AC6E0")
ggsave("output/figuras/Gráfico-Coluna-BomGO.png", plot = grafico_bar_bom, width = 10, height = 5, dpi = 300)
## IBEU - Class médio ----
grafico_bar_medio <- grafico_barras_classificacao(tab_GO, "Médio",cor="#7ACF8F")
ggsave("output/figuras/Gráfico-Coluna-MédioGO.png", plot = grafico_bar_medio, width = 10, height = 20, dpi = 300)
## IBEU - Class ruim ----
grafico_bar_ruim <- grafico_barras_classificacao(tab_GO, "Ruim",cor="#EEA236FF")
ggsave("output/figuras/Gráfico-Coluna-RuimGO.png", plot = grafico_bar_ruim, width = 10, height = 15, dpi = 300)
## IBEU - Class muito ruim ----
grafico_bar_muito_ruim <- grafico_barras_classificacao(tab_GO, "Muito ruim",cor="#EA5757")
ggsave("output/figuras/Gráfico-Coluna-MuitoRuimGO.png", plot = grafico_bar_muito_ruim, width = 10, height = 10, dpi = 300)


## População residente ----

grafico_bar_pop <- tab_DF %>% # tab4_1
  mutate(classificacao = case_when(IBEU < 0.2 ~ "0.0 - 0.2",
                                   IBEU >= 0.2 & IBEU < 0.4 ~ "0.2 - 0.4",
                                   IBEU >= 0.4 & IBEU < 0.6 ~ "0.4 - 0.6",
                                   IBEU >= 0.6 & IBEU < 0.8 ~ "0.6 - 0.8",
                                   IBEU >= 0.8 & IBEU <= 1 ~ "0.8 - 1.0"), 
         classificacao = factor(classificacao, levels = c("0.0 - 0.2", "0.2 - 0.4",
                                                          "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0"))) %>% 
  group_by(classificacao) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n), 
         label = paste0(format(n, big.mark = '.', decimal.mark = ','), ' (', round(pct*100, 2), ' %)')) %>% 
  ggplot(aes(x = classificacao, y = pct, label = label, fill = classificacao))+
  geom_col()+
  geom_text(hjust = 0.5, vjust = -0.5, aes(family = 'serif'))+
  scale_fill_manual(values = cores)+
  labs(x = "", y = "")+
  theme_light()+
  theme(legend.title = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 14, family = 'serif'), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14, angle = 00, family = 'serif'),
        axis.title.y = element_text(size = 14, family = 'serif'),
        axis.title.x = element_text(size = 14, family = 'serif'), 
        strip.placement = "outside", 
        strip.background = element_blank(), 
        strip.text = element_text(color = 'black', size = 14, family = 'serif', face = 'bold'), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())

ggsave("output/figuras/Gráfico-Coluna-PopulaçãoDF.png", plot = grafico_bar_pop, width = 10, height = 6, dpi = 300)


grafico_bar_pop <- tab_GO %>% # tab4_1
  mutate(classificacao = case_when(IBEU < 0.2 ~ "0.0 - 0.2",
                                   IBEU >= 0.2 & IBEU < 0.4 ~ "0.2 - 0.4",
                                   IBEU >= 0.4 & IBEU < 0.6 ~ "0.4 - 0.6",
                                   IBEU >= 0.6 & IBEU < 0.8 ~ "0.6 - 0.8",
                                   IBEU >= 0.8 & IBEU <= 1 ~ "0.8 - 1.0"), 
         classificacao = factor(classificacao, levels = c("0.0 - 0.2", "0.2 - 0.4",
                                                          "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0"))) %>% 
  group_by(classificacao) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n), 
         label = paste0(format(n, big.mark = '.', decimal.mark = ','), ' (', round(pct*100, 2), ' %)')) %>% 
  ggplot(aes(x = classificacao, y = pct, label = label, fill = classificacao))+
  geom_col()+
  geom_text(hjust = 0.5, vjust = -0.5, aes(family = 'serif'))+
  scale_fill_manual(values = cores)+
  labs(x = "", y = "")+
  theme_light()+
  theme(legend.title = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 14, family = 'serif'), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14, angle = 00, family = 'serif'),
        axis.title.y = element_text(size = 14, family = 'serif'),
        axis.title.x = element_text(size = 14, family = 'serif'), 
        strip.placement = "outside", 
        strip.background = element_blank(), 
        strip.text = element_text(color = 'black', size = 14, family = 'serif', face = 'bold'), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())

ggsave("output/figuras/Gráfico-Coluna-PopulaçãoGO.png", plot = grafico_bar_pop, width = 10, height = 6, dpi = 300)

#--------------
# GRAFICOS DE RADAR

gerar_radar(tab_DF, IBEU >= 0.8, "DF", "output/figuras/radar_muito_bom")
gerar_radar(tab_DF, IBEU >= 0.6 & IBEU < 0.8, "DF", "output/figuras/radar_bom")
gerar_radar(tab_DF, IBEU >= 0.4 & IBEU < 0.6, "DF", "output/figuras/radar_medio")
gerar_radar(tab_DF, IBEU >= 0.2 & IBEU < 0.4, "DF", "output/figuras/radar_ruim")
#gerar_radar(tab_DF, IBEU >= 0 & IBEU < 0.2, "DF", "output/figuras/radar_muito_ruim")

gerar_radar(tab_GO, IBEU >= 0.8, "GO", "output/figuras/radar_muito_bom")
gerar_radar(tab_GO, IBEU >= 0.6 & IBEU < 0.8, "GO", "output/figuras/radar_bom")
gerar_radar(tab_GO, IBEU >= 0.4 & IBEU < 0.6, "GO", "output/figuras/radar_medio")
#gerar_radar(tab_GO, IBEU >= 0.2 & IBEU < 0.4, "GO", "output/figuras/radar_ruim") #não tem dessa categoria
#gerar_radar(tab_GO, IBEU >= 0 & IBEU < 0.2, "GO", "output/figuras/radar_muito_ruim")





## Tabela Distribuição dos pesos dos indicadores - IBEU-DF ----

pesos_indicadores <- data.frame(
  Dimensao = c(
    "D1 - Mobilidade Urbana", "Tempo de deslocamento casa-trabalho",
    "D2 - Condições ambientais urbanas", "Arborização no entorno do domicílio", "Ruas que alagam (NOVO)", "Esgoto a céu aberto no entorno do domicílio", "Entulho no entorno do domicílio",
    "D3 - Condições habitacionais urbanas", "Espécie do domicílio", "Densidade domiciliar", "Densidade de banheiro", "Parede",
    "D4 - Atendimento de serviços coletivos urbanos", "Atendimento de água", "Atendimento de esgoto", "Coleta de lixo", "Atendimento de energia",
    "D5 - Infraestrutura urbana", "Iluminação pública", "Pavimentação", "Calçada", "Rampa (NOVO)", "Bueiro ou boca de lobo", "Qualidade da calçada"
  ),
  Peso_Dimensao = c(
    '1', '1',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/6', '1/6', '1/6', '1/6', '1/6', '1/6'
  ),
  Peso_Indice = c(
    '1/5', '1/5',
    '1/5', '1/20', '1/20', '1/20', '1/20',
    '1/5', '1/20', '1/20', '1/20', '1/20',
    '1/5', '1/20', '1/20', '1/20', '1/20',
    '1/5', '1/30', '1/30', '1/30', '1/30', '1/30', '1/30'
  )
)


pesos_indicadores2 <- data.frame(
  Dimensao = c(
    "D1 - Mobilidade Urbana", "Tempo de deslocamento casa-trabalho",
    "D2 - Condições ambientais urbanas", "Arborização no entorno do domicílio", "Ruas que alagam (NOVO)", "Esgoto a céu aberto no entorno do domicílio", "Entulho no entorno do domicílio",
    "D3 - Condições habitacionais urbanas", "Espécie do domicílio", "Densidade domiciliar", "Densidade de banheiro", "Parede",
    "D4 - Atendimento de serviços coletivos urbanos", "Atendimento de água", "Atendimento de esgoto", "Coleta de lixo", "Atendimento de energia",
    "D5 - Infraestrutura urbana", "Iluminação pública", "Pavimentação", "Calçada", "Rampa (NOVO)", "Bueiro ou boca de lobo", "Qualidade da calçada"
  ),
  Peso_Dimensao = c(
    '1', '1',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/4', '1/4', '1/4', '1/4',
    '1', '1/6', '1/6', '1/6', '1/6', '1/6', '1/6'
  ),
  Peso_Indice = c(
    '1/19', '1/19',
    '4/19', '1/19', '1/19', '1/19', '1/19',
    '4/19', '1/19', '1/19', '1/19', '1/19',
    '4/19', '1/19', '1/19', '1/19', '1/19',
    '6/19', '1/19', '1/19', '1/19', '1/19', '1/19', '1/19'
  )
)

tab_pesos_indicadores <- pesos_indicadores %>%
  flextable() %>% 
  set_table_properties(width = 1) %>% 
  theme_vanilla() %>% 
  align(align = "center", part = "header") %>% 
  set_header_labels(
    Dimensao = "Dimensão/Indicadores",
    Peso_Dimensao = "Peso na Dimensão",
    Peso_Indice = "Peso no Índice"
  ) %>% 
  align(i = c(1, 3, 8, 13, 18), j = 2:3, align = "center") %>% # Alinha ao centro as dimensões principais
  align(i = c(2, 4:7, 9:12, 14:17, 19:24), j = 2:3, align = "right") %>% # Alinha à direita os indicadores
  bg(i = c(1, 3, 8, 13, 18), bg = "#E0EAF6", part = "body") %>% # Cor de fundo para as dimensões
  bold(i = c(1, 3, 8, 13, 18), bold = TRUE) %>% # Negrito para as dimensões
  autofit()

save_as_image(tab_pesos_indicadores, path = "output/figuras/Tabela-pesos-indicadores.png")


## Tabela Distribuição do IBEU-DF por região administrativa ----

tab_tab_ibeu_ra <- tab_DF %>%
  mutate(
    Dim1 = round(Dim1, 2),
    Dim2 = round(Dim2, 2),
    Dim3 = round(Dim3, 2),
    Dim4 = round(Dim4, 2),
    Dim5 = round(Dim5, 2),
    IBEU = round(IBEU, 2)
  ) %>%
  flextable() %>%
  set_table_properties(width = 0.3) %>%
  theme_vanilla() %>%
  align(align = "center", part = "header") %>%
  set_header_labels(
    RA = "",
    n = "Estimativa Populacional",
    Dim1 = "IBEU D1",
    Dim2 = "IBEU D2",
    Dim3 = "IBEU D3",
    Dim4 = "IBEU D4",
    Dim5 = "IBEU D5",
    IBEU = ""
  ) %>%
  align( j = c(3:8), align = "center" ) %>% # Alinhar ao Centro valores dimensoes
  bg(j = c(2, 4, 6, 8), bg = "#E0EAF6", part = "body") %>%
  add_header_row(
    values = c("Região Administrativa", "Dimensão 1 Mobilidade Urbana", "Dimensão 2 Condições Ambientais Urbanas", "Dimensão 3 Condições Habitacionais Urbanas", "Dimensão 4 Atendimento de Serviços Urbanos", "Dimensão 5 Infraestrutura Urbana", "IBEU"),
    colwidths = c(2, 1, 1, 1, 1,1, 1)) %>%
  vline(j = c(2,7)) %>%
  merge_v(part = "header") 

save_as_image(tab_tab_ibeu_ra, path = "output/figuras/Tabela-IBEU-RADF.png")


## Tabela Distribuição do IBEU-GO por região administrativa ----

tab_tab_ibeu_ra <- tab_GO %>%
  mutate(
    Dim1 = round(Dim1, 2),
    Dim2 = round(Dim2, 2),
    Dim3 = round(Dim3, 2),
    Dim4 = round(Dim4, 2),
    Dim5 = round(Dim5, 2),
    IBEU = round(IBEU, 2)
  ) %>%
  flextable() %>%
  set_table_properties(width = 0.3) %>%
  theme_vanilla() %>%
  align(align = "center", part = "header") %>%
  set_header_labels(
    RA = "",
    n = "Estimativa Populacional",
    Dim1 = "IBEU D1",
    Dim2 = "IBEU D2",
    Dim3 = "IBEU D3",
    Dim4 = "IBEU D4",
    Dim5 = "IBEU D5",
    IBEU = ""
  ) %>%
  align( j = c(3:8), align = "center" ) %>% # Alinhar ao Centro valores dimensoes
  bg(j = c(2, 4, 6, 8), bg = "#E0EAF6", part = "body") %>%
  add_header_row(
    values = c("Região Administrativa", "Dimensão 1 Mobilidade Urbana", "Dimensão 2 Condições Ambientais Urbanas", "Dimensão 3 Condições Habitacionais Urbanas", "Dimensão 4 Atendimento de Serviços Urbanos", "Dimensão 5 Infraestrutura Urbana", "IBEU"),
    colwidths = c(2, 1, 1, 1, 1,1, 1)) %>%
  vline(j = c(2,7)) %>%
  merge_v(part = "header") 

save_as_image(tab_tab_ibeu_ra, path = "output/figuras/Tabela-IBEU-RAGO.png")

# Criar um dataframe com os dados
referencia_ibeu <- data.frame(
  Categoria = factor(c("0.8 - 1.0", "0.6 - 0.8", "0.4 - 0.6", "0.2 - 0.4", "0.0 - 0.2"), 
                     levels = c("0.8 - 1.0", "0.6 - 0.8", "0.4 - 0.6", "0.2 - 0.4", "0.0 - 0.2")),
  Valor = c(0.2, 0.2, 0.2, 0.2, 0.2)  # Valores ajustados para refletir os intervalos
)

# Criar o gráfico de barras empilhadas
ggplot(referencia_ibeu, aes(x = "", y = Valor, fill = Categoria)) +
  geom_bar(stat = "identity", width = 0.3) +
  labs(x = "IBEU", y = "", fill = "Categoria") +
  theme_minimal() +
  theme(    axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(1, 1, 1, 0.5), "cm"),
            aspect.ratio = 2/2) +  # Ajuste a proporção conforme necessário) +  # Ajustar as margens do gráfico) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), 
                     labels = scales::number_format(accuracy = 0.001)) +
  scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red"))+
  geom_hline(yintercept = c(0, 0.200, 0.400, 0.600, 0.800, 1.000), linetype = "solid", color = "black")

ggsave("output/figuras/Escala de referência do IBEU.png", width = 7, height = 7, dpi = 300)

###########################################################################
#         TESTE DE CONFIABILIDADE
###########################################################################


# Instalar e carregar o pacote psych se necessário
# install.packages("psych")
library(psych)

#repetir para PMB
# Selecionar apenas as colunas das dimensões
dimensoes <- tab_DF %>% dplyr::select(Dim1, Dim2, Dim3, Dim4, Dim5)

# Calcular o Alfa de Cronbach
resultado_alpha <- psych::alpha(dimensoes)

# Ver resultado
print(resultado_alpha)

resultado_alpha$total#  valor do Alfa de Cronbach (consistência interna).

resultado_alpha$item.stats#  estatísticas de cada dimensão, mostrando se alguma reduz a confiabilidade.

resultado_alpha$alpha.drop#  indica como o alfa mudaria se uma dimensão fosse retirada.

# media e variancia da escala
resultado <- lapply(names(dimensoes), function(item) {
  escala_sem_item <- dimensoes %>% select(-all_of(item))
  
  soma <- rowSums(escala_sem_item, na.rm = TRUE)
  
  data.frame(
    Item = item,
    Media = mean(soma),
    Variancia = var(soma)
  )
})

do.call(rbind, resultado)

#organizar 
tabela_itens <- tibble(
  Item = rownames(resultado_alpha$item.stats),
  
  `Média da escala, se o item é desprezado` =
    resultado_alpha$alpha.drop$mean,
  
  `Variância da escala, se o item é desprezado` =
    resultado_alpha$alpha.drop$var,
  
  `Correlação corrigida entre item e total` =
    resultado_alpha$item.stats$r.cor,
  
  `Correlação múltipla ao quadrado` =
    resultado_alpha$item.stats$smc,
  
  `Alfa, se o item é desprezado` =
    resultado_alpha$alpha.drop$raw_alpha
) %>%
  mutate(across(-Item, ~ round(., 3)))

tabela_escala <- tibble(
  `Média` = round(resultado_alpha$total$mean, 2),
  `Variância` = round(resultado_alpha$total$sd^2, 4),
  `Desvio Padrão` = round(resultado_alpha$total$sd, 2),
  `N de itens` = resultado_alpha$total$nvar
)

tabela_confiabilidade <- tibble(
  `Alfa de Cronbach` = round(resultado_alpha$total$raw_alpha, 2),
  `Alfa de Cronbach padronizado` = round(resultado_alpha$total$std.alpha, 2),
  `N de itens` = resultado_alpha$total$nvar
)

tabela_itens <- tabela_itens %>%
  mutate(Item = recode(Item,
                       "Dim1" = "D1",
                       "Dim2" = "D2",
                       "Dim3" = "D3",
                       "Dim4" = "D4",
                       "Dim5" = "D5"))

tabela_itens
tabela_escala
tabela_confiabilidade

