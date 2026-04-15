#------------------------------------------------------------------------------#

#           FUNÇÕES PARA O RELATÓRIO DO SUMÁRIO EXECUTIVO: IBEU -----                                                                                                                                                                               #------------------------------------------------------------------------------#

## PRE-SETS ----

## PACOTES NECESSÁRIOS ----

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,      # pacote para manipulação de dados
               sf,             # pacote para manipulação de dados espaciais
               geobr,          # pacote para baixar shapefiles do Brasil
               flextable,      # pacote para criar tabelas
               survey,         # Pacote para manipulação de dados survey
               srvyr,          # Pacote que ajusta a família tidyverse para dados survey
               rio,            # Pacote para importar e exportar dados
               loadfonts,      # Pacote para carregar fontes
               extrafont,      # Pacote para carregar fontes
               fmsb,            # Pacote para criar gráficos de radar
               readxl,         # Pacote para ler arquivos locais
               readr,
               xlsx,
               patchwork       # gráficos organizados
)





# padrão flextable
# Ajuste das casas decimais
set_flextable_defaults(
  digits = 2,
  decimal.mark = ",",
  big.mark = ".",
  na_str = "<na>"
)

# opção survey
options(survey.lonely.psu = "adjust")



# ----------------------------------------------------------------------------#
# AJUSTES DE DADOS
# ----------------------------------------------------------------------------#
# Grupos de renda RA (PED e Déficit habitacional)

alto <- c('Plano Piloto', 'Jardim Botânico' ,'Lago Norte', 'Lago Sul',
          'Park Way', 'Sudoeste e Octogonal')

medio_alto <- c('Águas Claras', 'Arniqueira', 'Candangolândia', 
                'Cruzeiro', 'Gama','Guará','Núcleo Bandeirante',
                'Sobradinho', 'Sobradinho II', 'Taguatinga',
                'Vicente Pires')

medio <- c('Brazlândia', 'Ceilândia', 'Planaltina', 
           'Riacho Fundo', 'Riacho Fundo II', 'Samambaia',
           'Santa Maria', 'São Sebastião', 'SIA')

baixo <- c('Fercal', 'Itapoã', 'Paranoá', 'Recanto Das Emas', 'SCIA', 
           'Varjão', 'Sol Nascente / Pôr do Sol',
           "Arapoanga","Água Quente")

rural <- c("Área Rural")

goias <- c("Águas Lindas de Goiás","Alexânia","Cidade Ocidental",
           "Cristalina", "Cocalzinho de Goiás","Formosa","Luziânia",
           "Novo Gama","Padre Bernardo","Planaltina de Goiás",
           "Santo Antônio do Descoberto","Valparaíso de Goiás")



# FUNÇÕES MATEMÁTICAS ----

## Função MinMax ----
#------------------------------------------------------------------------------#
calculaMinMax <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}

# FUNÇÕES OPERACIONAIS ----

## FAZER DOWNLOAD E LEITURA DE ARQUIVOS .CSV ----
#------------------------------------------------------------------------------#

download_e_leitura_csv <- function (url, file ) {
  tempFile <- tempfile()
  
  download.file(url, tempFile)
  csv <- read_csv2(tempFile)
  
}

# Função para criar gráficos de dimensões usando cores do IBEU
criar_grafico_dim <- function(df, dim_col) {
  
  # mapa de títulos
  titulos <- c(
    "Dim1" = "D1 - Mobilidade urbana",
    "Dim2" = "D2 - Condições ambientais urbanas",
    "Dim3" = "D3 - Condições habitacionais urbanas",
    "Dim4" = "D4 - Atendimento de serviços coletivos urbanos",
    "Dim5" = "D5 - Infraestrutura urbana",
    "IBEU" = "IBEU"
  )
  
  df %>%
    # mutate(classificacao = case_when(
    #   !!sym(dim_col) < 0.5 ~ "Muito ruim",
    #   !!sym(dim_col) >= 0.5 & !!sym(dim_col) < 0.7 ~ "Ruim",
    #   !!sym(dim_col) >= 0.7 & !!sym(dim_col) < 0.8 ~ "Médio",
    #   !!sym(dim_col) >= 0.8 & !!sym(dim_col) < 0.9 ~ "Bom",
    #   !!sym(dim_col) >= 0.9 & !!sym(dim_col) <= 1 ~ "Muito bom"
    # )
    mutate(classificacao = case_when(
      !!sym(dim_col) >= 0.0 & !!sym(dim_col) < 0.2 ~  "0.0 - 0.2",
      !!sym(dim_col) >= 0.2 & !!sym(dim_col) < 0.4 ~ "0.2 - 0.4",
      !!sym(dim_col) >= 0.4 & !!sym(dim_col) < 0.6 ~ "0.4 - 0.6",
      !!sym(dim_col) >= 0.6 & !!sym(dim_col) < 0.8 ~ "0.6 - 0.8",
      !!sym(dim_col) >= 0.8 & !!sym(dim_col) <= 1 ~ "0.8 - 1.0"
    ),
    #quantis
    # mutate(classificacao = case_when(
    #   !!sym(dim_col) <= quantile(!!sym(dim_col), 0.1, na.rm = TRUE) ~ "Muito ruim",
    #   !!sym(dim_col) <= quantile(!!sym(dim_col), 0.3, na.rm = TRUE) ~ "Ruim",
    #   !!sym(dim_col) <= quantile(!!sym(dim_col), 0.7, na.rm = TRUE) ~ "Médio",
    #   !!sym(dim_col) <= quantile(!!sym(dim_col), 0.9, na.rm = TRUE) ~ "Bom",
    #   TRUE ~ "Muito bom"
    # ),
    classificacao = factor(
      classificacao,
      levels = c(
        "0.0 - 0.2",
        "0.2 - 0.4",
        "0.4 - 0.6",
        "0.6 - 0.8",
        "0.8 - 1.0"
      )
    )) %>%
    
    ggplot(aes(y = reorder(RA, !!sym(dim_col)), 
               x = !!sym(dim_col), 
               label = round(!!sym(dim_col), 2), 
               fill = classificacao)) +
    
    geom_col() +
    geom_text(hjust = -0.2, vjust = 0.2, size = 3.5, aes(family = 'serif')) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    
    labs(
      title = titulos[dim_col],
      x = '',
      y = ''
    ) +
    
    theme_light() +
    theme(
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text = element_text(size = 14, family = 'serif'),
      axis.text.y = element_text(size = 14, family = 'serif'),
      axis.text.x = element_text(size = 14, family = 'serif'),
      plot.title = element_text(size = 16, family = 'serif', face = 'bold', hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

#Função para criar gráficos de dimensões separados por RA
grafico_barras_classificacao <- function(df, classe, cor = "#357EBDFF") {
  
  # 1. Filtrar pela classificação
  df_filtrado <- df %>%
    mutate(classificacao = case_when(
      IBEU < 0.2 ~ "Muito ruim",
      IBEU >= 0.2 & IBEU < 0.4 ~ "Ruim",
      IBEU >= 0.4 & IBEU < 0.6 ~ "Médio",
      IBEU >= 0.6 & IBEU < 0.8 ~ "Bom",
      IBEU >= 0.8 & IBEU <= 1 ~ "Muito bom"
    )) %>%
    
    filter(classificacao == classe)
  
  # 2. Transformar para formato longo
  df_long <- df_filtrado %>%
    select(RA, Dim1, Dim2, Dim3, Dim4, Dim5, IBEU) %>%
    rename(
      "D1\nMobilidade" = Dim1,#\n para quebra de linha
      "D2\nAmbientais" = Dim2,
      "D3\nHabitacionais" = Dim3,
      "D4\nServiços" = Dim4,
      "D5\nInfraestrutura" = Dim5,
      "IBEU\nGeral" = IBEU
    ) %>%
    pivot_longer(-RA, names_to = "Dim", values_to = "valor") %>%
    mutate(Dim = factor(Dim, levels = c(
      "D1\nMobilidade", "D2\nAmbientais", "D3\nHabitacionais",
      "D4\nServiços", "D5\nInfraestrutura", "IBEU\nGeral"
    )))
  
  # 3. Criar gráficos por RA
  graficos <- df_long %>%
    split(.$RA) %>%
    lapply(function(d) {
      ggplot(d, aes(x = Dim, y = valor)) +
        geom_col(fill = cor, width = 0.6) +
        labs(x = NULL, y = NULL, title = unique(d$RA)) +
        theme_light() +
        theme(
          plot.title = element_text(hjust = 0.5, family = "serif"),
          axis.text.x = element_text(size = 10, family = "serif"),
          axis.text.y = element_text(size = 12, family = "serif"),
          plot.margin = margin(10, 10, 20, 10)
        )
    })
  
  # 4. Layout com 2 colunas
  ncol <- 2
  
  # organizar
  wrap_plots(graficos, ncol=ncol)
}


gerar_graficos_dim <- function(base, prefixo) {
  dims <- c(paste0("Dim", 1:5),"IBEU")
  
  for (d in dims) {
    g <- criar_grafico_dim(base, d)
    
    ggsave(
      paste0("output/figuras/", d, prefixo, ".png"),
      plot = g,
      width = 8, height = 10, dpi = 300
    )
  }
}


gerar_radar <- function(base, filtro, prefixo, nome_arquivo) {
  # Definir os valores máximos e mínimos para cada indicador
  max_min <- data.frame(
    Dim1 = c(1, 0),
    Dim2 = c(1, 0),
    Dim3 = c(1, 0),
    Dim4 = c(1, 0),
    Dim5 = c(1, 0)
  )
  # Filtrar somente as RAs com os melhores valores do IBEU
  dados_filtrados <- base %>%
    dplyr::filter({{filtro}}) %>%
    dplyr::arrange(desc(IBEU)) %>%
    head(3)
  
  df_radar <- dados_filtrados %>%
    dplyr::select(-RA, -n, -IBEU) %>%
    rbind(max_min, .)
  # Lista com os nomes das RAs
  lista_ras <- dados_filtrados$RA
  
  # Abrir um dispositivo gráfico para salvar o arquivo
  png(paste0(nome_arquivo, prefixo, ".png"), width = 800, height = 600)
  
  # MUITO IMPORTANTE
  # Ajustar a margem do gráfico para dar espaço à legenda
  par(mar = c(7, 4, 4, 2))   # espaço para legenda
  par(xpd = TRUE)            # permite desenhar fora do plot
  
  radarchart(
    df_radar,
    pcol = 2:4,# Cores das linhas
    plwd = 2,# Largura da linha
    plty = 1,# Tipo de linha
    cglcol = "grey",# Cor da grade
    cglty = 1,# Tipo de linha da grade
    vlabels = c("Mobilidade (D1)", "Ambientais (D2)", "Habitacionais (D3)", "Serviços (D4)", "Infraestrutura (D5)")# Posição do rótulo
  )
  # Adicionar a legenda fora da área de plotagem
  legend("bottom",# Posição da legenda
         legend = lista_ras,# Rótulos da legenda (nomes dos municípios)
         col = 2:4,                   # Cores correspondentes
         lty = 1,                     # Tipo de linha
         lwd = 2,                     # Largura da linha
         horiz = TRUE,                # Legenda em linha (horizontal)
         bty = "n",                   # Sem borda
         x.intersp = 1.5,             # Espaçamento horizontal entre os rótulos
         y.intersp = 1.5,             # Espaçamento vertical entre os rótulos
         inset = c(0, -0.1)           # Ajusta a distância da legenda em relação ao gráfico
         )
  
  dev.off()
}


