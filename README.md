
# Índice de bem-estar urbano (IBEU)

<!-- badges: start -->
<!-- badges: end -->
O Índice de Bem-Estar Urbano (IBEU) se fundamenta na concepção das condições materiais que a cidade deve propiciar a seus habitantes
de forma coletiva promovendo o bem-estar em ambiente urbano. Nesse contexto, o IBEU é composto por cinco dimensões centrais de análise
(i) A mobilidade (ii) condições ambientais e (iii) habitacionais, (iv) a infraestrutura e os (v) serviços coletivos urbanos que abarcam 19 (dezenove) 
indicadores. Sua metodologia é baseado no índice do Observatório das Metrópoles considerando as particularidades territoriais do DF e da PMB.


Os processamentos de cálculo do IBEU e de geração de resultados (tabelas, gráficos e mapas) foram feitos no software R e são contemplados
nos arquivos presentes neste repositório, tendo como base o conjunto de dados disponível em data/raw.
As fontes desses dados e os tratamentos realizados para se obter a planilha citada são estão disponíveis em scripts/01_limpeza.R.


A disponibilização do código-fonte e da descrição dos procedimentos realizados é um caminho facilitador para a reprodução do índice para outros municípios.


## Escala de medição:

Todas essas métricas sofrem um processo de padronização, de forma que os indicadores finais utilizados estejam em uma escala entre 0 e 1,
possibilitando a comparação entre indicadores e entre regiões: quanto mais perto de 1, melhor é o bem estar urbano percebido pela população.
Com isso, o IBEU é calculado como uma média dessas cinco dimensões padronizadas.


## Periodicidade

O IBEU PARA O DF E PMB possui atualização bienal, conforme publicação da Pesquisa Distrital por Amostra de Domicílios Ampliada – PDAD-A.
Mais informações sobre o projeto podem ser consultadas nos relatórios publicados, disponíveis na pasta reports/ deste repositório.


## Reprodutibilidade

O pacote renv do R foi utilizado com o intuito de garantir reprodutibilidade, documentando os pacotes usados e suas versões, além de facilitar a reprodução das condições do ambiente por um novo usuário.


## Pastas e Arquivos

Segue o descritivo das pastas e dos arquivos do repositório:


**data/**: pasta com os arquivos de dados utilizados ou gerados no projeto.

- **raw/**: contém os dados brutos de entrada ( bases da PDAD e dicionário de variáveis).
- **interim/**: armazena bases intermediárias já tratadas e padronizadas (amostra_2024.rds, objeto survey).
- **output/**: reúne as bases finais consolidadas, incluindo os indicadores por dimensão e o IBEU.

---

**R/**: pasta que contém funções auxiliares escritas em R, como:

- Funções para criação de gráficos (`criar_grafico_dim`, `grafico_barras_classificacao`, etc.)
- Rotinas de padronização e tratamento de variáveis
- Funções reutilizáveis ao longo do pipeline

---

**scripts/**: diretório com os scripts principais do projeto. Os arquivos seguem uma ordem lógica de execução:

- **01_tratamento_dados.R**: leitura, limpeza e padronização dos dados
- **02_construcao_indice.R**: cálculo das dimensões e do IBEU
- **03_graficos.R**: geração dos gráficos e exportação dos resultados visuais

---

**outputs/**: utilizado para armazenar saídas analíticas adicionais, como tabelas ou bases exportadas.

**outputs/figuras/**: diretório com os produtos gráficos gerados pelos scripts, incluindo:

- Gráficos de barras por dimensão (D1 a D5)
- Distribuição do IBEU por região
- Gráficos por classificação ((0-0.2),(0.2-0.4), etc.)
- Gráficos radar (perfil das dimensões por grupo de regiões)

**outputs/tabelas/**: diretório com as tabelas geradas pelos scripts, incluindo:

- IBEU_resultados.xlsx (resultados das dimensões padronizadas)
- indices.xlsx (indices padronizados)
- tabela_proporcoes_xlsx (indices antes da padronização)

---

**README.md**: arquivo de documentação principal do projeto, contendo descrição, metodologia e instruções de uso.

---

**.gitignore**: arquivo de configuração do Git que especifica quais arquivos e pastas não devem ser versionados (por exemplo, arquivos temporários do R e bibliotecas locais do renv).

---

**.Rbuildignore**: define quais arquivos e pastas devem ser ignorados durante o processo de build do projeto (especialmente útil caso o projeto seja estruturado como pacote R). Diferente do .gitignore, ele não controla versionamento, mas sim o que entra no build.

---

**.Rprofile**: arquivo de configuração do R utilizado para ativar automaticamente o ambiente renv ao abrir o projeto, garantindo o uso das versões corretas dos pacotes.

---

**\*.Rproj**: arquivo de projeto do RStudio, que define o diretório raiz e facilita a organização do fluxo de trabalho.

---

**renv.lock**: pasta criada automaticamente pelo pacote renv, contendo os arquivos necessários para recriar o ambiente computacional do projeto, incluindo as versões exatas dos pacotes utilizados.




# Créditos

Equipe:

- Aline da Nóbrega Oliveira

- Leandro de Almeida Salles

- Werner Bessa Vieira

- Yasmin Lírio Souza de Oliveira


Contato: <coea@ipe.df.gov.br>


**Fonte para citação**: IPEDF Codeplan, Instituto de Pesquisa e Estatística do Distrito Federal. Índice de Bem-estar Urbano do Distrito Federal e Periferia Metropolitana de Brasília - IBEU-DF e IBEU-PMB  - 2024. Brasília, 2026.

O uso dos dados é livre, desde que citada a fonte.

