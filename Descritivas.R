# TRABALHO DE ECONOMETRIA
# IZABELA NUNES E PEDRO CABRAL

# ESTATÍSTICAS DESCRITIVAS

# 1. IMPORTANDO PACOTES E BASE -------------------------------------------------
## Importando os pacotes
library(modelsummary)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(sf)
library(cowplot)

## Criando as pastas onde os resultados são salvos
dir.create("graficos", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

## Lendo a base
base <- read.csv("base.csv")

## Criando as variáveis necessárias
### Variáveis de análise
base$investimento_relativo <- base$investimento_liquido / base$rcl
base$capag_alta <- ifelse(is.na(base$capag), NA,
                          ifelse(base$capag %in% c("A+", "A", "B+", "B"), 1, 0))
base$pib_pc <- base$pib / base$pop

### Em mil, para caber na tabela
base$pib_pc_mil <- base$pib_pc / 1000
base$pop_mil <- base$pop / 1000

## Criando a base utilizada na análise
### Só os municípios com informação completa em todas as variáveis
subbase <- base[complete.cases(base), ]

nrow(base)     # Número total de municípios
nrow(subbase)  # Municípios com informação completa

# 2. TABELA DE ESTATÍSTICAS DESCRITIVAS (LaTeX) --------------------------------
options("modelsummary_format_numeric_latex" = "plain")

## Base de análise
datasummary(
  (`Investimento / RCL` = investimento_relativo) +
    (`CAPAG alta` = capag_alta) +
    (`População (mil hab.)` = pop_mil) +
    (`PIB per capita (R$ mil)` = pib_pc_mil) +
    (`Déficit de esgoto` = def_esgoto) +
    (`Dep. de transferências` = dep_transferencias) +
    (`Troca de gestão` = troca_gestao) +
    (`Idade do prefeito` = idade_prefeito) +
    (`Prefeita mulher` = prefeita_mulher) +
    (`Prefeito com superior` = prefeito_superior) +
    (`Prefeito aliado` = aliado) ~
    (`Média` = Mean) + (`DP` = SD) + (`Mín.` = Min) +
    (`Mediana` = Median) + (`Máx.` = Max),
  data = subbase,
  fmt = 2,
  title = "Estatísticas descritivas da base de análise",
  notes = "Fonte: FINBRA, Tesouro Nacional, IBGE e TSE. Elaboração própria.",
  output = "tabelas/tabela_01_descritivas_subbase.tex"
)

# 3. COMPARANDO AS BASES -------------------------------------------------------
## Criando as variáveis
base$ln_pib_pc <- log(base$pib_pc)
base$ln_pop <- log(base$pop)
base$na_analise <- ifelse(base$cod_ibge %in% subbase$cod_ibge, 1, 0)

## Proporção de municípios dentro ou fora da análise
table(base$na_analise)

## Diferença de médias por regressão
### Regressão da variável sobre a dummy na_analise: a constante é a média de
### quem está fora, e o coeficiente da dummy é a diferença de médias
variaveis <- c("ln_pib_pc", "ln_pop", "def_esgoto")
nomes <- c("PIB per capita (em log)", "População (em log)", "Déficit de esgoto")

comparacao <- data.frame()

for (i in 1:3) {
  modelo <- lm(reformulate("na_analise", variaveis[i]), data = base)
  teste <- coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

  dif <- teste["na_analise", "Estimate"]
  ep  <- teste["na_analise", "Std. Error"]
  p   <- teste["na_analise", "Pr(>|t|)"]

  ### Estrelas: * p<0,10; ** p<0,05; *** p<0,01
  estrelas <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                     ifelse(p < 0.10, "*", "")))

  media_fora    <- coef(modelo)[1]
  media_analise <- coef(modelo)[1] + dif

  linha_dif <- c(nomes[i],
                 formatC(media_analise, format = "f", digits = 2),
                 formatC(media_fora, format = "f", digits = 2),
                 paste0(formatC(dif, format = "f", digits = 2), estrelas),
                 ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
  linha_ep <- c("", "", "",
                paste0("(", formatC(ep, format = "f", digits = 2), ")"), "")

  comparacao <- rbind(comparacao, linha_dif, linha_ep)
}

### Número de municípios em cada grupo
comparacao <- rbind(comparacao,
                    c("N", sum(base$na_analise == 1), sum(base$na_analise == 0), "", ""))
names(comparacao) <- c("Variável", "Análise", "Fora", "Diferença", "p-valor")

## Exportando
datasummary_df(
  comparacao[, c("Variável", "Análise", "Fora", "Diferença")],
  title = "Comparação entre municípios",
  notes = c("Erros-padrão robustos à heteroscedasticidade entre parênteses. *, ** e *** indicam, respectivamente, significância a 10%, 5% e 1%. Fonte: IBGE. Elaboração própria."),
  output = "tabelas/tabela_02_comparacao_bases.tex")

# 4. DESCRITIVAS DA SUBBASE: CAPAG E INVESTIMENTO ------------------------------
ler_mapa <- function(tipo) {
  url <- paste0("https://github.com/ipea/geobr_prep_data/releases/download/",
                "v2.0.0/", tipo, "_2020_simplified.parquet")
  arquivo <- tempfile(fileext = ".parquet")
  download.file(url, arquivo, mode = "wb", quiet = TRUE)
  mapa <- as.data.frame(arrow::read_parquet(arquivo))
  ### A geometria vem em formato WKB; o sf converte para polígonos
  mapa$geometry <- st_as_sfc(structure(mapa$geometry, class = "WKB"), crs = 4674)
  st_sf(mapa)
}

## 4.1 Proporção de CAPAG alta por região e por UF -----------------------------
### O 1º dígito do código IBGE é a região (1 = Norte, 2 = Nordeste,
### 3 = Sudeste, 4 = Sul, 5 = Centro-Oeste)
subbase$code_region <- as.numeric(substr(subbase$cod_ibge, 1, 1))

### Médias de capag_alta (= proporção de municípios com CAPAG alta)
prop_regiao <- subbase %>%
  group_by(code_region) %>%
  summarise(prop_capag_alta = mean(capag_alta))
prop_uf <- subbase %>%
  group_by(uf) %>%
  summarise(prop_capag_alta = mean(capag_alta))

### Juntando com os mapas
regioes <- ler_mapa("regions")
estados <- ler_mapa("states")
mapa_regioes <- left_join(regioes, prop_regiao, by = "code_region")
mapa_estados <- left_join(estados, prop_uf, by = c("abbrev_state" = "uf"))

### Escala de cores comum (0 a 1) para os dois mapas serem comparáveis
escala_capag <- scale_fill_distiller(palette = "Greens", direction = 1,
                                     limits = c(0, 1), na.value = "grey90",
                                     labels = scales::percent,
                                     name = "% CAPAG\nalta")

### Mapa por região
mapa_a <- ggplot(mapa_regioes) +
  geom_sf(aes(fill = prop_capag_alta), color = "white") +
  geom_sf_text(aes(label = paste0(name_region, "\n",
                                  scales::percent(prop_capag_alta, accuracy = 1))),
               size = 3) +
  escala_capag +
  labs(title = "Por região") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

### Mapa por UF (linhas pretas: fronteiras das regiões). O DF não tem
### municípios na base, então fica cinza e sem rótulo
mapa_b <- ggplot() +
  geom_sf(data = mapa_estados, aes(fill = prop_capag_alta), color = "white") +
  geom_sf(data = regioes, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf_text(data = filter(mapa_estados, !is.na(prop_capag_alta)),
               aes(label = abbrev_state), size = 2.5) +
  escala_capag +
  labs(title = "Por UF") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

### Lado a lado
plot_grid(mapa_a, mapa_b, nrow = 1, rel_widths = c(0.8, 1))

### Salvar
ggsave("graficos/grafico_01_mapa_capagalta.png",
       width = 12, height = 6, dpi = 300, bg = "white")

## 4.2 Investimento relativo por nota de CAPAG ---------------------------------
### Notas em ordem: "n.d." e "n.e." primeiro e depois da pior para a melhor
### (sem isso, o R ordena em ordem alfabética)
subbase$capag <- factor(subbase$capag,
                        levels = c("n.d.", "n.e.", "D", "C", "B", "B+", "A", "A+"))

table(subbase$capag)  # Municípios da subbase em cada nota

ggplot(subbase, aes(x = capag, y = investimento_relativo, fill = capag)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 0.25)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("grey55", "grey75", "firebrick", "tomato",
                               "gold", "yellowgreen", "limegreen", "forestgreen")) +
  labs(x = "Nota CAPAG", y = "Investimento líquido / RCL") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graficos/grafico_02_boxplot_investimento_por_capag.png",
       width = 8, height = 5, dpi = 300, bg = "white")

## 4.3 Histograma do investimento relativo -------------------------------------
### A distribuição tem cauda longa à direita: 99% dos municípios investem até
### cerca de 31% da RCL, mas o máximo passa de 100%. O coord_cartesian corta
### só a visualização em 40% (os poucos municípios acima disso continuam nos
### cálculos, apenas não aparecem no eixo). A linha tracejada é a mediana.
summary(subbase$investimento_relativo)          # Resumo do investimento / RCL
sum(subbase$investimento_relativo > 0.4)        # Municípios fora do eixo (> 40%)

ggplot(subbase, aes(x = investimento_relativo)) +
  geom_histogram(binwidth = 0.01, boundary = 0,
                 fill = "steelblue", color = "white") +
  geom_vline(xintercept = median(subbase$investimento_relativo),
             linetype = "dashed") +
  coord_cartesian(xlim = c(0, 0.4)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Investimento líquido / RCL", y = "Número de municípios") +
  theme_minimal()

ggsave("graficos/grafico_03_histograma_investimento.png",
       width = 8, height = 5, dpi = 300, bg = "white")
