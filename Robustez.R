# TRABALHO DE ECONOMETRIA
# IZABELA NUNES E PEDRO CABRAL

# TESTES DE ROBUSTEZ

# 1. IMPORTANDO PACOTES E BASE -------------------------------------------------
## Importando os pacotes
library(AER)
library(modelsummary)
library(tinytable)

## Lendo a base
base <- read.csv("base.csv")

## Criando as variáveis necessárias (mesmas do Regressões.R)
base$investimento_relativo <- base$investimento_liquido / base$rcl
base$capag_alta <- ifelse(is.na(base$capag), NA,
                          ifelse(base$capag %in% c("A+", "A", "B+", "B"), 1, 0))
base$pib_pc <- base$pib / base$pop

## Criando a base utilizada na análise
subbase <- base[complete.cases(base), ]

# 2. MODELO DE REFERÊNCIA ------------------------------------------------------
reg_ref <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado +
    def_esgoto,
    data = subbase)

coeftest(reg_ref, vcov. = vcovHC, type = "HC1")

# 3. EXCLUINDO OUTLIERS DO INVESTIMENTO RELATIVO -------------------------------
## Definindo os cortes (percentis 1 e 99)
cortes <- quantile(subbase$investimento_relativo, probs = c(0.01, 0.99))

## Base sem outliers
subbase_1 <- subbase[subbase$investimento_relativo >= cortes[1] &
                           subbase$investimento_relativo <= cortes[2], ]

## Modelo completo na base sem outliers
reg_1 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado +
    def_esgoto,
    data = subbase_1)

coeftest(reg_1, vcov. = vcovHC, type = "HC1")

## Comparando o coeficiente de capag_alta com o modelo de referência
coeftest(reg_ref, vcov. = vcovHC, type = "HC1")["capag_alta", ]
coeftest(reg_1, vcov. = vcovHC, type = "HC1")["capag_alta", ]

# 4. EXCLUINDO CAPAG "n.d." e "n.e."  ------------------------------------------
## Base sem valores ausentes
subbase_2 <- subbase[subbase$capag %in% c("A+", "A", "B+", "B", "C", "D"), ]

## Modelo completo na base sem valores ausentes
reg_2 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado +
    def_esgoto,
    data = subbase_2)

coeftest(reg_2, vcov. = vcovHC, type = "HC1")

## Comparando o coeficiente de capag_alta com o modelo de referência
coeftest(reg_ref, vcov. = vcovHC, type = "HC1")["capag_alta", ]
coeftest(reg_2, vcov. = vcovHC, type = "HC1")["capag_alta", ]

# 5. OUTRA FORMA FUNCIONAL: LOG(INVESTIMENTO PER CAPITA) -----------------------
## Criando o investimento per capita
subbase$investimento_pc <- subbase$investimento_liquido / subbase$pop

## Excluindo o município com zero
subbase_3 <- subbase[subbase$investimento_pc > 0, ]

## Modelo completo com log(investimento per capita)
reg_3 <- lm(log(investimento_pc) ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado +
    def_esgoto,
    data = subbase_3)

coeftest(reg_3, vcov. = vcovHC, type = "HC1")

## Coeficiente de capag_alta (as unidades diferem do modelo de referência)
coeftest(reg_3, vcov. = vcovHC, type = "HC1")["capag_alta", ]

# 6. TABELA DOS TESTES DE ROBUSTEZ ---------------------------------------------
options("modelsummary_format_numeric_latex" = "plain")

## 6.1 Linhas que indicam o que muda em cada coluna
linhas_robustez <- data.frame(
  term = c("Amostra"),
  m1 = c("Completa"),
  m2 = c("Sem outliers"),
  m3 = c("Sem n.d./n.e."),
  m4 = c("Inv. pc > 0")
)
names(linhas_robustez) <- c("term", "(1)", "(2)", "(3)", "(4)")

## 6.2 Gerando a tabela
modelos_robustez <- list("(1)" = reg_ref, "(2)" = reg_1,
                         "(3)" = reg_2, "(4)" = reg_3)

tabela_rob <- modelsummary(
  modelos_robustez,
  vcov = "HC1",
  coef_map = c("capag_alta" = "CAPAG Alta"),
  gof_map = list(
    list(raw = "r.squared", clean = "$R^2$", fmt = 2),
    list(raw = "nobs", clean = "Observações", fmt = 0)
  ),
  add_rows = linhas_robustez,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = 3,
  escape = FALSE,
  output = "tinytable",
  title = "Testes de robustez",
  notes = c("Erros-padrão robustos à heterocedasticidade entre parênteses. *, ** e *** indicam significância a 10\\%, 5\\% e 1\\%, respectivamente.",
            "A coluna (1) é o modelo de referência. Todas as colunas incluem os mesmos controles e efeito fixo de UF.",
            "Fonte: FINBRA, Tesouro Nacional, IBGE e TSE. Elaboração própria.")
)

tabela_rob <- group_tt(tabela_rob, j = list("Investimento Relativo" = 2:4,
                                            "Log Inv. per capita" = 5))

save_tt(tabela_rob, "tabelas/tabela_05_robustez.tex", overwrite = TRUE)
