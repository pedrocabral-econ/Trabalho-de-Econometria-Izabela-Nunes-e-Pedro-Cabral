# TRABALHO DE ECONOMETRIA
# IZABELA NUNES E PEDRO CABRAL

# REGRESSÕES E ANÁLISES

# 1. IMPORTANDO PACOTES E BASE -------------------------------------------------
## Importando os pacotes
library(AER)
library(modelsummary)
library(tinytable)

## Lendo a base
base <- read.csv("base.csv")

## Criando as variáveis necessárias
base$investimento_relativo <- base$investimento_liquido / base$rcl
base$capag_alta <- ifelse(is.na(base$capag), NA,
                          ifelse(base$capag %in% c("A+", "A", "B+", "B"), 1, 0))
base$pib_pc <- base$pib / base$pop

## Criando a base utilizada na análise
subbase <- base[complete.cases(base), ]

# 2. REGRESSÃO PRINCIPAL -------------------------------------------------------
## 2.1 Sem Controles
reg1 <- lm(investimento_relativo ~ capag_alta, data = subbase)

coeftest(reg1, vcov. = vcovHC, type = "HC1")

## 2.2. Com características do município
reg2 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias,
    data = subbase)

coeftest(reg2, vcov. = vcovHC, type = "HC1")

## 2.3. Com características do prefeito
reg3 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior,
    data = subbase)

coeftest(reg3, vcov. = vcovHC, type = "HC1")

## 2.4. Com contexto político
reg4 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado,
    data = subbase)

coeftest(reg4, vcov. = vcovHC, type = "HC1")

## 2.5. Com necessidade de infraestrutura
reg5 <- lm(investimento_relativo ~ capag_alta +
    as.factor(uf) + log(pop) + log(pib_pc) + dep_transferencias +
    idade_prefeito + prefeita_mulher + prefeito_superior +
    troca_gestao + aliado +
    def_esgoto,
    data = subbase)

coeftest(reg5, vcov. = vcovHC, type = "HC1")

# 3. TABELA DE REGRESSÕES ------------------------------------------------------
options("modelsummary_format_numeric_latex" = "plain")

## 3.1 Linhas que indicam quais controles entram em cada coluna
linhas_controles <- data.frame(
  term = c("Características do município", "Características do prefeito",
           "Contexto político", "Necessidade de infraestrutura"),
  m1 = c("Não", "Não", "Não", "Não"),
  m2 = c("Sim", "Não", "Não", "Não"),
  m3 = c("Sim", "Sim", "Não", "Não"),
  m4 = c("Sim", "Sim", "Sim", "Não"),
  m5 = c("Sim", "Sim", "Sim", "Sim")
)
names(linhas_controles) <- c("term", "(1)", "(2)", "(3)", "(4)", "(5)")

## 3.2 Gerando a tabela
modelos <- list("(1)" = reg1, "(2)" = reg2, "(3)" = reg3,
                "(4)" = reg4, "(5)" = reg5)

tabela <- modelsummary(
  modelos,
  vcov = "HC1",
  coef_map = c("capag_alta" = "CAPAG Alta"),
  gof_map = list(
    list(raw = "r.squared", clean = "$R^2$", fmt = 2),
    list(raw = "nobs", clean = "Observações", fmt = 0)
  ),
  add_rows = linhas_controles,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = 3,
  escape = FALSE,
  output = "tinytable",
  title = "Associação entre CAPAG alta e investimento relativo",
  notes = c("Erros-padrão robustos à heterocedasticidade entre parênteses. *, ** e *** indicam significância a 10\\%, 5\\% e 1\\%, respectivamente.",
            "Fonte: FINBRA, Tesouro Nacional, IBGE e TSE. Elaboração própria.")
)

tabela <- group_tt(tabela, j = list("Investimento Relativo" = 2:6))
tabela <- style_tt(tabela, i = 4, line = "b", line_width = 0.1)

save_tt(tabela, "tabelas/tabela_03_regressoes.tex", overwrite = TRUE)

# 4. TABELA DO MODELO COMPLETO -------------------------------------------------
tabela2 <- modelsummary(
  list("(1)" = reg5),
  vcov = "HC1",
  coef_map = c("capag_alta" = "CAPAG Alta",
               "def_esgoto" = "Déficit de esgoto",
               "troca_gestao" = "Troca de gestão",
               "aliado" = "Aliado"),
  gof_map = list(
    list(raw = "r.squared", clean = "$R^2$", fmt = 2),
    list(raw = "nobs", clean = "Observações", fmt = 0)
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = 3,
  escape = FALSE,
  output = "tinytable",
  title = "Coeficientes principais do modelo completo",
  notes = c("Erros-padrão robustos à heterocedasticidade entre parênteses. *, ** e *** indicam significância a 10\\%, 5\\% e 1\\%, respectivamente.",
            "Fonte: FINBRA, Tesouro Nacional, IBGE e TSE. Elaboração própria.")
)

tabela2 <- group_tt(tabela2, j = list("Investimento Relativo" = 2))

save_tt(tabela2, "tabelas/tabela_04_coeficientes_principais.tex", overwrite = TRUE)

# 5. TESTE DE HOMOSCEDASTICIDADE
bptest(reg5)
