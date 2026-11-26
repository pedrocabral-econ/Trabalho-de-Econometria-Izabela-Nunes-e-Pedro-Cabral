# TRABALHO DE ECONOMETRIA
# IZABELA NUNES E PEDRO CABRAL

# LIMPEZA DAS BASES DE DADOS

# 1. INVESTIMENTO LIQUIDADO (FINBRA 2025) --------------------------------------
## Vamos extrair, para cada município, nome, código IBGE, UF e o valor de
## 4.4.00.00.00 - Investimentos na categoria Despesas Liquidadas.

## Lendo a base
### As 3 primeiras linhas são metadados; a 4ª tem os nomes das colunas.
despesas <- read.csv2(
  "01. finbra - despesas orçamentárias.csv",
  skip = 3,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

## Filtrando a conta de Investimentos em Despesas Liquidadas
investimento <- despesas[
  despesas$Coluna == "Despesas Liquidadas" &
    despesas$Conta == "4.4.00.00.00 - Investimentos",
]

## Mantendo só as colunas necessárias
investimento <- data.frame(
  municipio            = investimento$Instituição,
  cod_ibge             = investimento$Cod.IBGE,
  uf                   = investimento$UF,
  investimento_liquido = investimento$Valor
)

## Incluindo municípios que estão na base de despesas, mas não registraram
## nada na conta de investimento
sem_conta <- despesas[
  !duplicated(despesas$Cod.IBGE) &
    !(despesas$Cod.IBGE %in% investimento$cod_ibge),
]

sem_conta <- data.frame(
  municipio            = sem_conta$Instituição,
  cod_ibge             = sem_conta$Cod.IBGE,
  uf                   = sem_conta$UF,
  investimento_liquido = 0
)

investimento <- rbind(investimento, sem_conta)

## Conferências
nrow(investimento)                            # Número total de municípios
sum(duplicated(investimento$cod_ibge))        # Número de municípios duplicados
sum(is.na(investimento$investimento_liquido)) # Valores ausentes

## Retirando as bases brutas
rm(sem_conta)
rm(despesas)


# 2. RECEITA CORRENTE LÍQUIDA (FINBRA 2025) ------------------------------------
## Vamos extrair, para cada município, nome, código IBGE, UF e o valor de
## RECEITA CORRENTE LÍQUIDA (III) = (I - II) na categoria
## TOTAL (ÚLTIMOS 12 MESES).

## Lendo a base
### As 5 primeiras linhas são metadados; a 6ª tem os nomes das colunas.
receita <- read.csv2(
  "02. finbra - receita.csv",
  skip = 5,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

## Filtrando a RCL acumulada dos últimos 12 meses
rcl <- receita[
  receita$Conta == "RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
    receita$Coluna == "TOTAL (ÚLTIMOS 12 MESES)",
]

## Mantendo só as colunas necessárias
rcl <- data.frame(
  municipio = rcl$Instituição,
  cod_ibge  = rcl$Cod.IBGE,
  uf        = rcl$UF,
  rcl       = rcl$Valor
)

## Nesse caso, todos os municípios da base têm registro de RCL.

## Da mesma base, extraímos também (sempre no TOTAL (ÚLTIMOS 12 MESES)):
## - RECEITAS CORRENTES (I): receita corrente total
## - Transferências Correntes: FPM, cota-parte do ICMS, FUNDEB etc.
## Com elas medimos a dependência de transferências de cada município:
## dep_transferencias = transferências correntes / receitas correntes
## (parcela da receita corrente que vem de outros entes: mede a falta de
## autonomia financeira). Usamos as receitas correntes brutas, e não a RCL, no
## denominador porque a RCL desconta deduções (como a contribuição ao FUNDEB),
## o que poderia levar o indicador a passar de 1. Assim ele fica entre 0 e 1.
receita <- receita[receita$Coluna == "TOTAL (ÚLTIMOS 12 MESES)", ]

### Receita corrente total
receita_corrente <- receita[receita$Conta == "RECEITAS CORRENTES (I)", ]
receita_corrente <- data.frame(
  cod_ibge         = receita_corrente$Cod.IBGE,
  receita_corrente = receita_corrente$Valor
)

### Transferências correntes
transferencias <- receita[receita$Conta == "Transferências Correntes", ]
transferencias <- data.frame(
  cod_ibge                 = transferencias$Cod.IBGE,
  transferencias_correntes = transferencias$Valor
)

### Calculando a dependência de transferências e juntando à base de RCL
dependencia <- merge(receita_corrente, transferencias, by = "cod_ibge")
dependencia$dep_transferencias <-
  dependencia$transferencias_correntes / dependencia$receita_corrente
rcl <- merge(rcl, dependencia[, c("cod_ibge", "dep_transferencias")],
             by = "cod_ibge", all.x = TRUE)

## Conferências
nrow(rcl)                       # Número total de municípios
sum(duplicated(rcl$cod_ibge))   # Número de municípios duplicados
colSums(is.na(rcl))             # Valores ausentes

## Retirando as bases brutas
rm(receita)
rm(receita_corrente)
rm(transferencias)
rm(dependencia)


# 3. CAPAG 2024 ----------------------------------------------------------------
## Vamos extrair, para cada município, nome, código IBGE e a nota do CAPAG
## calculada com base no ano de 2023, divulgada em 2024. Também extraímos a UF,
## que será usada como controle.

## Lendo a base
### Utilizamos apenas a aba "CAPAG Ano Base 2023".
### As 3 primeiras linhas são cabeçalhos; a 4ª tem os nomes das colunas.
library(readxl)
capag_bruta <- read_excel(
  "03. tesouro - capag.xlsx",
  sheet = "CAPAG Ano Base 2023",
  skip = 3,
  col_types = "text",
  .name_repair = "minimal"
)

## Filtrando o CAPAG e os outros dados
n_col <- ncol(capag_bruta)
capag <- data.frame(
  cod_ibge  = capag_bruta[[1]],
  municipio = capag_bruta[[2]],
  uf        = capag_bruta[[3]],
  capag     = capag_bruta[[n_col]]
)

## Conferências
nrow(capag)                       # Número total de municípios
sum(duplicated(capag$cod_ibge))   # Número de municípios duplicados
sum(is.na(capag$capag))           # Valores ausentes

## Retirando a base bruta
rm(capag_bruta)
rm(n_col)


# 4. PIB MUNICIPAL (IBGE 2023) -------------------------------------------------
## Vamos extrair, para cada município do Brasil, o código IBGE e o PIB a
## preços correntes de 2023 (mais recente disponível).
## Essa base contém todos os municípios, e será usada para testar se os
## municípios que ficaram fora da análise (sem dados) diferem dos que entraram.

## Lendo a base
pib <- read.csv(
  "04. ibge - pib.csv",
  stringsAsFactors = FALSE
)

## Mantendo só as colunas necessárias
pib <- data.frame(
  cod_ibge = as.numeric(pib$cod_ibge),
  pib      = as.numeric(pib$pib) * 1000
)

## Conferências
nrow(pib)                      # Número total de municípios
sum(duplicated(pib$cod_ibge))  # Número de municípios duplicados
sum(is.na(pib$pib))            # Valores ausentes


# 5. POPULAÇÃO MUNICIPAL (IBGE, CENSO 2022) ------------------------------------
## Vamos extrair, para cada município do Brasil, o código IBGE e a população
## residente do Censo 2022.
## Essa base contém todos os municípios, e será usada para testar se os
## municípios que ficaram fora da análise (sem dados) diferem dos que entraram.

## Lendo a base
populacao <- read.csv(
  "05. ibge - populacao.csv",
  stringsAsFactors = FALSE
)

## Mantendo só as colunas necessárias
populacao <- data.frame(
  cod_ibge = as.numeric(populacao$cod_ibge),
  pop      = as.numeric(populacao$populacao)
)

## Conferências
nrow(populacao)                     # Número total de municípios
sum(duplicated(populacao$cod_ibge)) # Número de municípios duplicados
sum(is.na(populacao$pop))           # Valores ausentes

sum(!(populacao$cod_ibge %in% pib$cod_ibge))  # População sem PIB
sum(!(pib$cod_ibge %in% populacao$cod_ibge))  # PIB sem população


# 6. SANEAMENTO (IBGE, CENSO 2022) ---------------------------------------------
## Vamos extrair, para cada município do Brasil, o déficit de esgotamento
## sanitário, usado como proxy da necessidade de infraestrutura.
## Essa base contém todos os municípios, e será usada para testar se os
## municípios que ficaram fora da análise (sem dados) diferem dos que entraram.

## Lendo a base
saneamento <- read.csv(
  "06. ibge - esgotamento.csv",
  stringsAsFactors = FALSE
)

## O SIDRA usa "-" para indicar zero absoluto: nenhum domicílio do município
## tem esgotamento adequado. Trocamos por 0 antes de transformar em número.
saneamento$domicilios_rede[saneamento$domicilios_rede == "-"] <- "0"

## Mantendo só as colunas necessárias
### def_esgoto = parcela dos domicílios SEM esgotamento adequado (entre 0 e 1).
### Esgotamento adequado = rede geral, rede pluvial ou fossa ligada à rede.
### Usamos o déficit (e não a cobertura) para que o sinal esperado seja
### positivo: quanto maior a necessidade, maior o investimento esperado.
saneamento <- data.frame(
  cod_ibge   = as.numeric(saneamento$cod_ibge),
  def_esgoto = 1 - as.numeric(saneamento$domicilios_rede) /
                   as.numeric(saneamento$domicilios_total)
)

## Conferências
nrow(saneamento)                      # Número total de municípios
sum(duplicated(saneamento$cod_ibge))  # Número de municípios duplicados
sum(is.na(saneamento$def_esgoto))     # Valores ausentes


# 7. PREFEITOS (TSE, ELEIÇÕES 2020 E 2024) -------------------------------------
## Vamos identificar, para cada município, se houve troca de gestão: ou seja,
## se o prefeito eleito em 2024 (que governa em 2025, ano do investimento) não
## faz parte da gestão anterior (prefeito ou vice eleitos em 2020).
## Também guardamos características do prefeito eleito em 2024.

## 7.1 Leitura e filtro dos eleitos --------------------------------------------
### Lendo as bases
cand_2024 <- read.csv2(
  "07. tse - candidatos 2024.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)
cand_2020 <- read.csv2(
  "08. tse - candidatos 2020.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

### Filtrando os eleitos: prefeitos 2024 e 2020 e vices 2020.
prefeitos_2024 <- cand_2024[
  cand_2024$CD_CARGO == 11 & cand_2024$DS_SIT_TOT_TURNO == "ELEITO",
]
prefeitos_2020 <- cand_2020[
  cand_2020$CD_CARGO == 11 & cand_2020$DS_SIT_TOT_TURNO == "ELEITO",
]
vices_2020 <- cand_2020[
  cand_2020$CD_CARGO == 12 & cand_2020$DS_SIT_TOT_TURNO == "ELEITO",
]

### Alguns municípios tiveram eleição suplementar e aparecem duas vezes.
### Em 2024, ficamos com a eleição mais antiga (a ordinária, que define quem
### assumiu em 2025). Em 2020, ficamos com a mais recente (se houve eleição
### suplementar entre 2021 e 2024, quem governava no fim do mandato era o
### eleito nela).
data_2024 <- as.Date(prefeitos_2024$DT_ELEICAO, format = "%d/%m/%Y")
data_2020 <- as.Date(prefeitos_2020$DT_ELEICAO, format = "%d/%m/%Y")
data_vice <- as.Date(vices_2020$DT_ELEICAO, format = "%d/%m/%Y")

prefeitos_2024 <- prefeitos_2024[order(data_2024), ]
prefeitos_2020 <- prefeitos_2020[order(data_2020, decreasing = TRUE), ]
vices_2020 <- vices_2020[order(data_vice, decreasing = TRUE), ]

prefeitos_2024 <- prefeitos_2024[!duplicated(prefeitos_2024$SG_UE), ]
prefeitos_2020 <- prefeitos_2020[!duplicated(prefeitos_2020$SG_UE), ]
vices_2020 <- vices_2020[!duplicated(vices_2020$SG_UE), ]

## 7.2 Base de prefeitos -------------------------------------------------------
### Partimos dos prefeitos de 2024 e acrescentamos, pelo código do TSE, o
### título eleitoral (que identifica a pessoa) do prefeito e do vice de 2020.
### Além disso, coletamos informações como:
### - idade_prefeito: em anos completos na data da posse (01/01/2025).
### - prefeita_mulher = 1 se a prefeita é mulher.
### - prefeito_superior = 1 se o prefeito tem ensino superior completo.
### - aliado = 1 se o prefeito eleito em 2024 é de partido aliado ao governo
###   federal (PT, PC do B ou PV, que formam a Federação Brasil da Esperança).
nascimento <- as.Date(prefeitos_2024$DT_NASCIMENTO, format = "%d/%m/%Y")

prefeitos <- data.frame(
  cod_tse           = prefeitos_2024$SG_UE,
  titulo_2024       = prefeitos_2024$NR_TITULO_ELEITORAL_CANDIDATO,
  idade_prefeito    = floor(as.numeric(as.Date("2025-01-01") - nascimento) /
                              365.25),
  prefeita_mulher   = ifelse(prefeitos_2024$DS_GENERO == "FEMININO", 1, 0),
  prefeito_superior = ifelse(
    prefeitos_2024$DS_GRAU_INSTRUCAO == "SUPERIOR COMPLETO", 1, 0
  ),
  aliado            = ifelse(
    prefeitos_2024$SG_PARTIDO %in% c("PT", "PC do B", "PV"), 1, 0
  )
)

prefeitos$titulo_2020 <- prefeitos_2020$NR_TITULO_ELEITORAL_CANDIDATO[
  match(prefeitos$cod_tse, prefeitos_2020$SG_UE)]
prefeitos$titulo_vice_2020 <- vices_2020$NR_TITULO_ELEITORAL_CANDIDATO[
  match(prefeitos$cod_tse, vices_2020$SG_UE)]

## 7.3 Variável de troca de gestão ---------------------------------------------
### troca_gestao = 1 se o prefeito de 2025 não é o prefeito nem o vice eleitos
###                em 2020 (a gestão que estava no cargo até o fim de 2024).
### troca_gestao = 0 se é o mesmo prefeito (reeleito) ou o vice de 2020 (o vice
###                participou do governo, então há continuidade administrativa).
### Usar o vice de 2020 também cobre o vice que assumiu no meio do mandato.
prefeitos$troca_gestao <- ifelse(
  prefeitos$titulo_2024 == prefeitos$titulo_2020 |
    prefeitos$titulo_2024 == prefeitos$titulo_vice_2020,
  0, 1
)

## 7.4 Código IBGE -------------------------------------------------------------
### O TSE usa um código próprio para os municípios. A tabela de
### correspondência liga o código do TSE ao código do IBGE.
codigos <- read.csv(
  "09. tse - codigos municipios tse-ibge.csv",
  stringsAsFactors = FALSE
)
codigos <- data.frame(
  cod_tse  = codigos$codigo_tse,
  cod_ibge = codigos$codigo_ibge
)

sum(!(prefeitos$cod_tse %in% codigos$cod_tse))
prefeitos <- merge(prefeitos, codigos, by = "cod_tse")
prefeitos <- prefeitos[, c("cod_ibge", "troca_gestao", "idade_prefeito",
                           "prefeita_mulher", "prefeito_superior", "aliado")]

### Conferências
nrow(prefeitos)                       # Número total de municípios
sum(duplicated(prefeitos$cod_ibge))   # Número de municípios duplicados
sum(is.na(prefeitos$troca_gestao))    # Valores ausentes

### Retirando as bases brutas
rm(cand_2024)
rm(cand_2020)
rm(prefeitos_2024)
rm(prefeitos_2020)
rm(vices_2020)
rm(nascimento)
rm(data_2024)
rm(data_2020)
rm(data_vice)
rm(codigos)


# 8. BASE ÚNICA ----------------------------------------------------------------
## Vamos juntar investimento, RCL e CAPAG pelo código IBGE.
## Além disso, incluímos os outros dados para funcionarem como controles.

## 8.1 Conferência de dados ausentes -------------------------------------------
### Passando o Código IBGE para número nas 3 bases
investimento$cod_ibge <- as.numeric(investimento$cod_ibge)
rcl$cod_ibge <- as.numeric(rcl$cod_ibge)
capag$cod_ibge <- as.numeric(capag$cod_ibge)

### Conferindo municípios que estão em uma base e não nas outras
sum(!(investimento$cod_ibge %in% rcl$cod_ibge))    # investimento sem RCL
sum(!(rcl$cod_ibge %in% investimento$cod_ibge))    # RCL sem investimento
sum(!(investimento$cod_ibge %in% capag$cod_ibge))  # investimento sem CAPAG
sum(!(capag$cod_ibge %in% investimento$cod_ibge))  # CAPAG sem investimento
sum(!(rcl$cod_ibge %in% capag$cod_ibge))           # RCL sem CAPAG
sum(!(capag$cod_ibge %in% rcl$cod_ibge))           # CAPAG sem RCL
sum(investimento$cod_ibge %in% rcl$cod_ibge &
      investimento$cod_ibge %in% capag$cod_ibge)   # Municípios nas 3 bases

## 8.2 Junção ------------------------------------------------------------------
### Primeiro, juntamos as bases de investimento e RCL.
### Os dados de UF vêm da base de investimento.
base <- merge(
  investimento[, c("cod_ibge", "uf", "investimento_liquido")],
  rcl[, c("cod_ibge", "rcl", "dep_transferencias")],
  by = "cod_ibge"
)

### Agora, juntamos com a base de CAPAG.
### O nome do município vem da base de CAPAG.
base <- merge(
  base,
  capag[, c("cod_ibge", "municipio", "capag")],
  by = "cod_ibge"
)

### Ordenando as colunas
base <- base[, c("cod_ibge", "municipio", "uf",
                 "investimento_liquido", "rcl", "capag",
                 "dep_transferencias")]

### Conferências
nrow(base)                      # Número total de municípios
sum(duplicated(base$cod_ibge))  # Número de municípios duplicados
colSums(is.na(base))            # Valores ausentes

## 8.3 PIB, população, saneamento e municípios fora da análise -----------------
### Para testar se os municípios que ficaram fora da análise diferem dos que
### entraram, acrescentamos o PIB, a população e o déficit de esgotamento
### (disponíveis para todos) e os municípios que faltam. Esses municípios ficam
### com NA nas demais variáveis.
base <- merge(base, pib, by = "cod_ibge", all = TRUE)
base <- merge(base, populacao, by = "cod_ibge", all = TRUE)
base <- merge(base, saneamento, by = "cod_ibge", all = TRUE)

### Conferências
nrow(base)                      # Número total de municípios
sum(is.na(base$rcl))            # Municípios fora da análise

## 8.4 Prefeitos ---------------------------------------------------------------
### Acrescentamos a variável de troca de gestão e as características do
### prefeito eleito em 2024 (inclusive se é de partido aliado ao governo).
base <- merge(
  base,
  prefeitos[, c("cod_ibge", "troca_gestao", "idade_prefeito",
                "prefeita_mulher", "prefeito_superior", "aliado")],
  by = "cod_ibge",
  all.x = TRUE
)

### Salvando a base final como csv
write.csv(base, "base.csv", row.names = FALSE)
