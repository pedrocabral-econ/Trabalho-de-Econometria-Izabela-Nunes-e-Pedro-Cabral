# Capacidade de Pagamento e Investimento Municipal: evidências para o Brasil
Izabela Nunes e Pedro Cabral

## Resumo

Os municípios brasileiros são responsáveis pela provisão de serviços públicos, mas enfrentam dificuldades para financiar obras de infraestrutura com recursos próprios. Este artigo investiga a relação entre acesso ao crédito e investimento municipal, utilizando como medida a Capacidade de Pagamento (CAPAG), nota do Tesouro Nacional que determina a elegibilidade à garantia da União em operações de crédito. Com dados de 5.465 municípios, estima-se, por mínimos quadrados ordinários, a relação entre a nota divulgada em 2024 e o investimento de 2025 como proporção da receita corrente líquida, controlando por características municipais, dos prefeitos, contexto político e necessidade de infraestrutura. Municípios com CAPAG alta investem cerca de 1 ponto percentual a mais, resultado robusto a diferentes especificações. O artigo também analisa a concessão da garantia e identifica etapas discricionárias que permitem atrasos potencialmente motivados por fatores políticos, embora o alinhamento partidário com o Presidente não seja estatisticamente significante.

## Dados

Todas as bases são de fontes públicas e estão na raiz do repositório, numeradas na ordem em que são usadas.

| Arquivo | Fonte | Conteúdo |
|---|---|---|
| `01. finbra - despesas orçamentárias.csv` | Siconfi | Despesas orçamentárias municipais |
| `02. finbra - receita.csv` | Siconfi | Receitas municipais |
| `03. tesouro - capag.xlsx` | Tesouro Nacional | Classificação de Capacidade de Pagamento (CAPAG) |
| `04. ibge - pib.csv` | IBGE | PIB municipal |
| `05. ibge - populacao.csv` | IBGE | População municipal |
| `06. ibge - esgotamento.csv` | IBGE | Déficit de esgotamento sanitário |
| `07. tse - candidatos 2024.csv` | TSE | Candidatos às eleições municipais de 2024 |
| `08. tse - candidatos 2020.csv` | TSE | Candidatos às eleições municipais de 2020 |
| `09. tse - codigos municipios tse-ibge.csv` | TSE | Tabela de correspondência entre códigos TSE e IBGE |

A base final, resultado do tratamento e do cruzamento dessas fontes, é o arquivo `base.csv`.

## Estrutura do repositório

```
├── 01. a 09. (bases brutas)    # Dados originais, conforme a tabela acima
├── Limpeza.R                   # Tratamento das bases e construção da base.csv
├── Descritivas.R               # Estatísticas descritivas, gráficos e tabelas
├── Regressões.R                # Estimação dos modelos principais
├── Robustez.R                  # Testes de robustez
├── base.csv                    # Base final usada nas análises
├── IA.md                       # Links das conversas com IA usadas no trabalho
├── graficos/                   # Gráficos gerados pelos scripts
├── tabelas/                    # Tabelas geradas pelos scripts
└── README.md
```

## Como reproduzir

Execute os scripts em R na ordem abaixo, com a raiz do repositório como diretório de trabalho:

1. `Limpeza.R`: lê as bases brutas e gera o `base.csv`.
2. `Descritivas.R`: produz as estatísticas descritivas.
3. `Regressões.R`: estima os modelos principais.
4. `Robustez.R`: roda as verificações de robustez.

Os gráficos e tabelas são salvos nas pastas `graficos/` e `tabelas/`.