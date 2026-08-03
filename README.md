# Harmonização dos estoques de carbono orgânico do solo para 0–30 cm

Repositório associado à dissertação de **Delara Maryamo Ibraimo Cassamo**:

> *Estoques de carbono orgânico do solo na transição floresta–pastagem em diferentes unidades geoambientais da Amazônia brasileira.*

**Orientador:** Prof. Dr. Alessandro Samuel-Rosa  
**Coorientadora:** Profa. Dra. Taciara Zborowski Horst

## Código disponível

O script final de padronização, auditoria e harmonização dos estoques de COS está em:

```text
R/01_harmonizacao_COS_0_30/harmonizacao_COS_0_30.R
```

Versão do protocolo: **1.0.4**.

O código aplica, em ordem hierárquica, os procedimentos descritos na Seção 5.4.2 da dissertação:

1. valor direto de 0–30 cm;
2. soma de camadas contíguas;
3. soma de camadas contíguas com corte proporcional da última camada;
4. corte proporcional de uma camada acumulada;
5. extrapolação empírica da camada de 20–30 cm;
6. expansão empírica do estoque de 0–20 para 0–30 cm.

A interpolação entre estoques acumulados não integra a versão 1.0.4, pois não foi utilizada na harmonização dos perfis da base analisada.

## Dados de entrada

Os dados não estão incluídos nesta versão do repositório. Para executar o script com o caminho padrão, o arquivo deve ser nomeado:

```text
dados_harmonizacao_COS_0_30.csv
```

e colocado em:

```text
dados/entrada/
```

O arquivo deve conter os campos esperados pelo script, incluindo os identificadores dos perfis, profundidades inicial e final, estoques calculados ou reportados e os marcadores utilizados na auditoria e seleção dos dados.

## Execução

A partir da pasta principal do repositório, execute:

```bash
Rscript R/01_harmonizacao_COS_0_30/harmonizacao_COS_0_30.R
```

Também é possível informar caminhos alternativos para a entrada e a saída:

```bash
Rscript R/01_harmonizacao_COS_0_30/harmonizacao_COS_0_30.R caminho/entrada.csv caminho/saida
```

Pacotes necessários: `tidyverse`, `janitor` e `writexl`.

As saídas são gravadas, por padrão, em:

```text
resultados/tabelas/
```

## Estrutura do repositório

```text
dissertacao-COS-amazonia/
├── R/
│   ├── 01_harmonizacao_COS_0_30/
│   ├── 02_inventario_bibliografico/
│   ├── 03_esforco_amostral_profundidade/
│   ├── 04_classes_idade_maxima/
│   ├── 05_estoques_por_classe_idade/
│   ├── 06_areas_unidades_geoambientais/
│   ├── 07_diferencas_COS_por_UG/
│   ├── 08_trajetorias_temporais_por_UG/
│   └── 09_tabelas_e_controles/
├── dados/
│   ├── entrada/
│   └── processados/
├── resultados/
│   ├── figuras/
│   ├── tabelas/
│   └── logs/
├── docs/
├── CITATION.cff
├── LICENSE
└── README.md
```

Nesta versão, o script efetivamente incluído é o de harmonização dos estoques para 0–30 cm. As demais pastas organizam os códigos e produtos que poderão ser acrescentados ao repositório.

## Citação

As informações de autoria e citação estão no arquivo `CITATION.cff`.
