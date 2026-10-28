# Forense estatística da eleição presidencial de 2026 por seção eleitoral

Código, resultados e manuscrito do artigo *"Forense Estatística da Eleição Presidencial
Brasileira de 2026 por Seção Eleitoral: uma Aplicação da Metodologia CRISP-DM aos Dados
Abertos do TSE"* (submissão à RITA). Autor: César A. C. Moreira (PPCA/UnB).

## Estrutura

```
scripts/        pipeline em R (e Python chamado pelo R)
  00_config.R                     configuração e funções comuns
  01_ ... 05_                     testes T1-T8 (ver "Scripts 01 a 05" abaixo)
  06_baixar_dados_abertos_extra.R perfil do eleitorado, locais de votação, votação 2022
  07_testes_modelo_perfil_painel.R  novos testes 6 (modelo da urna), 7 (perfil), 8 (painel 2022-2026)
  08_testes_padroes_espaciais_ml.R  novos testes 9 (RKD), 10 (LISA), 11 (floresta aleatória), 12 (dip)
  09_arquivos_urna.R              prepara o Python (reticulate) e roda 09_arquivos_urna.py
  09_arquivos_urna.py             novos testes 1 (RDV x BU), 2 (assinatura), 4 (ritmo), 5 (biometria)
  10_testes_arquivos_urna.R       resumo dos testes 1, 2, 4 e 5
  99_executar_tudo.R              roda a sequência inteira, parando no primeiro erro
  urna_vendor/                    código de terceiros (DataUrnas-BR, MIT) para ler os arquivos de urna
resultados/     saídas já geradas dos testes T1-T8 (CSV, PNG) e logs resumidos
artigo/         manuscrito LaTeX (classe SelfArx da RITA), bibliografia e figuras
testes/         simulador de dados no layout do TSE e arquivos reais de urna de 2026 usados nos testes
```

## Como reproduzir

1. Clone o repositório numa pasta fora do OneDrive (ex.: `D:/dados/Estudo_eleicao`).
2. Abra o R nessa pasta (`setwd`) e rode `scripts/99_executar_tudo.R` a partir da pasta `scripts`.
3. Espaço em disco: ~6 GB para 01-05, ~15 GB para 06-08, ~200 GB para 09 (arquivos de urna dos dois turnos).

Os dados brutos do TSE **não** estão no repositório (vários GB); os scripts baixam tudo do
[Portal de Dados Abertos do TSE](https://dadosabertos.tse.jus.br/) e conferem a integridade.

## Onde rodar

Os scripts usam caminhos relativos (`tse_2026/`, `resultados/`): defina a pasta `scripts/`
como diretório de trabalho antes de rodar. Os resultados novos vão para `scripts/resultados/`.

## Situação

- T1-T8: executados nos dados reais (resultados em `resultados/`).
- Novos testes 1, 2 e 4-12: scripts prontos e validados com dados simulados e com arquivos reais
  de urna de 2026 (`testes/fixtures`); execução nos dados completos pendente.
- `artigo/`: as figuras `fig_t8_deciles.pdf` e `fig_t8_uf.pdf` citadas no .tex ainda precisam ser
  adicionadas a `artigo/Figures/`.

## Licenças e créditos

- `scripts/urna_vendor/`: DataUrnas-BR, de Alisson Linneker, licença MIT (ver `LICENSE` e `LEIAME.txt` na pasta).
- `artigo/SelfArx.cls`: classe do template da RITA.
