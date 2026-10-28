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
resultados/     saídas já geradas dos testes T1-T8 e N1-N12 (CSV, PNG) e logs resumidos
artigo/         manuscrito LaTeX (classe SelfArx da RITA), bibliografia e figuras
  artigo_eleicoes_2026_RITA.tex      arquivo principal (compilar este)
  secao_fundamentos.tex              fundamentos matemáticos e estatísticos, incluído por \input
  secoes_maquina_testes_extras.tex   seções N1-N12, incluídas por \input
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
- N1, N2, N4-N12: executados nos dados reais dos dois turnos (resultados `resultados/N*`).
  O processamento dos arquivos de urna (script 09) levou 143 min com 15 processos.
  N3 (boletins impressos x publicados) depende de amostra de campo e não foi executado.
- Pendências conhecidas:
  - N10: refazer com mais permutações (com 199, o menor p possível é 0,005 e não sobrevive à correção BH).
  - N11: reajustar o modelo sem a variável do último dígito (provável causa da segunda moda em ~0,75).
  - `classificar_locais_t6.R`: o padrão `"LAR "` também casa com "ESCOLAR ".
  - Figuras geradas pelos scripts R estão com rótulos em português.
- `artigo/Figures/fig_t8_*.pdf`: geradas por `scripts/gerar_figuras_t8.R` a partir de `resultados/T8_*.csv`.
- `artigo/Figures/fig_n4_hourly.pdf`: gerada a partir de `resultados/N4_votos_por_hora.csv`.
- `artigo/Figures/fig_n6_n7_coef.pdf`, `fig_n8_uf.pdf`, `fig_n9_foci.pdf`, `fig_n10_moran.pdf`, `fig_n11_importance.pdf`:
  geradas por `scripts/gerar_figuras_n6_n11.py` (Python, pandas e matplotlib) a partir de `resultados/N*.csv`.
- Para compilar o artigo: `pdflatex`, `bibtex`, `pdflatex`, `pdflatex` (requer os pacotes `abntex2cite` e `multibib`).

## Licenças e créditos

- `scripts/urna_vendor/`: [DataUrnas-BR](https://github.com/alissonlinneker/DataUrnas-BR), de Alisson Linneker,
  versão 0.2.2 (commit `12f0433`), licença MIT (ver `LICENSE` e `LEIAME.txt` na pasta).
- `artigo/SelfArx.cls`: classe do template da RITA.
