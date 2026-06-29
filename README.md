# Automatyzacja analiz w ZBIE

Celem projektu jest stworzenie narzędzia do samodzielnego przeprowadzania analiz 
(analiza psychometryczna, analiza RCT) na danych z badań edukacyjnych.

# Jak uruchomić projekt?

Projekt korzysta z systemu `renv`do izolacji środowiska. Aby go uruchomić na nowym komputerze:

1. Sklonuj to repozytorium na swój komputer.
2. Otwórz plik projektu `aa-zbie.Rproj` w RStudio.
3. Zainstaluj pakiet `devtools`:
   ```r
   install.packages('devtools')
   ```
4. W konsoli RStudio wpisz komendę
   ```r
   devtools::install()
   ```
5. Wygeneruj raport za pomocą komendy:
   ```r
   rmarkdown::render(
    'reports/psychometria_raport.Rmd',
    params = list(
      data_path = "ścieżka do pliku z danymi",
      item_prefix = "prefiks itemów",
      group_var = "zmienna grupująca",
      id_var = "id_ucznia",
      dif_group_var = "grupa"
    )
  )
  ```
  gdzie:
  * `item_prefix` opisać
  * `group_var` opisać
  * `id_var` opisać
  * `dif_group_var` opisać

# Struktura projektu

```
├── aa-zbie.Rproj
├── data
│   └── math_data.csv
├── renv
│   ├── activate.R
│   ├── library
│   │   └── windows
│   ├── settings.json
│   └── staging
├── renv.lock
├── reports
│   └── psychometria_raport.Rmd
└── R
│   ├── silnik_psychometria_analizy.R
│   └── silnik_psychometria_wczytanie_walidacja.R
└── tests
```
# Wersja programu 

*R version 4.6.0 (2026-04-24 ucrt)*
