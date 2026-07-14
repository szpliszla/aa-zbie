# Automatyzacja analiz w ZBIE

Celem projektu jest stworzenie narzędzia do samodzielnego przeprowadzania analiz 
(analiza psychometryczna, analiza RCT) na danych z badań edukacyjnych.

# Jak uruchomić projekt?

Projekt korzysta z systemu `renv`do izolacji środowiska. Aby go uruchomić na nowym komputerze:

1. Zainstaluj pakiet `pak`:
   ```r
   install.packages('pak')
   ```
2. Zaintaluj ten pakiet. W konsoli R wykonaj:
   ```r
   pak::pkg_install('szpliszla/aa-zbie')
   ```
3. Wygeneruj raport za pomocą komendy:
   ```r
   aazbie::render_report(
      "ścieżka pod którą zapisany zostanie raport",
      "ścieżka do pliku z danymi",
      "prefiks itemów",
      "zmienna grupująca",
      "zmienna z id_ucznia",
      "zmienna grupująca dla diff-ów"
   )
   ```
   Dokładny opis parametrów można znaleźć w dokumentacji funkcji `aazbie::render_report`
   (aby ją zobaczyć, wykonaj w konsoli R `?aazbie::render_report`).

# Struktura projektu

```
├── aa-zbie.Rproj
├── data
│   └── math_data.rda
├── inst
│   └── extdata
│       └── math_data.csv
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
