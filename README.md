# Automatyzacja analiz w ZBIE

Celem projektu jest stworzenie narzędzia do samodzielnego przeprowadzania analiz (analiza psychometryczna, analiza RCT) na danych z badań edukacyjnych.

# Jak uruchomić projekt?

Projekt korzysta z systemu `renv` do izolacji środowiska. Aby go uruchomić na nowym komputerze:

1.  Zainstaluj pakiet `pak`:

    ``` r
    install.packages('pak')
    ```

2.  Zaintaluj ten pakiet. W konsoli R wykonaj:

    ``` r
    pak::pkg_install('szpliszla/aa-zbie')
    ```

3.  Wygeneruj raport za pomocą komendy:

    ``` r
    aazbie::render_report(
       "ścieżka pod którą zapisany zostanie raport",
       "ścieżka do pliku z danymi",
       "prefiks itemów",
       "zmienna grupująca",
       "zmienna z id_ucznia",
       "zmienna grupująca dla diff-ów"
       )
    ```

Aby raport działał poprawnie, dane muszą spełnić określone wymagania.

## Format danych

- **Plik CSV** (kodowanie UTF-8, separator: przecinek lub średnik)
- **Plik XLSX** (format Microsoft Excel)
- **Plik DTA** (format Stata)
- **Plik RDS** (format R)

## Struktura danych

Dane muszą być w formacie **szerokim** (wide format) – każdy wiersz to jedna osoba badana, każda kolumna to jedna zmienna.

| Kolumna | Opis | Wymagane |
|------------------------|------------------------|------------------------|
| Zmienne identyfikujące (ID) | ID osoby, ID szkoły itp. | Opcjonalne |
| **Itemy testowe** | Odpowiedzi na zadania testu | **TAK** |
| Zmienna grupująca | Np. grupa eksperymentalna/kontrolna, płeć | Opcjonalne (wymagane dla DIF) |
| Wersja testu | Różne wersje/formularze testu | Opcjonalne |

## Kodowanie odpowiedzi

- Itemy muszą być zakodowane jako **0/1** (dychotomicznie)
  - `0` = odpowiedź błędna
  - `1` = odpowiedź poprawna
  - `NA` = brak odpowiedzi (dozwolone)
- Wszystkie itemy testowe muszą mieć **wspólny prefiks** w nazwie (np. `mat_`, `item_`, `q_`)

## Parametry funkcji `render_report()`

- `output_path`: ścieżka, pod którą zapisany zostanie raport
- `data_path`: ścieżka do pliku z danymi
- `item_prefix`: prefiks nazw kolumn z itemami (np. `"mat_"`)
- `id_var`: nazwa zmiennej z identyfikatorem osoby – opcjonalna
- `group_var`: zmienna grupująca (np. grupa eksperymentalna/kontrolna) – opcjonalna
- `dif_group_var`: zmienna do analizy DIF (np. `"grupa"`, `"płeć"`) – opcjonalna
- `exclude_items`: lista itemów do wykluczenia z analiz (np. `c("mat_5", "mat_12")`) – opcjonalna
- `version_var`: nazwa zmiennej z wersją testu – opcjonalna (jeśli `NULL`, uruchamiana jest automatyczna heurystyka)
- `alpha_threshold`: próg rzetelności Alfa Cronbacha (domyślnie `0.70`)
- `discrimination_min`: minimalna akceptowalna moc dyskryminacyjna itemu (domyślnie `0.30`)

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
