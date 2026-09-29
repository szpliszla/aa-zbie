# Automatyzacja analiz w ZBIE

Pakiet R do automatycznej analizy psychometrycznej danych testowych:
CTT, IRT (modele binarne i politomiczne), item fit, DIF, eksport
wyników do Excel i raport HTML.

# Jak uruchomić projekt?

1.  Zainstaluj pakiet `pak`:

    ``` r
    install.packages('pak')
    ```

2.  Zainstaluj ten pakiet. W konsoli R wykonaj:

    ``` r
    pak::pkg_install('szpliszla/aa-zbie')
    ```

3.  Wygeneruj raport za pomocą komendy:

    ``` r
    aazbie::render_report(
      output_path = "raport.html",
      data_path   = "dane.csv",
      item_prefix = "mat_"
    )
    ```

    Pełny przykład z opcjonalnymi parametrami:

    ``` r
    aazbie::render_report(
      output_path   = "raport.html",
      data_path     = "dane.csv",
      item_prefix   = "mat_",
      group_var     = "grupa",
      id_var        = "id_ucznia",
      dif_group_var = "plec",
      version_var   = "nr_zeszytu"
    )
    ```

Aby raport działał poprawnie, dane muszą spełnić określone wymagania.

## Format danych

- **Plik CSV** (kodowanie UTF-8, separator: przecinek lub średnik)
- **Plik XLSX** (format Microsoft Excel)
- **Plik DTA** (format Stata)
- **Plik RDS** (format R)

## Struktura danych

Dane muszą być w formacie **szerokim** (wide format) — każdy wiersz
to jedna osoba badana, każda kolumna to jedna zmienna.

| Kolumna | Opis | Wymagane |
|---|---|---|
| **Itemy testowe** | Odpowiedzi na zadania testu | **TAK** |
| Zmienne identyfikujące (ID) | ID osoby, ID szkoły itp. | Opcjonalne |
| Zmienna grupująca | Np. grupa eksperymentalna/kontrolna, płeć | Opcjonalne (wymagane dla DIF) |
| Wersja testu | Różne wersje/formularze testu | Opcjonalne |

## Kodowanie odpowiedzi

- Itemy mogą być zakodowane **binarnie** lub **politomicznie**:
  - `0` = odpowiedź błędna
  - `1` = odpowiedź poprawna (lub częściowo poprawna)
  - `2`, `3`, ... = wyższe kategorie punktowe (partial credit)
  - `NA` = brak odpowiedzi (dozwolone)
- Dla itemów politomicznych (np. 0/1/2) pakiet automatycznie dobiera
  modele partial credit (PCM, GPCM) zamiast binarnych (1PL, 2PL, 3PL).
- Wszystkie itemy testowe muszą mieć **wspólny prefiks** w nazwie
  (np. `mat_`, `item_`, `zad_`).

## Parametry funkcji `render_report()`

Trzy pierwsze parametry są wymagane, pozostałe opcjonalne.

| Parametr | Domyślnie | Opis |
|---|---|---|
| `output_path` | — | Ścieżka do pliku raportu HTML |
| `data_path` | — | Ścieżka do pliku z danymi |
| `item_prefix` | — | Prefiks nazw kolumn z itemami (np. `"mat_"`) |
| `group_var` | `NULL` | Zmienna grupująca (np. grupa eksperymentalna/kontrolna) |
| `id_var` | `NULL` | Zmienna z identyfikatorem osoby |
| `dif_group_var` | `NULL` | Zmienna do analizy DIF (np. `"plec"`) |
| `exclude_items` | `NULL` | Wektor itemów do wykluczenia (np. `c("mat_5")`) |
| `version_var` | `NULL` | Zmienna z wersją testu; gdy `NULL`, wersje wykrywane automatycznie |
| `unified_irt` | `TRUE` | Wspólna kalibracja IRT dla wielu wersji (FIML) |
| `min_pattern_prop` | `0.05` | Minimalny udział wzorca braków, by uznać go za wersję |
| `item_missing_max_prop` | `0.90` | Maks. dopuszczalny udział braków w itemie per wersja |
| `warn_unclassified_prop` | `0.05` | Próg ostrzeżenia o nieprzypisanych obserwacjach |
| `max_unclassified_prop` | `0.20` | Próg wyłączenia automatycznego podziału na wersje |
| `alpha_threshold` | `0.70` | Próg rzetelności alfa Cronbacha |
| `discrimination_min` | `0.30` | Minimalny próg mocy dyskryminacyjnej itemu |
| `dif_method` | `"logistic"` | Metoda analizy DIF |

### Wspólna kalibracja IRT

Gdy dane zawierają wiele wersji testu (zeszyty rotowane), domyślnie
(`unified_irt = TRUE`) IRT jest estymowany na pełnej macierzy danych
— `mirt` obsługuje braki strukturalne przez FIML. Dzięki temu
parametry itemów są estymowane na całej próbie zamiast na małych
podgrupach per zeszyt.

CTT i sekwencyjna eliminacja nadal biegną osobno per wersja.


## Orientacyjny czas renderowania

Zmierzone na Windows 10, R 4.6, dane binarne bez braków strukturalnych.

| Osoby | Itemy | Czas |
|-------|-------|------|
| 100 | 15 | ~4 s |
| 500 | 30 | ~6 s |
| 500 | 50 | ~8 s |
| 1000 | 30 | ~4 s |

Problemy z wydajnością mogą wystąpić przy danych z dużą liczbą braków
strukturalnych (np. projekt rotowany z >50 itemów). W takim przypadku
warto podać `version_var` lub użyć `unified_irt = TRUE` (domyślne).

# Struktura projektu

```
├── R/
│   ├── render_report.R
│   ├── silnik_psychometria_analizy.R
│   └── silnik_psychometria_wczytanie_walidacja.R
├── inst/
│   ├── extdata/
│   │   ├── math_data.csv
│   │   ├── mixed_data.csv
│   │   └── mixed_data_params.csv
│   └── reports/
│       └── psychometria_raport.Rmd
├── tests/
│   └── testthat/
│       ├── test_end2end.R
│       └── test_poprawki.R
├── DESCRIPTION
├── NAMESPACE
└── README.md
```
