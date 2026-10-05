# Automated psychometric analyses at ZBIE

An R package for automated psychometric analysis of test data: CTT,
IRT (binary and polytomous models), item fit, DIF, an HTML report and
an Excel file with the results.

# Getting started

1.  **Windows only: install Rtools.** Some dependencies may need to be
    compiled from source, and without Rtools the installation or the
    report rendering fails. Download the Rtools version that matches
    your R version from <https://cran.r-project.org/bin/windows/Rtools/>
    (e.g. R 4.5 and R 4.6 use Rtools45) and install it with the default
    settings. Restart RStudio and check the installation:

    ``` r
    install.packages("pkgbuild")
    pkgbuild::has_build_tools(debug = TRUE)
    ```

    The result should be `TRUE`. On macOS install the Xcode Command Line
    Tools (`xcode-select --install`); on Linux, the compilers (e.g. the
    `build-essential` package on Debian/Ubuntu).

2.  Install the `pak` package:

    ``` r
    install.packages("pak")
    ```

3.  Install this package. In the R console run:

    ``` r
    pak::pkg_install("szpliszla/aa-zbie")
    ```

4.  Generate a report:

    ``` r
    aazbie::render_report(
      output_path = "report.html",
      data_path   = "data.csv",
      item_prefix = "mat_"
    )
    ```

    A full example with optional parameters:

    ``` r
    aazbie::render_report(
      output_path   = "report.html",
      data_path     = "data.csv",
      item_prefix   = "mat_",
      group_var     = "group",
      id_var        = "student_id",
      dif_group_var = "gender",
      version_var   = "booklet"
    )
    ```

The data must meet the requirements below for the report to work
correctly.

## Data format

- **CSV file** (UTF-8 encoding, separator: comma or semicolon)
- **XLSX file** (Microsoft Excel format)
- **DTA file** (Stata format)
- **RDS file** (R format)

## Data structure

The data must be in **wide** format: each row is one test taker and
each column is one variable.

| Column | Description | Required |
|---|---|---|
| **Test items** | Responses to the test items | **YES** |
| Identifier variables (ID) | Person ID, school ID, etc. | Optional |
| Grouping variable | E.g. experimental/control group, gender | Optional (required for DIF) |
| Test version | Different versions/forms (booklets) of the test | Optional |

## Response coding

- Items can be coded **binary** or **polytomous**:
  - `0` = incorrect response
  - `1` = correct (or partially correct) response
  - `2`, `3`, ... = higher score categories (partial credit)
  - `NA` = missing response (allowed)
- For polytomous items (e.g. 0/1/2) the package automatically uses
  partial credit models (PCM, GPCM) instead of binary models (1PL,
  2PL, 3PL).
- All test items must share a **common prefix** in their names
  (e.g. `mat_`, `item_`, `zad_`).

## `render_report()` parameters

The first three parameters are required; the others are optional.

| Parameter | Default | Description |
|---|---|---|
| `output_path` | — | Path of the HTML report file |
| `data_path` | — | Path of the data file |
| `item_prefix` | — | Prefix of the item column names (e.g. `"mat_"`) |
| `group_var` | `NULL` | Grouping variable (e.g. experimental/control group) |
| `id_var` | `NULL` | Person identifier variable |
| `dif_group_var` | `NULL` | Variable used in the DIF analysis (e.g. `"gender"`) |
| `exclude_items` | `NULL` | Vector of items to exclude (e.g. `c("mat_5")`) |
| `version_var` | `NULL` | Test version variable; when `NULL`, versions are detected automatically |
| `unified_irt` | `TRUE` | Joint IRT calibration for multiple versions (FIML) |
| `min_pattern_prop` | `0.05` | Minimum share of a missing-data pattern to treat it as a version |
| `item_missing_max_prop` | `0.90` | Maximum share of missing responses in an item within a version |
| `warn_unclassified_prop` | `0.05` | Warning threshold for observations without a version |
| `max_unclassified_prop` | `0.20` | Threshold above which automatic version detection is switched off |
| `alpha_threshold` | `0.70` | Cronbach's alpha reliability threshold |
| `discrimination_min` | `0.30` | Minimum item discrimination |
| `dif_method` | `"logistic"` | Not used yet (see below) |
| `jezyk` | `"en"` | Report language: `"en"` or `"pl"` |

The DIF method is currently chosen automatically from the item types:
logistic regression (`sirt`, ETS A/B/C classification) for binary
items, and a multiple-group IRT model (`mirt`, 2PL for binary and GPCM
for polytomous items, likelihood ratio tests with the Holm correction)
for polytomous or mixed items. The `dif_method` parameter is reserved
for a future choice of method.

### Joint IRT calibration

When the data contain several test versions (rotated booklets), IRT is
by default (`unified_irt = TRUE`) estimated on the full data matrix —
`mirt` handles structurally missing responses with FIML. Item
parameters are therefore estimated on the whole sample instead of
small per-booklet subsamples.

CTT and sequential item elimination still run separately for each
version.

### Report language

By default the report is generated in English (`jezyk = "en"`); the
Polish version is produced with `jezyk = "pl"`. The report texts are
stored in `inst/reports/tlumaczenia.csv` (separator `;`, a `token`
column and one column per language). A new language can be added as a
new column named with its code.

## Results in Excel

Together with the report, the package saves an Excel file with the
results (CTT item statistics, IRT parameters, person scores, item fit,
DIF). The file is saved in the same folder as the HTML report and
named

```
<data file name>_results_<YYYY-MM-DD_HHMMSS>.xlsx
```

e.g. `math_data_results_2026-10-05_102819.xlsx`. The timestamp means
that subsequent renders (e.g. the English and Polish versions of a
report) do not overwrite earlier files. Column names and values are in
the report language.

## Approximate rendering time

Measured on Windows with R 4.6. Times depend on the machine and its
current load.

| Persons | Items | Data | Time |
|---|---|---|---|
| 100 | 15 | binary | ~4 s |
| 500 | 30 | binary | ~6 s |
| 500 | 50 | binary | ~8 s |
| 1000 | 30 | binary | ~4 s |
| ~4400 | 21 | binary, DIF | ~40–46 s |
| ~4400 | 21 | 17 binary + 4 polytomous, DIF | ~1.5–3 min |

With polytomous or mixed items most of the time is taken by the DIF
analysis in `mirt` (the model is refitted for each item). Performance
problems can also occur with many structurally missing responses
(e.g. a rotated design with more than 50 items); in that case specify
`version_var` or keep `unified_irt = TRUE` (the default).

# Project structure

```
├── .github/
│   └── workflows/
│       └── R-CMD-check.yaml        # R CMD check on GitHub Actions
├── R/
│   ├── helpers.R                   # table formatting and display: fmt_table(), show_table()
│   ├── render_report.R             # render_report(): entry point, renders the report template
│   ├── silnik_psychometria_analizy.R            # engine: CTT, IRT, item fit, DIF, Excel export
│   ├── silnik_psychometria_wczytanie_walidacja.R # engine: data loading, item identification, validation
│   └── tlumaczenia.R               # translations: wczytaj_tlumaczenia(), t(), t_kolumny(), t_logiczne()
├── inst/
│   ├── extdata/
│   │   ├── math_data.csv           # example binary data (separator ;)
│   │   ├── mixed_data.csv          # example mixed data: 10 binary + 5 polytomous items
│   │   └── mixed_data_params.csv   # parameters used to simulate mixed_data (incl. true DIF)
│   └── reports/
│       ├── psychometria_raport.Rmd # report template (structure, display and interpretation)
│       └── tlumaczenia.csv         # report texts: token;en;pl
├── man/                            # function documentation generated by roxygen2
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── helper-sim.R            # data simulation for tests
│       ├── helper-tlumaczenia.R    # polskie_frazy(): Polish phrases in English reports
│       ├── test_dif.R              # DIF (mirt path: 2PL + GPCM, GPCM)
│       ├── test_eksport.R          # Excel export: file name, translated columns and values
│       ├── test_end2end.R          # full reports on the example data
│       ├── test_helpers.R          # fmt_table(), show_table(), t_logiczne()
│       ├── test_item_fit_wersje.R  # item fit per test version
│       ├── test_poprawki.R         # regression tests for earlier fixes
│       └── test_tlumaczenia.R      # translation file and tokens used in the code
├── DESCRIPTION
├── LICENSE / LICENSE.md
├── NAMESPACE
├── README.md
├── aa-zbie.Rproj
└── renv.lock                       # package versions (renv)
```
