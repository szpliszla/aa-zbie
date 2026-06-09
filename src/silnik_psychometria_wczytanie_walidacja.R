# ============================================================================
# FUNKCJE POMOCNICZE - RAPORT PSYCHOMETRYCZNY
# ============================================================================
# ============================================================================
# ŁADOWANIE PAKIETÓW
# ============================================================================

load_required_packages <- function() {
  required_packages <- c(
    "psych",
    "mirt",
    "ggplot2",
    "dplyr",
    "tidyr",
    "knitr",
    "kableExtra",
    "corrplot",
    "writexl",
    "scales",
    "reshape2"
  )

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org")
    }
    library(pkg, character.only = TRUE)
  }

  if (!requireNamespace("sirt", quietly = TRUE)) {
    install.packages("sirt", dependencies = TRUE, repos = "https://cran.r-project.org")
  }
  library(sirt)

  cat("Wszystkie pakiety zaladowane pomyslnie.\n")
}

# ============================================================================
# WCZYTYWANIE DANYCH
# ============================================================================

read_psych_data <- function(data_path = NULL) {

  if (is.null(data_path)) {
    data_path <- file.choose()
  }

  file_ext <- tolower(tools::file_ext(data_path))

  raw_data <- switch(
    file_ext,

    "csv" = {
      d <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

      if (ncol(d) == 1) {
        d2 <- read.csv2(data_path, stringsAsFactors = FALSE, check.names = FALSE)

        if (ncol(d2) > ncol(d)) {
          d <- d2
        }
      }

      d
    },

    "dta" = {
      if (requireNamespace("foreign", quietly = TRUE)) {
        foreign::read.dta(data_path)
      } else if (requireNamespace("haven", quietly = TRUE)) {
        as.data.frame(haven::read_dta(data_path))
      } else {
        stop("Zainstaluj pakiet 'foreign' lub 'haven' do wczytania pliku .dta")
      }
    },

    "rds" = readRDS(data_path),

    "xlsx" = {
      if (requireNamespace("readxl", quietly = TRUE)) {
        as.data.frame(readxl::read_excel(data_path))
      } else {
        stop("Zainstaluj pakiet 'readxl' do wczytania pliku .xlsx")
      }
    },

    stop(paste("Nieobslugiwany format pliku:", file_ext))
  )

  return(raw_data)
}

# ============================================================================
# IDENTYFIKACJA ITEMOW
# ============================================================================

identify_item_columns <- function(raw_data, item_prefix, exclude_items = NULL) {

  item_cols <- grep(paste0("^", item_prefix), names(raw_data), value = TRUE)

  if (length(item_cols) == 0) {
    stop(paste0(
      "BLAD: Nie znaleziono zadnych kolumn z prefiksem '", item_prefix, "'.\n",
      "Dostepne kolumny: ", paste(head(names(raw_data), 20), collapse = ", "),
      "\nSprawdz parametr 'item_prefix'."
    ))
  }

  item_nums <- suppressWarnings(as.numeric(sub(paste0("^", item_prefix), "", item_cols)))

  if (all(!is.na(item_nums))) {
    item_cols <- item_cols[order(item_nums)]
  }

  if (is.null(exclude_items)) {
    exclude_items <- character(0)
  }

  exclude_items <- as.character(exclude_items)
  exclude_items <- exclude_items[exclude_items != ""]

  if (length(exclude_items) > 0) {
    item_cols <- setdiff(item_cols, exclude_items)

    cat(
      "### Recznie wykluczone itemy\n\n",
      paste(exclude_items, collapse = ", "),
      "\n\n"
    )
  }

  return(item_cols)
}

# ============================================================================
# WALIDACJA DANYCH
# ============================================================================

validate_items_data <- function(raw_data, item_cols) {

  validation_issues <- list()
  validation_warnings <- list()

  items_data <- raw_data[, item_cols, drop = FALSE]

  col_types <- sapply(items_data, class)
  non_numeric <- names(col_types[!col_types %in% c("numeric", "integer")])

  if (length(non_numeric) > 0) {
    cat("**UWAGA:** Nastepujace kolumny nie sa numeryczne i zostana skonwertowane:\n\n")

    for (col in non_numeric) {
      cat("-", col, "(typ:", col_types[col], ")\n")
      items_data[[col]] <- as.numeric(as.character(items_data[[col]]))
    }

    validation_warnings <- c(
      validation_warnings,
      list(paste("Skonwertowano", length(non_numeric), "kolumn na typ numeryczny"))
    )

    cat("\n")
  }

  value_check <- sapply(items_data, function(x) {
    vals <- unique(x[!is.na(x)])
    all(vals %in% c(0, 1))
  })

  non_binary <- names(value_check[!value_check])

  if (length(non_binary) > 0) {
    cat("**BLAD:** Nastepujace itemy zawieraja wartosci inne niz 0/1:\n\n")

    for (col in non_binary) {
      vals <- sort(unique(items_data[[col]][!is.na(items_data[[col]])]))
      cat("-", col, ": wartosci =", paste(vals, collapse = ", "), "\n")
    }

    cat("\n**UWAGA:** Itemy z wartosciami innymi niz 0/1 zostana wykluczone z analiz.\n\n")

    item_cols <- setdiff(item_cols, non_binary)
    items_data <- items_data[, item_cols, drop = FALSE]

    validation_issues <- c(
      validation_issues,
      list(paste(
        "Wykluczono", length(non_binary), "nie-binarnych itemow:",
        paste(non_binary, collapse = ", ")
      ))
    )
  }

  item_vars <- sapply(items_data, function(x) var(x, na.rm = TRUE))
  zero_var_items <- names(item_vars[item_vars == 0 | is.na(item_vars)])

  if (length(zero_var_items) > 0) {
    cat("**UWAGA:** Itemy z zerowa wariancja zostana wykluczone:\n\n")
    cat(paste(zero_var_items, collapse = ", "), "\n\n")

    item_cols <- setdiff(item_cols, zero_var_items)
    items_data <- items_data[, item_cols, drop = FALSE]

    validation_issues <- c(
      validation_issues,
      list(paste("Wykluczono", length(zero_var_items), "itemow z zerowa wariancja"))
    )
  }

  return(list(
    item_cols = item_cols,
    items_data = items_data,
    validation_issues = validation_issues,
    validation_warnings = validation_warnings
  ))
}

# ============================================================================
# WYKRYWANIE WERSJI TESTU
# ============================================================================

detect_test_versions <- function(raw_data, items_data, item_cols, version_var = NULL) {

  has_versions <- FALSE
  detected_versions <- NULL
  version_items <- list()

  if (!is.null(version_var) && version_var %in% names(raw_data)) {

    has_versions <- TRUE
    detected_versions <- sort(unique(raw_data[[version_var]]))
    raw_data$detected_version <- raw_data[[version_var]]

    for (v in detected_versions) {
      v_rows <- which(raw_data$detected_version == v)
      v_data <- items_data[v_rows, , drop = FALSE]
      v_items <- item_cols[colMeans(is.na(v_data)) < 0.50]
      version_items[[as.character(v)]] <- v_items
    }

  } else if (any(is.na(items_data))) {

    missing_pattern <- apply(is.na(items_data), 1, function(x) paste(as.integer(x), collapse = ""))
    pattern_counts <- sort(table(missing_pattern), decreasing = TRUE)
    major_patterns <- names(pattern_counts[pattern_counts >= nrow(items_data) * 0.05])

    if (length(major_patterns) > 1) {

      raw_data$detected_version <- match(missing_pattern, major_patterns)
      raw_data$detected_version[is.na(raw_data$detected_version)] <- 0

      detected_versions <- sort(unique(raw_data$detected_version[raw_data$detected_version > 0]))

      for (v in detected_versions) {
        v_rows <- which(raw_data$detected_version == v)
        v_data <- items_data[v_rows, , drop = FALSE] #fix: ] zamiast ).
v_items <- item_cols[colMeans(is.na(v_data)) < 0.50]
version_items[[as.character(v)]] <- v_items
      }

      has_versions <- TRUE

    } else {
      has_versions <- FALSE
    }

  } else {
    has_versions <- FALSE
  }

  return(list(
    raw_data = raw_data,
    has_versions = has_versions,
    detected_versions = detected_versions,
    version_items = version_items
  ))
}
