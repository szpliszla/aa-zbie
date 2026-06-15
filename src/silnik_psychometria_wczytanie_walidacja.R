# ============================================================================
# FUNKCJE POMOCNICZE - RAPORT PSYCHOMETRYCZNY
# ============================================================================
# ============================================================================
# ŁADOWANIE PAKIETÓW
# ============================================================================

library(haven)
library(readxl)
library(here)

# ============================================================================
# WCZYTYWANIE DANYCH
# ============================================================================

#'@title Wczytanie danych psychometrycznych do analizy
#'
#'@description Proszę wczytać dane w jednym z obsługiwanych formatów:
#'CSV, DTA, RDS, XLS lub XLSX.
#'@param data_path ścieźka do pliku danych.
#'@return Ramka danych ('data.frame').
#'@export

read_psych_data <- function(data_path) {

  if (
    missing(data_path) ||
    is.null(data_path) ||
    !is.character(data_path) ||
    length(data_path) != 1 ||
    data_path == ""
  ) {
    stop(
      "Argument 'data_path' musi być pojedynczą, niepustą ścieżką do pliku.",
      call. = FALSE
    )
  }

  if (!file.exists(data_path)) {
    stop(
      paste0("Nie znaleziono pliku: ", data_path),
      call. = FALSE
    )
  }

  file_ext <- tolower(tools::file_ext(data_path))

  if (file_ext == "") {
    stop(
      paste0("Plik nie ma rozszerzenia: ", data_path),
      call. = FALSE
    )
  }

  raw_data <- switch(
    file_ext,

    "csv" = {
      data_csv <- read.csv(
        data_path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (ncol(data_csv) == 1) {
        data_csv2 <- read.csv2(
          data_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        if (ncol(data_csv2) > ncol(data_csv)) {
          data_csv <- data_csv2
        }
      }

      data_csv
    },

    "dta" = {
      as.data.frame(haven::read_dta(data_path))
    },

    "rds" = {
      readRDS(data_path)
    },

    "xls" = {
      as.data.frame(readxl::read_excel(data_path))
    },

    "xlsx" = {
      as.data.frame(readxl::read_excel(data_path))
    },

    stop(
      paste0("Nieobsługiwany format pliku: ", file_ext),
      call. = FALSE
    )
  )

  if (!is.data.frame(raw_data)) {
    stop(
      "Wczytany obiekt nie jest ramką danych.",
      call. = FALSE
    )
  }

  as.data.frame(raw_data)
}

#raw_data <- read_psych_data(here("data", "math_data.csv"))

# ============================================================================
# IDENTYFIKACJA ITEMOW
# ============================================================================

#'@title Identyfikacja kolumn itemów
#'
#' @description Funkcja wyszukuje kolumny rozpoczynajace sie od wskazanego prefiksu.
#' Jeżeli itemy mają numeryczne końcówki, porządkuje je według numerów.
#'
#' @param raw_data Ramka danych
#' @param item_prefix Prefiks nazw kolumn z itemami.
#' @param exclude_items Opcjonalny wektor nazw itemów do wykluczenia.
#'
#' @return Wektor nazw kolumn z itemami
#'
#' @export


identify_item_columns <- function(raw_data, item_prefix, exclude_items = NULL) {
  if (!is.data.frame(raw_data)) {
    stop(
      "Argument 'raw_data' musi być ramką danych.",
      call. = FALSE
    )
  }

  if (
    missing(item_prefix) ||
    is.null(item_prefix) ||
    !is.character(item_prefix) ||
    length(item_prefix) != 1 ||
    item_prefix == ""
  ) {
    stop(
      "Argument 'item_prefix' musi być pojedynczym, niepustym ciągiem znaków.",
      call. = FALSE
    )
  }

item_cols <- names(raw_data)[startsWith(names(raw_data), item_prefix)]

  if (length(item_cols) == 0) {
    stop(paste0(
      "BLAD: Nie znaleziono zadnych kolumn z prefiksem '", item_prefix, "'.\n",
      "Dostepne kolumny: ", paste(head(names(raw_data), 20), collapse = ", "),
      "\nSprawdz parametr 'item_prefix'."
    ),
    call. = FALSE)
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
  }

  item_cols
}

#item_cols <- identify_item_columns(raw_data, item_prefix = "mat_")


# ============================================================================
# WALIDACJA DANYCH
# ============================================================================

#' @title Walidacja itemów testowych
#'
#' @description Sprawdza typ danych, binarność itemów oraz wariancję
#'
#' @param raw_data Ramka danych
#' @param item_cols Wektor nazw kolumn z itemami

#' @return Lista z informacjami o problemach walidacyjnych
#' @export

validate_items_data <- function(raw_data, item_cols) {

  if (!is.data.frame(raw_data)) {
    stop(
      "Argument 'raw_data' musi być ramką danych.",
      call. = FALSE
    )
  }

  if (
    missing(item_cols) ||
    is.null(item_cols) ||
    !is.character(item_cols) ||
    length(item_cols) == 0
  ) {
    stop(
      "Argument 'item_cols' musi być ciągiem znaków o długości większej niż 0.",
      call. = FALSE
    )
  }

  missing_cols <- setdiff(item_cols, names(raw_data))

  if (length(missing_cols) > 0) {
    stop(
      paste0("Brakuje kolumn: ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  validation_issues <- list()
  validation_warnings <- list()

  items_data <- raw_data[, item_cols, drop = FALSE]

  col_types <- vapply(items_data,
                      function(x) class(x)[1],
                      character(1))

  non_numeric <- names(col_types[!col_types %in% c("numeric", "integer")])

  conversion_diagnostics <- data.frame(
    item = character(0),
    type_before = character(0),
    new_na_count = integer(0),
    stringsAsFactors = FALSE
  )

  if (length(non_numeric) > 0) {

    conversion_diagnostics <- data.frame(
      item = non_numeric,
      type_before = unname(col_types[non_numeric]),
      new_na_count = integer(length(non_numeric)),
      stringsAsFactors = FALSE
    )

    for (col in non_numeric) {
      na_before <- sum(is.na(items_data[[col]]))

      if (is.logical(items_data[[col]])) {
        items_data[[col]] <- as.numeric(items_data[[col]])
      } else {
        items_data[[col]] <- suppressWarnings(
          as.numeric(as.character(items_data[[col]]))
        )
      }

      na_after <- sum(is.na(items_data[[col]]))

      conversion_diagnostics$new_na_count[
        conversion_diagnostics$item == col
      ] <- na_after - na_before
    }

    validation_warnings <- c(
      validation_warnings,
      list(paste("Skonwertowano", length(non_numeric), "kolumn na typ numeryczny"))
    )
  }

  # Lukasz: dobre miejsce na "wpięcie się" z danymi kategorycznymi.

  value_check <- vapply(items_data, function(x) {
    vals <- unique(x[!is.na(x)])
    all(vals %in% c(0, 1))
  }, logical(1))

  non_binary <- names(value_check[!value_check])
  non_binary_values <- list()

  if (length(non_binary) > 0) {

    non_binary_values <- lapply(
      non_binary,
      function(col) {
        sort(unique(items_data[[col]][!is.na(items_data[[col]])]))
      }
    )

    names(non_binary_values) <- non_binary

    item_cols <- setdiff(item_cols, non_binary)
    items_data <- items_data[, item_cols, drop = FALSE]

    validation_issues <- c(
      validation_issues,
      list(
        paste0(
          "Wykluczono itemy niebinarne: ",
          paste(non_binary, collapse = ", ")
        )
      )
    )
  }

  if (length(item_cols) == 0) {
    stop(
      "Po wykluczeniu itemów niebinarnych nie pozostały żadne itemy do analizy.",
      call. = FALSE
    )
  }

  item_vars <- vapply(
    items_data,
    function(x) var(x, na.rm = TRUE),
    numeric(1)
  )

  zero_var_items <- names(item_vars[item_vars == 0 | is.na(item_vars)])

  if (length(zero_var_items) > 0) {

    item_cols <- setdiff(item_cols, zero_var_items)
    items_data <- items_data[, item_cols, drop = FALSE]

    validation_issues <- c(
      validation_issues,
      list(
        paste0(
          "Wykluczono itemy z zerową wariancją: ",
          paste(zero_var_items, collapse = ", ")
        )
      )
    )
  }

  if (length(item_cols) == 0) {
    stop(
      "Po walidacji nie pozostały żadne itemy do analizy.",
      call. = FALSE
    )
  }

  list(
    item_cols = item_cols,
    items_data = items_data,
    validation_issues = validation_issues,
    validation_warnings = validation_warnings,
    non_numeric_items = non_numeric,
    conversion_diagnostics = conversion_diagnostics,
    non_binary_items = non_binary,
    non_binary_values = non_binary_values,
    zero_variance_items = zero_var_items
  )
}

#wyniki_walidacji <- validate_items_data(raw_data, item_cols)


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
