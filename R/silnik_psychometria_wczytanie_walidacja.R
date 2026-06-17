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

#' @title Wykrywanie wersji testu
#'
#' @description Wykrywa wersje testu z kolumny wersji lub ze wzorców braków danych.
#'
#' @param raw_data Ramka danych.
#' @param items_data Ramka danych z itemami.
#' @param item_cols Wektor nazw kolumn z itemami.
#' @param version_var Opcjonalna nazwa zmiennej z wersją testu.
#' @param min_pattern_prop Minimalny udział wzorca braków, aby uznać go za wersję.
#' @param item_missing_max_prop Maksymalny udział braków itemu w danej wersji.
#' @param detected_version_col Nazwa roboczej kolumny z wykrytą wersją.
#'
#' @return Lista z danymi, wersjami testu, itemami wersji i diagnostyką.
#'
#'
#' @export



detect_test_versions <- function(
    raw_data,
    items_data,
    item_cols,
    version_var = NULL,
    min_pattern_prop = 0.05,
    item_missing_max_prop = 0.90,
    detected_version_col = ".detected_version"
) {

  if (!is.data.frame(raw_data)) {
    stop("Argument 'raw_data' musi być ramką danych.", call. = FALSE)
  }

  if (!is.data.frame(items_data)) {
    stop("Argument 'items_data' musi być ramką danych.", call. = FALSE)
  }

  if (
    missing(item_cols) ||
    is.null(item_cols) ||
    !is.character(item_cols) ||
    length(item_cols) == 0
  ) {
    stop("Argument 'item_cols' musi być niepustym wektorem nazw kolumn.", call. = FALSE)
  }

  if (nrow(raw_data) != nrow(items_data)) {
    stop(
      "Argumenty 'raw_data' i 'items_data' muszą mieć tę samą liczbę wierszy.",
      call. = FALSE
    )
  }

  missing_item_cols <- setdiff(item_cols, names(items_data))

  if (length(missing_item_cols) > 0) {
    stop(
      paste0(
        "W 'items_data' brakuje następujących itemów: ",
        paste(missing_item_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (
    !is.null(version_var) &&
    (!is.character(version_var) || length(version_var) != 1 || version_var == "")
  ) {
    stop(
      "Argument 'version_var' musi być pojedynczą nazwą kolumny albo NULL.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(min_pattern_prop) ||
    length(min_pattern_prop) != 1 ||
    min_pattern_prop <= 0 ||
    min_pattern_prop >= 1
  ) {
    stop(
      "Argument 'min_pattern_prop' musi być pojedynczą liczbą z przedziału (0, 1).",
      call. = FALSE
    )
  }

  if (
    !is.numeric(item_missing_max_prop) ||
    length(item_missing_max_prop) != 1 ||
    item_missing_max_prop <= 0 ||
    item_missing_max_prop > 1
  ) {
    stop(
      "Argument 'item_missing_max_prop' musi być pojedynczą liczbą z przedziału (0, 1].",
      call. = FALSE
    )
  }

  if (
    !is.character(detected_version_col) ||
    length(detected_version_col) != 1 ||
    detected_version_col == ""
  ) {
    stop(
      "Argument 'detected_version_col' musi być pojedynczą nazwą kolumny.",
      call. = FALSE
    )
  }

  if (detected_version_col %in% names(raw_data)) {
    stop(
      paste0(
        "Kolumna '", detected_version_col,
        "' już istnieje w danych. Wybierz inną nazwę przez 'detected_version_col'."
      ),
      call. = FALSE
    )
  }

  items_matrix <- items_data[, item_cols, drop = FALSE]

  detection_method <- "none"
  detection_warnings <- character(0)
  pattern_counts <- NULL
  major_patterns <- NULL
  unclassified_rows <- integer(0)
  unclassified_patterns <- NULL

  version_vector <- rep(NA_character_, nrow(items_matrix))

  make_version_items <- function(version_vector, detected_versions) {
    version_items <- lapply(
      detected_versions,
      function(version) {
        version_rows <- which(
          as.character(version_vector) == as.character(version)
        )

        version_data <- items_matrix[version_rows, , drop = FALSE]
        item_missing_prop <- colMeans(is.na(version_data))

        item_cols[item_missing_prop <= item_missing_max_prop]
      }
    )

    names(version_items) <- detected_versions
    version_items
  }

  make_version_data <- function(raw_data, items_matrix, version_vector, detected_versions, version_items) {
    version_data <- lapply(
      detected_versions,
      function(version) {
        version_rows <- which(
          as.character(version_vector) == as.character(version)
        )

        version_item_cols <- version_items[[as.character(version)]]

        list(
          raw_data = raw_data[version_rows, , drop = FALSE],
          items_data = items_matrix[version_rows, version_item_cols, drop = FALSE],
          all_items_data = items_matrix[version_rows, , drop = FALSE],
          item_cols = version_item_cols,
          n_obs = length(version_rows),
          n_items = length(version_item_cols)
        )
      }
    )

    names(version_data) <- detected_versions
    version_data
  }

  if (!is.null(version_var) && version_var %in% names(raw_data)) {

    detection_method <- "version_var"

    version_vector <- as.character(raw_data[[version_var]])
    version_vector[!is.na(version_vector) & version_vector == ""] <- NA_character_

    unclassified_rows <- which(is.na(version_vector))

    if (length(unclassified_rows) > 0) {
      detection_warnings <- c(
        detection_warnings,
        paste0(
          "Zmienna wersji testu zawiera braki danych dla ",
          length(unclassified_rows),
          " obserwacji."
        )
      )
    }

  } else {

    if (!is.null(version_var) && !version_var %in% names(raw_data)) {
      detection_warnings <- c(
        detection_warnings,
        paste0(
          "Nie znaleziono zmiennej wersji testu: ",
          version_var,
          ". Podjęto próbę wykrycia wersji na podstawie braków danych."
        )
      )
    }

    if (any(is.na(items_matrix))) {

      missing_pattern <- apply(
        is.na(items_matrix),
        1,
        function(x) paste(as.integer(x), collapse = "")
      )

      pattern_counts <- sort(table(missing_pattern), decreasing = TRUE)

      major_patterns <- names(
        pattern_counts[pattern_counts >= nrow(items_matrix) * min_pattern_prop]
      )

      if (length(major_patterns) > 0) {

        detection_method <- "missing_pattern"

        version_id <- match(missing_pattern, major_patterns)
        version_vector <- as.character(version_id)
        version_vector[is.na(version_id)] <- NA_character_

        unclassified_rows <- which(is.na(version_vector))

        if (length(unclassified_rows) > 0) {
          unclassified_patterns <- sort(
            table(missing_pattern[unclassified_rows]),
            decreasing = TRUE
          )

          detection_warnings <- c(
            detection_warnings,
            paste0(
              "Nie przypisano wersji dla ",
              length(unclassified_rows),
              " obserwacji z rzadkimi wzorcami braków danych."
            )
          )
        }

      } else {
        detection_warnings <- c(
          detection_warnings,
          "Nie wykryto żadnego głównego wzorca braków danych spełniającego kryterium krytyczne."
        )
      }

    } else {
      detection_warnings <- c(
        detection_warnings,
        "Nie podano zmiennej wersji i nie wykryto braków danych pozwalających rozpoznać wersje."
      )
    }
  }

  raw_data[[detected_version_col]] <- version_vector

  detected_versions <- sort(unique(version_vector[!is.na(version_vector)]))

  version_items <- make_version_items(
    version_vector = version_vector,
    detected_versions = detected_versions
  )

  version_data <- make_version_data(
    raw_data = raw_data,
    items_matrix = items_matrix,
    version_vector = version_vector,
    detected_versions = detected_versions,
    version_items = version_items
  )

  unclassified_data <- list(
    raw_data = raw_data[unclassified_rows, , drop = FALSE],
    items_data = items_matrix[unclassified_rows, , drop = FALSE],
    n_obs = length(unclassified_rows)
  )

  version_summary <- data.frame(
    version = detected_versions,
    n_obs = vapply(version_data, function(x) x$n_obs, integer(1)),
    n_items = vapply(version_data, function(x) x$n_items, integer(1)),
    stringsAsFactors = FALSE
  )

  empty_version_items <- version_summary$version[version_summary$n_items == 0]

  if (length(empty_version_items) > 0) {
    detection_warnings <- c(
      detection_warnings,
      paste0(
        "Dla następujących wersji nie przypisano żadnych itemów: ",
        paste(empty_version_items, collapse = ", ")
      )
    )
  }

  list(
    raw_data = raw_data,
    has_versions = length(detected_versions) > 0,
    has_multiple_versions = length(detected_versions) > 1,
    detected_versions = detected_versions,
    version_items = version_items,
    version_data = version_data,
    version_summary = version_summary,
    unclassified_rows = unclassified_rows,
    unclassified_data = unclassified_data,
    detection_method = detection_method,
    detection_warnings = detection_warnings,
    pattern_counts = pattern_counts,
    major_patterns = major_patterns,
    unclassified_patterns = unclassified_patterns,
    detected_version_col = detected_version_col,
    min_pattern_prop = min_pattern_prop,
    item_missing_max_prop = item_missing_max_prop
  )
}



version_result <- detect_test_versions(
  raw_data              = raw_data,
  items_data            = items_data,
  item_cols             = item_cols,
  version_var           = "wersja",
  min_pattern_prop      = 0.05,
  item_missing_max_prop = 0.90,
  detected_version_col  = ".detected_version"
)




