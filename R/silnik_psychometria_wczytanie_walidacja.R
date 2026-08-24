# ============================================================================
# FUNKCJE POMOCNICZE - RAPORT PSYCHOMETRYCZNY
# ============================================================================
# ============================================================================
# LADOWANIE PAKIETOW
# ============================================================================

# Pakiety wymagane przez funkcje w tym pliku.
# Blok nie jest wykonywany, ale zostawia jawne deklaracje zaleznosci dla renv.
# W kodzie funkcji uzywamy wywolan z przestrzenia nazw, np. haven::read_dta(),
# zeby nie ladowac calych pakietow do sciezki wyszukiwania przez library().
if (FALSE) {
  library(haven)
  library(readxl)
}

# ============================================================================
# WCZYTYWANIE DANYCH
# ============================================================================

#' @title Wczytanie danych psychometrycznych do analizy
#'
#' @description Wczytuje dane z jednego z obslugiwanych formatow: CSV, DTA, RDS,
#' XLS lub XLSX. Funkcja wymaga jawnie podanej sciezki do pliku i nie uruchamia
#' interaktywnego wyboru pliku.
#'
#' @param data_path Jednoelementowy wektor tekstowy ze sciezka do pliku danych.
#'
#' @return Ramka danych (`data.frame`).
#'
#' @details Funkcja ma kontrolowany efekt wejsciowy: odczytuje plik z dysku.
#' Nie zapisuje plikow i nie modyfikuje srodowiska globalnego.
#'
#' @export

read_psych_data <- function(data_path) {

  if (
    missing(data_path) ||
    is.null(data_path) ||
    !is.character(data_path) ||
    length(data_path) != 1 ||
    data_path == ""
  ) {
    stop(
      "Argument 'data_path' musi byc pojedyncza, niepusta sciezka do pliku.",
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
      data_csv <- utils::read.csv(
        data_path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (ncol(data_csv) == 1) {
        data_csv2 <- utils::read.csv2(
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
      paste0("Nieobslugiwany format pliku: ", file_ext),
      call. = FALSE
    )
  )

  if (!is.data.frame(raw_data)) {
    stop(
      "Wczytany obiekt nie jest ramka danych.",
      call. = FALSE
    )
  }

  as.data.frame(raw_data)
}

# ============================================================================
# IDENTYFIKACJA ITEMOW
# ============================================================================

#' @title Identyfikacja kolumn itemow
#'
#' @description Funkcja wyszukuje kolumny rozpoczynajace sie od wskazanego prefiksu.
#' Jezeli itemy maja numeryczne koncowki, porzadkuje je wedlug numerow.
#'
#' @param raw_data Ramka danych
#' @param item_prefix Prefiks nazw kolumn z itemami.
#' @param exclude_items Opcjonalny wektor nazw itemow do wykluczenia.
#'
#' @return Wektor nazw kolumn z itemami.
#'
#' @details Funkcja jest czysta: nie zapisuje plikow, nie wypisuje komunikatow
#' i nie modyfikuje przekazanej ramki danych.
#'
#' @export


identify_item_columns <- function(raw_data, item_prefix, exclude_items = NULL) {
  if (!is.data.frame(raw_data)) {
    stop(
      "Argument 'raw_data' musi byc ramka danych.",
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
      "Argument 'item_prefix' musi byc pojedynczym, niepustym ciagiem znakow.",
      call. = FALSE
    )
  }

  item_cols <- names(raw_data)[startsWith(names(raw_data), item_prefix)]

  if (length(item_cols) == 0) {
    stop(paste0(
      "BLAD: Nie znaleziono zadnych kolumn z prefiksem '", item_prefix, "'.\n",
      "Dostepne kolumny: ", paste(utils::head(names(raw_data), 20), collapse = ", "),
      "\nSprawdz parametr 'item_prefix'."
    ),
    call. = FALSE)
  }

  item_nums <- suppressWarnings(
    as.numeric(substr(item_cols, nchar(item_prefix) + 1, nchar(item_cols)))
  )

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

  if (length(item_cols) == 0) {
    stop(
      "Po zastosowaniu 'exclude_items' nie pozostaly zadne itemy do analizy.",
      call. = FALSE
    )
  }

  item_cols
}


# ============================================================================
# WALIDACJA DANYCH
# ============================================================================

#' @title Walidacja itemow testowych
#'
#' @description Sprawdza typ danych, klasyfikuje itemy jako binarne lub
#' politomiczne i weryfikuje wariancje. Itemy z prawidlowym kodowaniem
#' (kolejne liczby calkowite od 0 do max) sa zachowywane niezaleznie od
#' liczby kategorii. Itemy z niecalkowitymi, ujemnymi lub niespojnymi
#' wartosciami sa wykluczane.
#'
#' @param raw_data Ramka danych
#' @param item_cols Wektor nazw kolumn z itemami

#' @return Lista z oczyszczonymi danymi itemowymi, diagnostykami walidacyjnymi
#'   oraz klasyfikacja typu itemow (\code{item_type}, \code{item_max_scores},
#'   \code{n_categories}).
#'
#' @details Funkcja jest czysta wzgledem srodowiska zewnetrznego: nie zapisuje
#' plikow, nie wypisuje komunikatow i nie modyfikuje przekazanej ramki danych.
#' Zwraca oczyszczone dane itemowe oraz diagnostyki walidacyjne.
#'
#' @export

validate_items_data <- function(raw_data, item_cols) {

  if (!is.data.frame(raw_data)) {
    stop(
      "Argument 'raw_data' musi byc ramka danych.",
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
      "Argument 'item_cols' musi byc ciagiem znakow o dlugosci wiekszej niz 0.",
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

  # ------------------------------------------------------------------
  # Klasyfikacja itemow: binarne vs politomiczne
  # ------------------------------------------------------------------
  # Itemy z wartosciami bedacymi kolejnymi liczbami calkowitymi (np.
  # 0,1 lub 0,1,2,3) sa traktowane jako prawidlowe. Itemy zaczynajace
  # sie od wartosci > 0 sa przeskalowywane (odjecie minimum). Itemy z
  # niecalkowitymi, ujemnymi lub niespojnymi wartosciami sa wykluczane.

  item_diagnostics <- lapply(names(items_data), function(col) {
    vals <- sort(unique(items_data[[col]][!is.na(items_data[[col]])]))

    if (length(vals) == 0) {
      return(list(
        name = col, vals = vals, min_val = NA_real_,
        max_score = NA_real_, n_categories = 0L,
        is_valid = FALSE, is_binary = FALSE,
        issue = "brak_wartosci"
      ))
    }

    is_integer <- all(vals == floor(vals))
    if (!is_integer) {
      return(list(
        name = col, vals = vals, min_val = NA_real_,
        max_score = NA_real_, n_categories = NA_integer_,
        is_valid = FALSE, is_binary = FALSE,
        issue = "niecalkowite"
      ))
    }

    is_non_negative <- all(vals >= 0)
    if (!is_non_negative) {
      return(list(
        name = col, vals = vals, min_val = NA_real_,
        max_score = NA_real_, n_categories = NA_integer_,
        is_valid = FALSE, is_binary = FALSE,
        issue = "ujemne"
      ))
    }

    min_val <- min(vals)
    max_val <- max(vals)
    expected_seq <- seq(min_val, max_val)
    is_contiguous <- setequal(vals, expected_seq)

    if (!is_contiguous) {
      return(list(
        name = col, vals = vals, min_val = min_val,
        max_score = NA_real_, n_categories = NA_integer_,
        is_valid = FALSE, is_binary = FALSE,
        issue = "luki_w_kategoriach"
      ))
    }

    effective_max <- max_val - min_val

    list(
      name = col,
      vals = vals,
      min_val = min_val,
      max_score = effective_max,
      n_categories = as.integer(effective_max + 1L),
      is_valid = TRUE,
      is_binary = (effective_max == 1),
      issue = NA_character_
    )
  })

  names(item_diagnostics) <- names(items_data)

  is_valid <- vapply(item_diagnostics, function(d) d$is_valid, logical(1))

  invalid_items <- names(is_valid[!is_valid])
  non_binary_values <- list()

  if (length(invalid_items) > 0) {

    non_binary_values <- lapply(
      invalid_items,
      function(col) item_diagnostics[[col]]$vals
    )

    names(non_binary_values) <- invalid_items

    invalid_reasons <- vapply(
      invalid_items,
      function(col) item_diagnostics[[col]]$issue,
      character(1)
    )

    item_cols <- setdiff(item_cols, invalid_items)
    items_data <- items_data[, item_cols, drop = FALSE]

    validation_issues <- c(
      validation_issues,
      list(
        paste0(
          "Wykluczono itemy z nieprawidlowymi wartosciami: ",
          paste(
            sprintf("%s (%s)", invalid_items, invalid_reasons),
            collapse = ", "
          )
        )
      )
    )
  }

  if (length(item_cols) == 0) {
    stop(
      "Po walidacji wartosci nie pozostaly zadne itemy do analizy.",
      call. = FALSE
    )
  }

  # Przeskaluj itemy zaczynajace sie od wartosci > 0
  recoded_items <- character(0)

  for (col in item_cols) {
    min_val <- item_diagnostics[[col]]$min_val
    if (!is.na(min_val) && min_val > 0) {
      items_data[[col]] <- items_data[[col]] - min_val
      recoded_items <- c(recoded_items, col)
    }
  }

  if (length(recoded_items) > 0) {
    validation_warnings <- c(
      validation_warnings,
      list(paste0(
        "Przeskalowano itemy (odjeto minimum, aby zakres zaczynal sie od 0): ",
        paste(recoded_items, collapse = ", ")
      ))
    )
  }

  # Okresl typ itemow
  valid_diag <- item_diagnostics[item_cols]

  item_max_scores <- vapply(
    valid_diag, function(d) as.integer(d$max_score), integer(1)
  )
  names(item_max_scores) <- item_cols

  n_categories <- vapply(
    valid_diag, function(d) d$n_categories, integer(1)
  )
  names(n_categories) <- item_cols

  all_binary <- all(item_max_scores == 1L)
  any_binary <- any(item_max_scores == 1L)

  item_type <- if (all_binary) {
    "binary"
  } else if (!any_binary) {
    "polytomous"
  } else {
    "mixed"
  }

  item_vars <- vapply(
    items_data,
    function(x) stats::var(x, na.rm = TRUE),
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
          "Wykluczono itemy z zerowa wariancja: ",
          paste(zero_var_items, collapse = ", ")
        )
      )
    )
  }

  if (length(item_cols) == 0) {
    stop(
      "Po walidacji nie pozostaly zadne itemy do analizy.",
      call. = FALSE
    )
  }

  list(
    item_cols = item_cols,
    items_data = items_data,
    item_type = item_type,
    item_max_scores = item_max_scores,
    n_categories = n_categories,
    validation_issues = validation_issues,
    validation_warnings = validation_warnings,
    non_numeric_items = non_numeric,
    conversion_diagnostics = conversion_diagnostics,
    non_binary_items = invalid_items,
    non_binary_values = non_binary_values,
    zero_variance_items = zero_var_items
  )
}

# ============================================================================
# WYKRYWANIE WERSJI TESTU
# ============================================================================

#' @title Wykrywanie wersji testu
#'
#' @description Wykrywa wersje testu z kolumny wersji lub ze wzorcow brakow danych.
#'
#' @param raw_data Ramka danych.
#' @param items_data Ramka danych z itemami.
#' @param item_cols Wektor nazw kolumn z itemami.
#' @param version_var Opcjonalna nazwa zmiennej z wersja testu.
#' @param min_pattern_prop Minimalny udzial wzorca brakow, aby uznac go za wersje.
#' @param item_missing_max_prop Maksymalny udzial brakow itemu w danej wersji.
#' @param detected_version_col Nazwa roboczej kolumny z wykryta wersja.
#'
#' @return Lista z danymi, wersjami testu, itemami wersji i diagnostyka.
#'
#' @details Funkcja nie modyfikuje przekazanej ramki danych w miejscu. Zwraca
#' kopie `raw_data` z dodana kolumna wskazana przez `detected_version_col`.
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
    stop("Argument 'raw_data' musi byc ramka danych.", call. = FALSE)
  }

  if (!is.data.frame(items_data)) {
    stop("Argument 'items_data' musi byc ramka danych.", call. = FALSE)
  }

  if (
    missing(item_cols) ||
    is.null(item_cols) ||
    !is.character(item_cols) ||
    length(item_cols) == 0
  ) {
    stop("Argument 'item_cols' musi byc niepustym wektorem nazw kolumn.", call. = FALSE)
  }

  if (nrow(raw_data) != nrow(items_data)) {
    stop(
      "Argumenty 'raw_data' i 'items_data' musza miec te sama liczbe wierszy.",
      call. = FALSE
    )
  }

  missing_item_cols <- setdiff(item_cols, names(items_data))

  if (length(missing_item_cols) > 0) {
    stop(
      paste0(
        "W 'items_data' brakuje nastepujacych itemow: ",
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
      "Argument 'version_var' musi byc pojedyncza nazwa kolumny albo NULL.",
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
      "Argument 'min_pattern_prop' musi byc pojedyncza liczba z przedzialu (0, 1).",
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
      "Argument 'item_missing_max_prop' musi byc pojedyncza liczba z przedzialu (0, 1].",
      call. = FALSE
    )
  }

  if (
    !is.character(detected_version_col) ||
    length(detected_version_col) != 1 ||
    detected_version_col == ""
  ) {
    stop(
      "Argument 'detected_version_col' musi byc pojedyncza nazwa kolumny.",
      call. = FALSE
    )
  }

  if (detected_version_col %in% names(raw_data)) {
    stop(
      paste0(
        "Kolumna '", detected_version_col,
        "' juz istnieje w danych. Wybierz inna nazwe przez 'detected_version_col'."
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
          ". Podjeto probe wykrycia wersji na podstawie brakow danych."
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
              " obserwacji z rzadkimi wzorcami brakow danych."
            )
          )
        }

      } else {
        detection_warnings <- c(
          detection_warnings,
          "Nie wykryto zadnego glownego wzorca brakow danych spelniajacego kryterium krytyczne."
        )
      }

    } else {
      detection_warnings <- c(
        detection_warnings,
        "Nie podano zmiennej wersji i nie wykryto brakow danych pozwalajacych rozpoznac wersje."
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
        "Dla nastepujacych wersji nie przypisano zadnych itemow: ",
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
