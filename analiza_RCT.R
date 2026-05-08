
# analiza RCT

# WCZYTANIE DANYCH -------------------------------------------------------------------

wczytaj_baze <- function(sciezka) {

  # Sprawdzenie i instalacja wymaganych pakietów

  pakiety <- c("rio", "tibble", "janitor", "tools", "readxl", "haven")
  braki <- pakiety[!(pakiety %in% installed.packages()[, "Package"])]
  if (length(braki) > 0) {
    message("Instalowanie brakujących pakietów: ", paste(braki, collapse = ", "))
    install.packages(braki, dependencies = TRUE)
  }

  # Sprawdzenie istnienia pliku
  if (!file.exists(sciezka)) {
    stop(paste("!!! BŁĄD: Plik nie istnieje w tej lokalizacji:", sciezka))
  }

  # Walidacja rozszerzenia pliku

  rozszerzenie <- tolower(tools::file_ext(sciezka))
  dozwolone_rozsz <- c("xlsx", "xls", "csv", "rds", "dta")

  if (!(rozszerzenie %in% dozwolone_rozsz)) {
    warning(paste("OSTRZEŻENIE: Próbujesz wczytać plik .", rozszerzenie,
                  ". Funkcja najlepiej działa z: ", paste(dozwolone_rozsz, collapse = ", ")))
  }

  # Wczytanie pliku
  dane <- rio::import(sciezka)

  # Sprawdzenie nazw kolumn przed ich modyfikacją
  nazwy_imp <- colnames(dane)
  # R domyślnie nadaje nazwy V1, V2 etc., jeśli kolumny w pliku nie miały nagłówków
  if (is.null(nazwy_imp) || all(nazwy_imp == "") || any(grepl("^V[0-9]+$", nazwy_imp))) {
    message("INFORMACJA: Wykryto brak oryginalnych nazw kolumn lub domyślne nazwy R (V1, V2).")
  } else {
    message("INFORMACJA: Wykryto oryginalne nazwy kolumn.")
  }

  dane <- janitor::clean_names(dane)
  return(tibble::as_tibble(dane))
}


# WALIDACJA DANYCH --------------------------------------------------------
## Funkcja do identyfikacji itemów (wspolna dla obu skryptow)

identyfikuj_itemy <- function(dane, prefiks) {

  # Walidacja wejścia
  if (!is.data.frame(dane)) {
    stop("BŁĄD: Przekazany obiekt 'dane' nie jest typu data.frame ani tibble.")
  }

  # Identyfikacja kolumn (itemów) za pomocą wyrażenia regularnego
  item_cols <- grep(paste0("^", prefiks), names(dane), value = TRUE)

  # Obsługa błędów: weryfikacja czy znaleziono przynajmniej jedną kolumnę pasującą do wzorca
  if (length(item_cols) == 0) {
    dostepne <- paste(head(names(dane)), collapse = ", ")

    stop(paste0(
      "BŁĄD: Nie znaleziono żadnych kolumn z prefiksem '", prefiks, "'.\n",
      "Dostępne kolumny w zbiorze (pierwsze 20): ", dostepne, "..."
    ))
  }
  return(item_cols)
}


## Funkcja do sprawdzania typów danych

waliduj_i_konwertuj_itemy <- function(dane, prefiks) {

  # Walidacja wejścia
  if (!is.data.frame(dane)) {
    stop("BŁĄD: Przekazany obiekt 'dane' nie jest typu data.frame.")
  }
  # Identyfikacja kolumn z itemami (zadań)
  item_cols <- grep(paste0("^", prefiks), names(dane), value = TRUE)

  if (length(item_cols) == 0) {
    stop(paste0("BŁĄD: Nie znaleziono kolumn z prefiksem '", prefiks, "'."))
  }

  col_types <- sapply(dane[item_cols], class)

  non_numeric <- names(col_types[!col_types %in% c("numeric", "integer")])

  if (length(non_numeric) > 0) {

    cat("UWAGA: Zidentyfikowano, że zmienne z itemami nie są numeryczne.\n")
    cat("Zostaną one przekonwertowane na typ numeryczny:\n\n")

    for (col in non_numeric) {
      cat("  Przetwarzanie:", col, "(typ pierwotny:", col_types[col], ")\n")

      dane[[col]] <- as.numeric(as.character(dane[[col]]))
    }
    cat("\n")
  }

  # Podsumowanie wszystkich typów danych w bazie
  cat("Wszystkie typy zmiennych w zbiorze danych:\n\n")
  wszystkie_typy <- sapply(dane, class)

  # Wylistowanie zmiennych w formacie tekstowym
  for (nazwa in names(wszystkie_typy)) {
    cat("  Zmienna:", nazwa, "[Typ:", wszystkie_typy[nazwa], "]\n")
  }
  cat("\nSUKCES: Wszystkie zmienne z itemami są numeryczne.\n\n")
  return(invisible(dane))
}


## Funkcja do sprawdzania zakresu wartości


waliduj_kodowanie_itemow <- function(dane, prefiks) {

  # Walidacja wejścia
  if (!is.data.frame(dane)) {
    stop("BŁĄD: Przekazany obiekt 'dane' nie jest typu data.frame.")
  }

  item_cols <- grep(paste0("^", prefiks), names(dane), value = TRUE)

  if (length(item_cols) == 0) {
    stop(paste0("BŁĄD: Nie znaleziono kolumn z prefiksem '", prefiks, "'."))
  }

  cat("Sprawdzenie kodowania odpowiedzi (0/1/NA)\n\n")
  items_data <- dane[, item_cols, drop = FALSE]

  # Sprawdzenie kodowania w zidentyfikowanych kolumnach
  value_check <- sapply(items_data, function(x) {
    vals <- unique(x[!is.na(x)])
    all(vals %in% c(0, 1))
  })

  non_binary <- names(value_check[!value_check])

  # Obsługa błędów kodowania i usuwanie niewłaściwych itemów
  if (length(non_binary) > 0) {

    cat("BŁĄD: Następujące itemy zawierają wartości inne niż 0/1:\n\n")

    for (col in non_binary) {
      vals <- sort(unique(items_data[[col]][!is.na(items_data[[col]])]))
      cat("  Zmienna:", col, "-> zidentyfikowane wartości =", paste(vals, collapse = ", "), "\n")
    }

    cat("\nUWAGA: Itemy z wartościami innymi niż 0/1 zostaną wykluczone z analiz.\n\n")

    dane <- dane[, !(names(dane) %in% non_binary), drop = FALSE]
  } else {
    cat("Wszystkie pierwotne itemy zakodowane poprawnie jako 0/1. Status: OK\n\n")
  }
  return(invisible(dane))
}


## Funkcja do wstępnego sprawdzania braków danych

analiza_brakow_danych <- function(data) {

  # Identyfikacja kolumn z itemami (prefiks "mat_")
  item_cols <- grep("^mat_", names(data), value = TRUE)

  # Selekcja danych do analizy
  if(length(item_cols) > 0) {
    items_data <- data[, item_cols]
  } else {
    items_data <- data
  }

  # Braki (procenty)
  missing_per_item <- colMeans(is.na(items_data)) * 100
  missing_per_person <- rowMeans(is.na(items_data)) * 100

  cat("Braki danych per item:\n\n")

  # Przygotowanie podsumowania dla itemów
  missing_summary <- data.frame(
    Item = names(missing_per_item),
    Procent_brakow = round(missing_per_item, 1)
  )

  missing_summary <- missing_summary[order(-missing_summary$Procent_brakow), ]
  items_with_missing <- missing_summary[missing_summary$Procent_brakow > 0, ]

  # Wyświetlanie tabeli itemów
  if (nrow(items_with_missing) > 0) {
    cat("Itemy, w których odnotowano braki danych:\n")
    print(items_with_missing, row.names = FALSE)
    cat("\n")
  } else {
    cat("Brak brakujących danych w poszczególnych itemach. Status: OK\n\n")
  }

  # Statystyki braków na poziomie osób
  cat("Braki danych per osoba (wiersz):\n\n")
  cat("  Sredni procent braków:      ", round(mean(missing_per_person), 1), "%\n")
  cat("  Mediana procentu braków:    ", round(median(missing_per_person), 1), "%\n")
  cat("  Maksymalny procent braków:  ", round(max(missing_per_person), 1), "%\n")
  cat("  Liczba osób z brakami > 90%:", sum(missing_per_person > 90), "\n")

}


# ZMIANA FORMATU Z LONG NA WIDE --------------------------------------


zmien_na_wide <- function(df, kolumny_id = c("id_szkoly", "id_ucznia", "grupa"), kolumna_pomiaru = "pomiar", kolumny_wartosci = NULL) {

  pakiety <- c("tidyr", "dplyr")
  brakujace <- pakiety[!vapply(pakiety, requireNamespace, logical(1), quietly = TRUE)]
  if (length(brakujace) > 0) {
    stop("Brakujące pakiety: ", paste(brakujace, collapse = ", "), ". Zainstaluj je.")
  }

  # Sprawdzenie, czy baza jest w formacie long (czy posiada kolumnę pomiaru)
  if (!(kolumna_pomiaru %in% names(df))) {
    message("Baza nie zawiera kolumny '", kolumna_pomiaru, "'. Zwracam dane bez zmian (format wide).")
    return(df)
  }

  # Automatyczna selekcja zmiennych (jeśli nie podano)
  if (is.null(kolumny_wartosci)) {
    kolumny_wartosci <- setdiff(names(df), c(kolumny_id, kolumna_pomiaru))
  }

  # Przekształcenie do formatu wide
  df_wide <- df |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(kolumny_id),
      names_from = dplyr::all_of(kolumna_pomiaru),
      values_from = dplyr::all_of(kolumny_wartosci),
      names_sep = "_"
    )

  # Konwersja grupy na faktor (tylko jeśli istnieje w nowej bazie)
  if ("grupa" %in% names(df_wide)) {
    df_wide$grupa <- as.factor(df_wide$grupa)
  }
  return(df_wide)
}




# WSTĘPNA EKSPLORACJA DANYCH ----------------------------------------------

oblicz_statystyki_badania <- function(df) {

  pakiety <- c("dplyr", "knitr")
  brakujace <- pakiety[!sapply(pakiety, requireNamespace, quietly = TRUE)]

  if (length(brakujace) > 0) {
    stop(paste("Brakujące pakiety:", paste(brakujace, collapse = ", "),
               ". Zainstaluj je, aby kontynuować."))
  }


  liczba_szkol <- length(unique(df$id_szkoly))
  liczba_uczniow <- length(unique(df$id_ucznia))

  # Podsumowanie liczby uczniów biorących udział w każdym z pomiarów (pre-test/post-test)
  pomiar_n <- dplyr::summarise(dplyr::group_by(df, pomiar), n = dplyr::n_distinct(id_ucznia))

  # Liczba uczniów w grupie kontrolnej (1) i eksperymentalnej (2) wyłącznie dla pierwszego pomiaru (pre-test)
  grupy_pre <- dplyr::summarise(dplyr::group_by(dplyr::filter(df, pomiar == 1), grupa), n = dplyr::n_distinct(id_ucznia))

  kolumny_mat <- grep("^mat_", names(df), value = TRUE)

  # Suma punktów dla każdego wiersza (wynik ogólny ucznia)
  df$wynik_total <- rowSums(df[, kolumny_mat], na.rm = TRUE)

  # Tabela statystyk opisowych (grupowanie) po numerze pomiaru oraz przynależności do grupy)
  tabela_stats <- dplyr::summarise(
    dplyr::group_by(df, pomiar, grupa),
    N = dplyr::n(),
    Min = min(wynik_total, na.rm = TRUE),
    Q1 = stats::quantile(wynik_total, 0.25, na.rm = TRUE), #
    Mediana = stats::median(wynik_total, na.rm = TRUE),
    Srednia = mean(wynik_total, na.rm = TRUE),
    Q3 = stats::quantile(wynik_total, 0.75, na.rm = TRUE),
    Max = max(wynik_total, na.rm = TRUE),
    SD = stats::sd(wynik_total, na.rm = TRUE),
    .groups = "drop"
  )

  return(list(
    n_szkoly = liczba_szkol, # Liczba szkół
    n_uczniow = liczba_uczniow, # Liczba uczniów
    pomiar_n = pomiar_n, # Uczniowie per pomiar
    grupy_pre = grupy_pre, # Podział na grupy na wejściu
    tabela = tabela_stats # Pełna tabela statystyk opisowych
  ))
}

# BALANCE CHECK (analiza poprawnosci randomizacji) ------------------------


sprawdz_randomizacje <- function(df) {

  pakiety <- c("dplyr", "lme4", "lmerTest")
  brakujace <- pakiety[!sapply(pakiety, requireNamespace, quietly = TRUE)]

  if (length(brakujace) > 0) {
    stop(paste("Do działania funkcji wymagane są pakiety:", paste(brakujace, collapse = ", ")))
  }

  # Etap 1: Sprawdzenie poprawności przypisania szkół do grup (każda szkoła powinna należeć tylko do jednej grupy)

  dane_zgrupowane <- dplyr::group_by(df, id_szkoly)

  # Obliczanie, ile różnych (unikalnych) wartości z kolumny 'grupa' ma każda szkoła, używając n_distinct
  liczba_grup_na_szkole <- dplyr::summarise(dane_zgrupowane, liczba_grup = dplyr::n_distinct(grupa))

  # Filtrowanie wyników, aby zachować tylko te szkoły, którym błędnie przypisano więcej niż jedną grupę
  konflikty <- dplyr::filter(liczba_grup_na_szkole, liczba_grup > 1)

  # Etap 2: Przygotowanie wyniku ogólnego z pre-testu w celu oceny braku początkowych różnic w wiedzy między grupami
  df_pre <- dplyr::filter(df, pomiar == 1)

  kolumny_mat <- grep("^mat_", names(df_pre), value = TRUE)

  # Tworzenie nowej kolumny 'score_pre', która przechowuje sumę punktów dla każdego ucznia
  df_pre$score_pre <- rowSums(df_pre[, kolumny_mat], na.rm = TRUE)

  # Etap 3: Weryfikacja równości grup z wykorzystaniem Wielopoziomowego Modelu Mieszanego (LMM - Balance Check)
  # Dopasowanie modelu, w którym wynik to zmienna zależna, grupa to predyktor (jako czynnik), a szkoła to losowy efekt odcięcia
  model_lmm <- lmerTest::lmer(score_pre ~ as.factor(grupa) + (1 | id_szkoly), data = df_pre)

  podsumowanie_modelu <- summary(model_lmm)

  return(list(konflikty_szkol = konflikty, model = podsumowanie_modelu))
}


# ANALIZA BRAKOW DANYCH ---------------------------------------------------

analiza_ubytku_proby <- function(df) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Pakiet 'dplyr' jest wymagany.")

  kolumny_mat <- grep("^mat_", names(df), value = TRUE)

  df_up <- df |>
    dplyr::mutate(score_total = rowSums(dplyr::across(dplyr::all_of(kolumny_mat)), na.rm = TRUE)) |>
    dplyr::group_by(id_ucznia, id_szkoly, grupa) |>
    dplyr::summarise(
      obecny_pre  = any(pomiar == 1),
      obecny_post = any(pomiar == 2),
      score_pre   = mean(score_total[pomiar == 1], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(obecny_pre == TRUE) |>
    dplyr::mutate(czy_wypadl = !obecny_post)

  # Sprawdzenie liczby poziomów zmiennej 'czy_wypadl'
  liczba_grup <- length(unique(df_up$czy_wypadl))
  tabela_up <- table(df_up$grupa, df_up$czy_wypadl)

  # Inicjacja pustych wyników
  t_test_wynik <- NULL
  chi2_test <- NULL

  # wykonanie testów tylko, jeśli mamy 2 grupy (TRUE i FALSE)
  if (liczba_grup == 2) {
    t_test_wynik <- stats::t.test(score_pre ~ czy_wypadl, data = df_up)
    chi2_test <- stats::chisq.test(tabela_up)
  } else {
    warning("Testy statystyczne nie zostały wykonane: brak zróżnicowania w ubytku próby (wszyscy zostali lub wszyscy wypadli).")
    t_test_wynik <- "Nie można wykonać testu - tylko jedna grupa obecna w danych."
    chi2_test <- "Nie można wykonać testu - tylko jedna grupa obecna w danych."
  }

  return(list(
    dane_atrycja = df_up,
    t_test       = t_test_wynik,
    chi2         = chi2_test,
    tabela       = tabela_up
  ))
}


# OBLICZANIE ICC (współczynnik korelacji wewnatrzklasowej) ------------------------------------


oblicz_icc_dla_pomiaru <- function(df, nr_pomiaru) {

  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(dplyr, tidyr, lme4)

  df_model <- df |>
    dplyr::filter(pomiar == nr_pomiaru) |>
    dplyr::mutate(
      score_total = rowSums(dplyr::across(dplyr::starts_with("mat_")), na.rm = TRUE)
    )

  # Sprawdzenie, czy mamy dane po filtrowaniu
  if (nrow(df_model) == 0) stop(paste("Brak danych dla pomiaru:", nr_pomiaru))

  # Estymacja modelu
  model_null <- tryCatch({
    lme4::lmer(score_total ~ 1 + (1 | id_szkoly), data = df_model)
  }, error = function(e) return(NULL))

  if (is.null(model_null)) return(list(icc = NA, msg = "Błąd zbieżności modelu"))

  # Komponenty wariancji
  vc <- as.data.frame(lme4::VarCorr(model_null))

  var_between <- vc$vcov[vc$grp == "id_szkoly"]
  var_within  <- vc$vcov[vc$grp == "Residual"]

  # Obliczenie ICC
  icc_val <- var_between / (var_between + var_within)

  return(list(
    icc = icc_val,
    var_between = var_between,
    var_within = var_within,
    is_singular = lme4::isSingular(model_null)
  ))
}


# OBLICZANIE SUMY WYNIKÓW I STANDARYZACJA SUMY ----------------------------


oblicz_punkty_zscore <- function(df) {
  kolumny_mat <- grep("^mat", names(df), value = TRUE)

  # Obliczanie sumy i standaryzacja w jednym ciągu
  df_gotowe <- df |>
    dplyr::mutate(
      suma_mat = rowSums(dplyr::pick(dplyr::all_of(kolumny_mat)), na.rm = TRUE)
    ) |>
    dplyr::mutate(
      std_suma_mat = (suma_mat - mean(suma_mat, na.rm = TRUE)) / stats::sd(suma_mat, na.rm = TRUE),
      .by = pomiar
    )

  # Obliczanie statystyk
  stat_grupa <- df_gotowe |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(suma_mat, na.rm = TRUE),
      sd = stats::sd(suma_mat, na.rm = TRUE),
      .by = grupa
    )

  stat_pomiar <- dplyr::summarise(
    dplyr::group_by(df_gotowe, pomiar),
    mean_std = round(mean(std_suma_mat, na.rm = TRUE), 10),
    sd_std = sd(std_suma_mat, na.rm = TRUE)
  )

  return(list(dane = df_gotowe, stat_grupa = stat_grupa, stat_pomiar = stat_pomiar))
}


# ESTYMACJA MODELU (Mixed Ancova) -----------------------------------------

analiza_mlm_ancova <- function(df) {

  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    message("Pakiet 'lmerTest' nie został znaleziony. Próbuję zainstalować...")
    install.packages("lmerTest", repos = "https://cloud.r-project.org")
  }

  library(lmerTest)

  pakiety <- c("dplyr", "tidyr", "lme4")
  brakujace <- pakiety[!vapply(pakiety, requireNamespace, logical(1), quietly = TRUE)]
  if (length(brakujace) > 0) stop("Zainstaluj: ", paste(brakujace, collapse = ", "))

  # Przygotowanie danych

  df_wide <- df |>
    dplyr::mutate(score = rowSums(dplyr::across(dplyr::starts_with("mat_")), na.rm = TRUE)) |>
    dplyr::group_by(pomiar) |>
    dplyr::mutate(std_score = as.vector(scale(score))) |>
    dplyr::ungroup() |>
    dplyr::select(id_ucznia, id_szkoly, grupa, pomiar, score, std_score) |>
    tidyr::pivot_wider(
      id_cols = c(id_ucznia, id_szkoly, grupa),
      names_from = pomiar,
      values_from = c(score, std_score),
      names_glue = "{.value}_{ifelse(pomiar==1, 'pre', 'post')}"
    ) |>
    stats::na.omit() |>
    dplyr::mutate(grupa = as.factor(grupa))

  # Modelowanie
  mod_raw  <- lmerTest::lmer(score_post ~ grupa + score_pre + (1 | id_szkoly), data = df_wide)
  mod_std  <- lmerTest::lmer(std_score_post ~ grupa + std_score_pre + (1 | id_szkoly), data = df_wide)
  mod_null <- lmerTest::lmer(score_post ~ 1 + (1 | id_szkoly), data = df_wide)

  sum_raw <- summary(mod_raw)

  # statystyki i wielkośc efektu
  vc <- as.data.frame(lme4::VarCorr(mod_null))
  var_between <- vc$vcov[vc$grp == "id_szkoly"]
  var_within  <- vc$vcov[vc$grp == "Residual"]

  s_total <- sqrt(var_between + var_within)

  # Efekt grupy
  idx <- grep("grupa", rownames(sum_raw$coefficients))
  if(length(idx) == 0) stop("Nie znaleziono zmiennej 'grupa' w modelu. Sprawdź dane.")

  beta_grupa <- sum_raw$coefficients[idx[1], "Estimate"]

  N <- nrow(df_wide)
  J <- 1 - (3 / (4 * (N - 2) - 1))
  hedges_g <- (beta_grupa / s_total) * J

  # Diagnostyka reszt
  reszty <- stats::residuals(mod_raw)
  shapiro_p <- if (N > 5000) stats::shapiro.test(sample(reszty, 5000))$p.value else stats::shapiro.test(reszty)$p.value
  hetero_p <- stats::cor.test(abs(reszty), stats::fitted(mod_raw), method = "spearman")$p.value

  # Wynik
  return(list(
    mod_raw_sum = sum_raw,
    mod_std_sum = summary(mod_std),
    icc = var_between / (var_between + var_within),
    hedges_g = hedges_g,
    N = N,
    diag_norm_p = shapiro_p,
    diag_hetero_p = hetero_p
  ))
}


# ANALIZA ODPORNOŚCI ------------------------------------------------------
# (analiza regresji z robust SE)


analiza_regresji_robust <- function(data, var_1, var_2, grupa_col = "grupa", cluster_col = "id_szkoly") {

  if (!requireNamespace("estimatr", quietly = TRUE)) {
    message("Instalacja brakującego pakietu 'estimatr'...")
    install.packages("estimatr")
  }

  library(estimatr)

  # Walidacja kolumn
  wymagane_kolumny <- c(var_1, var_2, grupa_col, cluster_col)
  if (!all(wymagane_kolumny %in% names(data))) {
    stop(paste("Błąd: Nie znaleziono kolumn:", paste(setdiff(wymagane_kolumny, names(data)), collapse = ", ")))
  }

  # Tworzenie formuły
  formula_robust <- reformulate(termlabels = c(grupa_col, var_1), response = var_2)

  cat("\n--- Klasyczna regresja z cluster robust SE (estimatr::lm_robust) ---\n")

  # Wykonanie modelu
  model_robust <- lm_robust(
    formula = formula_robust,
    data = data,
    clusters = !!sym(cluster_col),
    se_type = "stata"
  )

  print(summary(model_robust))
  return(model_robust)
}




