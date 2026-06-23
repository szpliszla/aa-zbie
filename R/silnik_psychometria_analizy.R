# ============================================================================
# FUNKCJE POMOCNICZE - ANALIZY PSYCHOMETRYCZNE
# ============================================================================


# Pakiety wymagane przez funkcje w tym pliku.
# Blok nie jest wykonywany, ale zostawia jawne deklaracje zaleznosci dla renv.
# W kodzie funkcji uzywamy wywolan z przestrzenia nazw, np. psych::alpha(),
# zeby nie ladowac calych pakietow do sciezki wyszukiwania przez library().
if (FALSE) {
  library(psych)
  library(ggplot2)
  library(rlang)
  library(mirt)
  library(sirt)
  library(writexl)
}


#' @importFrom rlang .data
NULL

#' @title Utworzenie statusu wykonania funkcji
#'
#' @description Tworzy jednolita ramke danych opisujaca status wykonania funkcji analitycznej. Status jest zwracany w wynikach zamiast wypisywania komunikatow do konsoli lub raportu.
#'
#' @param ok Wartosc logiczna informujaca, czy funkcja zakonczyla sie poprawnie.
#' @param code Jednoelementowy wektor tekstowy z kodem statusu lub bledu.
#' @param message Jednoelementowy wektor tekstowy z komunikatem statusu lub bledu.
#'
#' @return Ramka danych z kolumnami `ok`, `code` i `message`.
#'
#' @examples
#' make_status(TRUE, "ok", NA_character_)
#'
#' @export
make_status <- function(ok = TRUE, code = NA_character_, message = NA_character_) {
  data.frame(
    ok = ok,
    code = code,
    message = message,
    stringsAsFactors = FALSE
  )
}

#' @title Interpretacja wspolczynnika alfa Cronbacha
#'
#' @description Przypisuje tekstowa interpretacje do wartosci wspolczynnika alfa Cronbacha zgodnie z progami stosowanymi w raporcie psychometrycznym.
#'
#' @param alpha_val Jedna wartosc liczbowa reprezentujaca alfa Cronbacha.
#'
#' @return Jednoelementowy wektor tekstowy z interpretacja rzetelnosci.
#'
#' @examples
#' interpret_alpha(0.82)
#'
#' @export
interpret_alpha <- function(alpha_val) {
  if (is.na(alpha_val)) return(NA_character_)
  if (alpha_val >= 0.9) return("Doskonala")
  if (alpha_val >= 0.8) return("Dobra")
  if (alpha_val >= 0.7) return("Akceptowalna")
  if (alpha_val >= 0.6) return("Watpliwa")
  "Niska"
}

#' @title Interpretacja wspolczynnika omega McDonalda
#'
#' @description Przypisuje tekstowa interpretacje do wartosci wspolczynnika omega McDonalda zgodnie z progami stosowanymi w raporcie psychometrycznym.
#'
#' @param omega_val Jedna wartosc liczbowa reprezentujaca omega McDonalda.
#'
#' @return Jednoelementowy wektor tekstowy z interpretacja rzetelnosci.
#'
#' @examples
#' interpret_omega(0.76)
#'
#' @export
interpret_omega <- function(omega_val) {
  if (is.na(omega_val)) return(NA_character_)
  if (omega_val >= 0.8) return("Dobra")
  if (omega_val >= 0.7) return("Akceptowalna")
  "Niska"
}

#' @title Przygotowanie bezpiecznej nazwy arkusza Excel
#'
#' @description Oczyszcza proponowana nazwe arkusza z niedozwolonych znakow i przycina ja do 31 znakow, czyli limitu stosowanego przez Excel. Funkcja jest uzywana podczas eksportu wynikow do pliku XLSX.
#'
#' @param x Jednoelementowy wektor tekstowy z proponowana nazwa arkusza.
#'
#' @return Jednoelementowy wektor tekstowy zawierajacy bezpieczna nazwe arkusza.
#'
#' @examples
#' safe_sheet_name("Wyniki Analizy CTT (Wersja A)!")
#'
#' @export
safe_sheet_name <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("Argument 'x' musi byc pojedynczym ciagiem znakow.")
  }

  x <- gsub("[^A-Za-z0-9_]", "_", x)
  return(substr(x, 1, 31))
}

#' @title Standaryzacja zmiennej do wyniku z
#'
#' @description Przelicza wektor liczbowy na wynik standaryzowany z, odejmujac srednia i dzielac przez odchylenie standardowe. Funkcja obsluguje braki danych oraz zwraca brak danych, gdy odchylenie standardowe jest zerowe lub niemozliwe do wyznaczenia.
#'
#' @param x Wektor liczbowy przeznaczony do standaryzacji.
#'
#' @return Wektor liczbowy tej samej dlugosci co `x`, zawierajacy wartosci z.
#'
#' @examples
#' z_score(c(1, 2, 3, 4, 5))
#'
#' @export
z_score <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }

  return((x - mean(x, na.rm = TRUE)) / s)
}

#' @title Przygotowanie wykresu jakosci itemow CTT
#'
#' @description Buduje obiekt wykresu ggplot przedstawiajacy relacje miedzy trudnoscia itemu a skorygowana korelacja item-test. Funkcja nie wyswietla wykresu, tylko zwraca obiekt do wykorzystania w raporcie.
#'
#' @param item_stats Ramka danych ze statystykami itemow, zawierajaca co najmniej kolumny `Item`, `Trudnosc_p`, `r_cor` i `Ocena`.
#' @param label Etykieta analizy uzywana w tytule wykresu.
#'
#' @return Obiekt klasy `ggplot`.
#'
#' @examples
#' df <- data.frame(Item = c("i1", "i2"), Trudnosc_p = c(0.4, 0.7), r_cor = c(0.2, 0.5), Ocena = c("Akceptowalny", "Dobry"))
#' make_ctt_plot(df, "Przyklad")
#'
#' @export
make_ctt_plot <- function(item_stats, label = "Caly test") {
  ggplot2::ggplot(item_stats, ggplot2::aes(x = .data$Trudnosc_p, y = .data$r_cor)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Ocena), size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Item), size = 2.5, vjust = -1, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0.30, linetype = "dashed", color = "green4", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0.20, linetype = "dashed", color = "orange", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = paste("Mapa jakosci itemow CTT -", label),
      x = "Trudnosc (p)",
      y = "Korelacja item-test (r.cor)",
      color = "Ocena"
    ) +
    ggplot2::theme_minimal()
}

# ============================================================================
# ANALIZA CTT
# ============================================================================

#' @title Analiza klasycznej teorii testów (CTT) dla zestawu itemow
#'
#' @description Oblicza podstawowe wskazniki CTT dla przekazanych itemow, w tym rzetelnosc, statystyki itemow, klasyfikacje jakosci itemow oraz obiekt wykresu mapy itemow. 
#'
#' @param data_items Ramka danych lub macierz z odpowiedziami na itemy. Kolumny odpowiadaja itemom, a wiersze osobom.
#' @param label Etykieta analizy zapisywana w wyniku i uzywana przy tworzeniu obiektow wykresow.
#' @param correlation_threshold_negative Prog korelacji `r_cor`, ponizej ktorego itemy sa zwracane jako slabe w polu `weak_items`.
#'
#' @return Lista zawierajaca status, statystyki rzetelnosci, statystyki itemow, podsumowania, wykresy i oczyszczone dane itemowe.
#'
#' @examples
#' data_items <- data.frame(i1 = c(0, 1, 1), i2 = c(1, 1, 0), i3 = c(0, 0, 1))
#' run_ctt_for_items(data_items, "Przyklad")
#'
#' @export
run_ctt_for_items <- function(
    data_items,
    label = "Caly test",
    correlation_threshold_negative = 0
) {

  vars <- sapply(data_items, var, na.rm = TRUE)
  zero_var <- names(vars[vars == 0 | is.na(vars)])

  if (length(zero_var) > 0) {
    data_items <- data_items[, !names(data_items) %in% zero_var, drop = FALSE]
  }

  if (ncol(data_items) < 3) {
    return(list(
      status = make_status(FALSE, "too_few_items", "Za malo itemow do analizy CTT (minimum 3)."),
      label = label,
      zero_variance_items = zero_var,
      data_items = data_items
    ))
  }

  complete_rows <- rowSums(!is.na(data_items)) > 0
  data_items <- data_items[complete_rows, , drop = FALSE]

  if (nrow(data_items) < 10) {
    return(list(
      status = make_status(FALSE, "too_few_observations", "Za malo obserwacji do analizy CTT (minimum 10)."),
      label = label,
      zero_variance_items = zero_var,
      data_items = data_items
    ))
  }

  alpha_result <- tryCatch(
    suppressWarnings(psych::alpha(data_items, check.keys = FALSE)),
    error = function(e) e
  )

  if (inherits(alpha_result, "error")) {
    return(list(
      status = make_status(FALSE, "alpha_error", conditionMessage(alpha_result)),
      label = label,
      zero_variance_items = zero_var,
      data_items = data_items
    ))
  }

  omega_val <- tryCatch({
    om <- suppressWarnings(psych::omega(data_items, nfactors = 1, plot = FALSE, warnings = FALSE))
    om$omega.tot
  }, error = function(e) NA_real_)

  alpha_val <- alpha_result$total$raw_alpha

  reliability_df <- data.frame(
    Wskaznik = c("Alpha Cronbacha", "Omega McDonalda"),
    Wartosc = c(round(alpha_val, 3), round(omega_val, 3)),
    Interpretacja = c(interpret_alpha(alpha_val), interpret_omega(omega_val)),
    stringsAsFactors = FALSE
  )

  reliability_df <- reliability_df[!is.na(reliability_df$Wartosc), , drop = FALSE]

  item_stats <- data.frame(
    Item = rownames(alpha_result$item.stats),
    N = colSums(!is.na(data_items)),
    Trudnosc_p = round(colMeans(data_items, na.rm = TRUE), 3),
    r_cor = round(alpha_result$item.stats$r.cor, 3),
    r_drop = round(alpha_result$item.stats$r.drop, 3),
    Alpha_bez_itemu = round(alpha_result$alpha.drop$raw_alpha, 3),
    stringsAsFactors = FALSE
  )

  item_stats$Ocena <- ""
  item_stats$Ocena[item_stats$r_cor < 0] <- "USUN (ujemna korelacja)"
  item_stats$Ocena[item_stats$r_cor >= 0 & item_stats$r_cor < 0.10] <- "Slaby"
  item_stats$Ocena[item_stats$r_cor >= 0.10 & item_stats$r_cor < 0.20] <- "Watpliwy"
  item_stats$Ocena[item_stats$r_cor >= 0.20 & item_stats$r_cor < 0.30] <- "Akceptowalny"
  item_stats$Ocena[item_stats$r_cor >= 0.30] <- "Dobry"
  item_stats$Ocena[is.na(item_stats$r_cor)] <- "Brak danych"

  ocena_counts <- as.data.frame(table(Ocena = item_stats$Ocena), stringsAsFactors = FALSE)
  names(ocena_counts)[2] <- "Liczba_itemow"

  ctt_plot <- make_ctt_plot(item_stats, label)

  list(
    status = make_status(TRUE, "ok", NA_character_),
    label = label,
    n_persons = nrow(data_items),
    n_items = ncol(data_items),
    zero_variance_items = zero_var,
    reliability = reliability_df,
    alpha = alpha_val,
    omega = omega_val,
    alpha_result = alpha_result,
    item_stats = item_stats,
    ocena_counts = ocena_counts,
    weak_items = item_stats$Item[item_stats$r_cor < correlation_threshold_negative],
    plots = list(ctt_map = ctt_plot),
    data_items = data_items
  )
}

# ============================================================================
# SEKWENCYJNA ELIMINACJA ITEMOW
# ============================================================================

#' @title Sekwencyjna eliminacja itemow wedlug korelacji item-test
#'
#' @description Wykonuje kolejne kroki eliminacji itemow na podstawie progow skorygowanej korelacji item-test i zwraca historie zmian wraz z finalnym zestawem itemow.
#'
#' @param data_items Ramka danych lub macierz z odpowiedziami na itemy.
#' @param label Etykieta analizy zapisywana w wyniku.
#' @param thresholds Wektor liczbowy z kolejnymi progami korelacji `r_cor` uzywanymi do usuwania itemow.
#'
#' @return Lista zawierajaca status, tabele krokow eliminacji, nazwy pozostalych i usunietych itemow oraz finalne dane.
#'
#' @examples
#' data_items <- data.frame(i1 = c(0, 1, 1), i2 = c(1, 1, 0), i3 = c(0, 0, 1))
#' sequential_elimination(data_items)
#'
#' @export
sequential_elimination <- function(
    data_items,
    label = "Caly test",
    thresholds = c(0, 0.10, 0.15)
) {

  if (ncol(data_items) < 3) {
    return(list(
      status = make_status(FALSE, "too_few_items", "Za malo itemow do sekwencyjnej eliminacji (minimum 3)."),
      label = label,
      steps = data.frame(),
      remaining_items = names(data_items),
      removed_items = character(0),
      final_data = data_items
    ))
  }

  complete_rows <- rowSums(!is.na(data_items)) > 0
  data_items <- data_items[complete_rows, , drop = FALSE]

  steps <- list()
  current_data <- data_items
  all_removed <- character(0)

  default_step_names <- c(
    "Ujemne korelacje (r <= 0)",
    "Bardzo niska korelacja (r <= 0.10)",
    "Niska korelacja (r <= 0.15)"
  )

  step_names <- if (length(thresholds) <= length(default_step_names)) {
    default_step_names[seq_along(thresholds)]
  } else {
    paste0("Korelacja r <= ", thresholds)
  }

  for (s in seq_along(thresholds)) {
    if (ncol(current_data) < 3) break

    alpha_res <- tryCatch(
      suppressWarnings(psych::alpha(current_data, check.keys = FALSE)),
      error = function(e) NULL
    )

    if (is.null(alpha_res)) break

    r_cors <- alpha_res$item.stats$r.cor
    names(r_cors) <- rownames(alpha_res$item.stats)

    to_remove <- names(r_cors[!is.na(r_cors) & r_cors <= thresholds[s]])
    to_remove_na <- names(r_cors[is.na(r_cors)])
    to_remove <- unique(c(to_remove, to_remove_na))

    steps[[length(steps) + 1]] <- data.frame(
      Krok = step_names[s],
      Threshold = thresholds[s],
      N_itemow = ncol(current_data),
      Alpha = alpha_res$total$raw_alpha,
      Usunieto = ifelse(length(to_remove) == 0, NA_character_, paste(to_remove, collapse = ", ")),
      stringsAsFactors = FALSE
    )

    if (length(to_remove) > 0) {
      current_data <- current_data[, !names(current_data) %in% to_remove, drop = FALSE]
      all_removed <- c(all_removed, to_remove)
    }
  }

  if (ncol(current_data) >= 3) {
    alpha_final <- tryCatch(
      suppressWarnings(psych::alpha(current_data, check.keys = FALSE)),
      error = function(e) NULL
    )

    if (!is.null(alpha_final)) {
      steps[[length(steps) + 1]] <- data.frame(
        Krok = "Po eliminacji",
        Threshold = NA_real_,
        N_itemow = ncol(current_data),
        Alpha = alpha_final$total$raw_alpha,
        Usunieto = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  steps_df <- if (length(steps) > 0) do.call(rbind, steps) else data.frame()
  if (nrow(steps_df) > 0) steps_df$Alpha <- round(steps_df$Alpha, 3)

  list(
    status = make_status(TRUE, "ok", NA_character_),
    label = label,
    steps = steps_df,
    remaining_items = names(current_data),
    removed_items = all_removed,
    final_data = current_data
  )
}

# ============================================================================
# PARAMETRY IRT
# ============================================================================

#' @title Przygotowanie tabeli parametrow IRT
#'
#' @description Pobiera parametry itemow z dopasowanego modelu IRT i porzadkuje je w ramce danych zawierajacej dyskryminacje, trudnosc, zgadywanie oraz interpretacje dyskryminacji.
#'
#' @param model Dopasowany model IRT z pakietu `mirt`.
#' @param model_name Nazwa modelu zapisywana w tabeli, np. `"1PL"`, `"2PL"` lub `"3PL"`.
#'
#' @return Ramka danych z parametrami itemow IRT.
#'
#' @examples
#' # model <- mirt::mirt(data_items, 1, itemtype = "2PL")
#' # make_params_table(model, "2PL")
#'
#' @export
make_params_table <- function(model, model_name = NA_character_) {

  item_params <- mirt::coef(model, simplify = TRUE, IRTpars = TRUE)$items

  params_df <- data.frame(
    Item = rownames(item_params),
    Model = model_name,
    a_dyskryminacja = if ("a" %in% colnames(item_params)) round(item_params[, "a"], 3) else NA_real_,
    b_trudnosc = if ("b" %in% colnames(item_params)) round(item_params[, "b"], 3) else NA_real_,
    g_zgadywanie = if ("g" %in% colnames(item_params)) round(item_params[, "g"], 3) else NA_real_,
    stringsAsFactors = FALSE
  )

  params_df$Ocena_a <- ""

  has_a <- !is.na(params_df$a_dyskryminacja)

  params_df$Ocena_a[has_a & params_df$a_dyskryminacja < 0.20] <- "Bardzo slaby"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.20 & params_df$a_dyskryminacja < 0.50] <- "Slaby"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.50 & params_df$a_dyskryminacja < 0.80] <- "Umiarkowany"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.80 & params_df$a_dyskryminacja < 1.50] <- "Dobry"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 1.50] <- "Bardzo dobry"
  params_df$Ocena_a[!has_a] <- NA_character_

  return(params_df)
}

#' @title Przygotowanie wykresow dla wybranego modelu IRT
#'
#' @description Tworzy liste obiektow wykresow dla wybranego modelu IRT, obejmujaca funkcje informacyjna testu, krzywe charakterystyczne itemow, opcjonalne dopasowanie empiryczne ICC oraz histogram theta.
#'
#' @param preferred_model Dopasowany model IRT wybrany do interpretacji.
#' @param data_items Ramka danych lub macierz itemow uzytych w modelu.
#' @param theta_vals Wektor liczbowy z oszacowaniami theta.
#' @param label Etykieta analizy uzywana w tytulach wykresow.
#' @param preferred_name Nazwa wybranego modelu IRT.
#' @param show_empirical_icc Wartosc logiczna okreslajaca, czy przygotowac wykres empirycznego dopasowania ICC.
#'
#' @return Lista obiektow wykresow.
#'
#' @examples
#' # make_irt_plots(model, data_items, theta_vals, "Caly test", "2PL")
#'
#' @export
make_irt_plots <- function(preferred_model, data_items, theta_vals, label, preferred_name, show_empirical_icc = TRUE) {
  plots <- list()

  plots$test_information <- mirt::plot(
    preferred_model,
    type = "info",
    facet_items = FALSE,
    main = paste("Funkcja informacyjna testu -", label, "-", preferred_name)
  )

  n_per_plot <- min(ncol(data_items), 12)
  trace_plots <- list()

  for (start_idx in seq(1, ncol(data_items), by = n_per_plot)) {
    end_idx <- min(start_idx + n_per_plot - 1, ncol(data_items))
    plot_name <- paste0("items_", start_idx, "_", end_idx)

    trace_plots[[plot_name]] <- mirt::plot(
      preferred_model,
      type = "trace",
      which.items = start_idx:end_idx,
      main = paste("ICC -", label, "-", preferred_name, "- itemy", start_idx, "do", end_idx),
      facet_items = TRUE,
      auto.key = list(points = FALSE, lines = TRUE)
    )
  }

  plots$item_traces <- trace_plots

  if (show_empirical_icc) {
    plots$empirical_icc <- make_empirical_icc_plot(
      preferred_model,
      data_items,
      theta_vals,
      label,
      preferred_name
    )
  }

  theta_df <- data.frame(theta = theta_vals)
  plots$theta_histogram <- ggplot2::ggplot(theta_df, ggplot2::aes(x = .data$theta)) +
    ggplot2::geom_histogram(bins = 30, color = "white") +
    ggplot2::geom_vline(xintercept = mean(theta_vals, na.rm = TRUE), linewidth = 1) +
    ggplot2::labs(
      title = paste("Rozklad theta (EAP) -", label, "-", preferred_name),
      x = "Zdolnosc (theta)",
      y = "Liczba osob"
    ) +
    ggplot2::theme_minimal()

  return(plots)
}

#' @title Przygotowanie wykresu empirycznego dopasowania ICC
#'
#' @description Porownuje przewidywane krzywe charakterystyczne itemow z empirycznymi srednimi odpowiedziami w grupach theta i zwraca jeden obiekt ggplot z panelami dla itemow.
#'
#' @param model Dopasowany model IRT z pakietu `mirt`.
#' @param data_items Ramka danych lub macierz odpowiedzi itemowych.
#' @param theta_vals Wektor liczbowy z oszacowaniami theta dla osob.
#' @param label Etykieta analizy uzywana w tytule wykresu.
#' @param model_name Nazwa modelu IRT uzywana w tytule wykresu.
#'
#' @return Obiekt klasy `ggplot` albo `NULL`, gdy nie da sie utworzyc grup theta.
#'
#' @examples
#' # make_empirical_icc_plot(model, data_items, theta_vals)
#'
#' @export
make_empirical_icc_plot <- function(model, data_items, theta_vals, label = "Caly test", model_name = "IRT") {
  breaks_theta <- unique(quantile(theta_vals, probs = seq(0, 1, 0.1), na.rm = TRUE))

  if (length(breaks_theta) < 3) {
    return(NULL)
  }

  theta_groups <- cut(theta_vals, breaks = breaks_theta, include.lowest = TRUE)

  empirical_list <- list()
  predicted_list <- list()
  theta_grid <- seq(min(theta_vals, na.rm = TRUE), max(theta_vals, na.rm = TRUE), length.out = 200)

  for (i in seq_len(ncol(data_items))) {
    item_name <- colnames(data_items)[i]
    tmp <- data.frame(
      item = item_name,
      theta = theta_vals,
      group = theta_groups,
      response = data_items[, i]
    )

    emp <- stats::aggregate(
      x = list(theta = tmp$theta, response = tmp$response),
      by = list(item = tmp$item, group = tmp$group),
      FUN = mean,
      na.rm = TRUE
    )

    item_obj <- mirt::extract.item(model, i)
    prob <- mirt::probtrace(item_obj, Theta = matrix(theta_grid))[, 2]

    pred <- data.frame(
      item = item_name,
      theta = theta_grid,
      probability = prob,
      stringsAsFactors = FALSE
    )

    empirical_list[[item_name]] <- emp
    predicted_list[[item_name]] <- pred
  }

  empirical_df <- do.call(rbind, empirical_list)
  predicted_df <- do.call(rbind, predicted_list)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = predicted_df,
      ggplot2::aes(x = .data$theta, y = .data$probability),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = empirical_df,
      ggplot2::aes(x = .data$theta, y = .data$response),
      size = 1.7
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$item)) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = paste("Empiryczne dopasowanie ICC -", label, "-", model_name),
      x = "Theta",
      y = "P(poprawnej)"
    ) +
    ggplot2::theme_minimal()
}
# ============================================================================
# ANALIZA IRT
# ============================================================================

#' @title Analiza IRT dla zestawu itemow
#'
#' @description Dopasowuje modele IRT 1PL, 2PL oraz opcjonalnie 3PL, porownuje ich dopasowanie, wybiera model preferowany na podstawie testow LRT i zwraca parametry itemow, wyniki theta oraz obiekty wykresow.
#'
#' @param data_items Ramka danych lub macierz z odpowiedziami na itemy.
#' @param label Etykieta analizy zapisywana w wyniku i uzywana przy tworzeniu wykresow.
#' @param show_plots Wartosc logiczna okreslajaca, czy przygotowac obiekty wykresow IRT.
#' @param show_empirical_icc Wartosc logiczna okreslajaca, czy przygotowac wykres empirycznego dopasowania ICC.
#'
#' @return Lista zawierajaca status, modele IRT, porownania modeli, parametry itemow, wyniki theta, wykresy i dane uzyte w analizie.
#'
#' @examples
#' # run_irt_for_items(data_items, "Caly test", show_plots = FALSE)
#'
#' @export
run_irt_for_items <- function(
    data_items,
    label = "Caly test",
    show_plots = TRUE,
    show_empirical_icc = TRUE
) {

  vars <- sapply(data_items, var, na.rm = TRUE)
  data_items <- data_items[, vars > 0 & !is.na(vars), drop = FALSE]

  complete_rows <- rowSums(!is.na(data_items)) > 0
  data_items <- data_items[complete_rows, , drop = FALSE]

  if (ncol(data_items) < 3 || nrow(data_items) < 50) {
    return(list(
      status = make_status(FALSE, "too_few_data", "Za malo danych do analizy IRT (min. 3 itemy, 50 osob)."),
      label = label,
      data_items = data_items
    ))
  }

  n_items <- ncol(data_items)
  model_spec <- paste0("F = 1-", n_items)

  model_1pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "1PL", verbose = FALSE),
    error = function(e) e
  )

  if (inherits(model_1pl, "error")) {
    return(list(
      status = make_status(FALSE, "model_1pl_error", conditionMessage(model_1pl)),
      label = label,
      data_items = data_items
    ))
  }

  model_2pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "2PL", verbose = FALSE),
    error = function(e) e
  )

  if (inherits(model_2pl, "error")) {
    return(list(
      status = make_status(FALSE, "model_2pl_error", conditionMessage(model_2pl)),
      label = label,
      model_1pl = model_1pl,
      data_items = data_items
    ))
  }

  model_3pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "3PL", verbose = FALSE),
    error = function(e) NULL
  )

  if (!is.null(model_3pl)) {
    anova_result <- anova(model_1pl, model_2pl, model_3pl)
  } else {
    anova_result <- anova(model_1pl, model_2pl)
  }

  comparison_df <- data.frame(
    Model = rownames(anova_result),
    AIC = round(anova_result$AIC, 1),
    BIC = round(anova_result$BIC, 1),
    LogLik = round(anova_result$logLik, 1),
    df = anova_result$df,
    stringsAsFactors = FALSE
  )

  lrt_labels <- c("1PL vs 2PL", "2PL vs 3PL")[seq_len(nrow(anova_result) - 1)]

  lrt_df <- data.frame(
    Porownanie = lrt_labels,
    Chi2 = round(anova_result$X2[-1], 2),
    df = anova_result$df[-1] - anova_result$df[-nrow(anova_result)],
    p = round(anova_result$p[-1], 4),
    stringsAsFactors = FALSE
  )

  preferred_model <- model_1pl
  preferred_name <- "1PL"

  p_val_2pl <- lrt_df$p[1]

  if (!is.na(p_val_2pl) && p_val_2pl < 0.05) {
    preferred_model <- model_2pl
    preferred_name <- "2PL"

    if (!is.null(model_3pl) && nrow(lrt_df) >= 2) {
      p_val_3pl <- lrt_df$p[2]

      if (!is.na(p_val_3pl) && p_val_3pl < 0.05) {
        preferred_model <- model_3pl
        preferred_name <- "3PL"
      }
    }
  }

  params_1pl_df <- make_params_table(model_1pl, "1PL")
  params_2pl_df <- make_params_table(model_2pl, "2PL")
  params_3pl_df <- if (!is.null(model_3pl)) make_params_table(model_3pl, "3PL") else NULL

  theta_scores <- mirt::fscores(preferred_model, method = "EAP")
  theta_vals <- theta_scores[, 1]

  plots <- if (show_plots) {
    make_irt_plots(preferred_model, data_items, theta_vals, label, preferred_name, show_empirical_icc)
  } else {
    list()
  }

  list(
    status = make_status(TRUE, "ok", NA_character_),
    label = label,
    n_persons = nrow(data_items),
    n_items = ncol(data_items),
    model_spec = model_spec,
    model_1pl = model_1pl,
    model_2pl = model_2pl,
    model_3pl = model_3pl,
    preferred_model = preferred_model,
    preferred_name = preferred_name,
    comparison_df = comparison_df,
    params_1pl_df = params_1pl_df,
    params_2pl_df = params_2pl_df,
    params_3pl_df = params_3pl_df,
    theta_scores = theta_scores,
    anova = anova_result,
    lrt = lrt_df,
    plots = plots,
    data_items = data_items
  )
}

# ============================================================================
# ITEM FIT
# ============================================================================

#' @title Analiza dopasowania itemow w modelu IRT
#'
#' @description Oblicza statystyki dopasowania itemow dla wybranego modelu IRT, w tym S-X2 oraz infit/outfit, a opcjonalnie takze PV-Q1*. Funkcja zwraca tabele i wykresy bez ich wyswietlania.
#'
#' @param irt_result Lista wynikowa zwrocona przez `run_irt_for_items()`.
#' @param label Etykieta analizy zapisywana w wyniku i uzywana w tytule wykresu.
#' @param run_pvq1 Wartosc logiczna okreslajaca, czy probowac obliczyc statystyke PV-Q1*.
#' @param pvq1_n_max Maksymalna liczba obserwacji, dla ktorej obliczana jest PV-Q1*.
#' @param pvq1_items_max Maksymalna liczba itemow, dla ktorej obliczana jest PV-Q1*.
#'
#' @return Lista zawierajaca status, surowe wyniki dopasowania, ramki danych ze statystykami, status PV-Q1* oraz obiekt wykresu.
#'
#' @examples
#' # irt <- run_irt_for_items(data_items, show_plots = FALSE)
#' # run_item_fit(irt)
#'
#' @export
run_item_fit <- function(
    irt_result,
    label = "Caly test",
    run_pvq1 = FALSE,
    pvq1_n_max = 500,
    pvq1_items_max = 30
) {

  if (is.null(irt_result) || isFALSE(irt_result$status$ok[1])) {
    return(list(
      status = make_status(FALSE, "no_irt_model", "Brak modelu IRT - pominieto item fit."),
      label = label
    ))
  }

  model <- irt_result$preferred_model
  model_name <- irt_result$preferred_name

  sx2_result <- tryCatch(
    mirt::itemfit(model, fit_stats = "S_X2"),
    error = function(e) e
  )

  sx2_df <- NULL
  sx2_status <- make_status(TRUE, "ok", NA_character_)

  if (inherits(sx2_result, "error")) {
    sx2_status <- make_status(FALSE, "sx2_error", conditionMessage(sx2_result))
    sx2_result <- NULL
  } else {
    sx2_col <- grep("S_X2", names(sx2_result), value = TRUE)[1]
    df_col <- grep("df", names(sx2_result), value = TRUE)[1]
    p_col <- grep("^p", names(sx2_result), value = TRUE)[1]

    if (!is.na(sx2_col) && !is.na(df_col) && !is.na(p_col)) {
      sx2_df <- data.frame(
        Item = rownames(sx2_result),
        S_X2 = round(sx2_result[[sx2_col]], 3),
        df = sx2_result[[df_col]],
        p = round(sx2_result[[p_col]], 4),
        stringsAsFactors = FALSE
      )

      sx2_df$Dopasowanie <- ifelse(
        sx2_df$p >= 0.05,
        "OK",
        ifelse(sx2_df$p >= 0.01, "Watpliwe", "Zle")
      )
    }
  }

  infit_result <- tryCatch(
    mirt::itemfit(model, fit_stats = "infit"),
    error = function(e) e
  )

  infit_df <- NULL
  infit_status <- make_status(TRUE, "ok", NA_character_)

  if (inherits(infit_result, "error")) {
    infit_status <- make_status(FALSE, "infit_error", conditionMessage(infit_result))
    infit_result <- NULL
  } else {
    infit_col <- grep("infit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]
    outfit_col <- grep("outfit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]

    if (!is.na(infit_col) && !is.na(outfit_col)) {
      infit_df <- data.frame(
        Item = rownames(infit_result),
        Infit_MNSQ = round(infit_result[[infit_col]], 3),
        Outfit_MNSQ = round(infit_result[[outfit_col]], 3),
        stringsAsFactors = FALSE
      )

      infit_df$Infit_ocena <- ifelse(
        infit_df$Infit_MNSQ >= 0.70 & infit_df$Infit_MNSQ <= 1.30,
        "OK",
        ifelse(infit_df$Infit_MNSQ > 1.30, "Niedodopasowanie", "Przeddopasowanie")
      )

      infit_df$Outfit_ocena <- ifelse(
        infit_df$Outfit_MNSQ >= 0.70 & infit_df$Outfit_MNSQ <= 1.30,
        "OK",
        ifelse(infit_df$Outfit_MNSQ > 1.30, "Niedodopasowanie", "Przeddopasowanie")
      )
    }
  }

  n_obs <- nrow(irt_result$data_items)
  n_items_fit <- ncol(irt_result$data_items)
  pvq1_result <- NULL
  pvq1_status <- make_status(FALSE, "pvq1_skipped", "PV-Q1* pominieto.")

  if (run_pvq1 && n_obs <= pvq1_n_max && n_items_fit <= pvq1_items_max) {
    pvq1_result <- tryCatch(
      mirt::itemfit(model, fit_stats = "PV_Q1*"),
      error = function(e) e
    )

    if (inherits(pvq1_result, "error")) {
      pvq1_status <- make_status(FALSE, "pvq1_error", conditionMessage(pvq1_result))
      pvq1_result <- NULL
    } else {
      pvq1_status <- make_status(TRUE, "ok", NA_character_)
    }
  }

  fit_plot_data <- NULL
  p_fit <- NULL

  if (!is.null(infit_result) && !is.null(sx2_result)) {
    p_col <- grep("^p", names(sx2_result), value = TRUE)[1]
    infit_col <- grep("infit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]
    outfit_col <- grep("outfit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]

    if (!is.na(p_col) && !is.na(infit_col) && !is.na(outfit_col)) {
      fit_plot_data <- data.frame(
        Item = rownames(infit_result),
        Infit = infit_result[[infit_col]],
        Outfit = infit_result[[outfit_col]],
        S_X2_p = sx2_result[[p_col]],
        stringsAsFactors = FALSE
      )

      p_fit <- ggplot2::ggplot(fit_plot_data, ggplot2::aes(x = .data$Infit, y = .data$Outfit)) +
        ggplot2::geom_point(ggplot2::aes(color = .data$S_X2_p < 0.05), size = 3) +
        ggplot2::geom_text(ggplot2::aes(label = .data$Item), size = 2, vjust = -1, alpha = 0.6) +
        ggplot2::geom_hline(yintercept = c(0.70, 1.30), linetype = "dashed", color = "red", alpha = 0.5) +
        ggplot2::geom_vline(xintercept = c(0.70, 1.30), linetype = "dashed", color = "red", alpha = 0.5) +
        ggplot2::labs(
          title = paste("Mapa dopasowania itemow -", label),
          x = "Infit MNSQ",
          y = "Outfit MNSQ",
          color = "S-X2 p < 0.05"
        ) +
        ggplot2::theme_minimal()
    }
  }

  list(
    status = make_status(TRUE, "ok", NA_character_),
    label = label,
    model_name = model_name,
    sx2 = sx2_result,
    sx2_df = sx2_df,
    sx2_status = sx2_status,
    infit = infit_result,
    infit_df = infit_df,
    infit_status = infit_status,
    pvq1 = pvq1_result,
    pvq1_status = pvq1_status,
    fit_plot_data = fit_plot_data,
    plots = list(fit_map = p_fit)
  )
}

# ============================================================================
# ANALIZA DIF
# ============================================================================

#' @title Analiza DIF dla pary grup
#'
#' @description Wykonuje logistyczna analize DIF dla dwoch wskazanych grup, korzystajac z wektora theta jako wyniku kontrolnego, i zwraca klasyfikacje ETS oraz wykres roznic.
#'
#' @param data_items Ramka danych lub macierz z odpowiedziami itemowymi.
#' @param group_vec Wektor z przynaleznoscia osob do grup.
#' @param theta_vec Wektor liczbowy z oszacowaniami theta.
#' @param group_ref Wartosc identyfikujaca grupe referencyjna.
#' @param group_focal Wartosc identyfikujaca grupe fokalna.
#' @param label Etykieta porownania grup.
#' @param model_name Opcjonalna nazwa modelu, z ktorego pochodza oszacowania theta.
#'
#' @return Lista zawierajaca status, informacje o grupach, wyniki DIF, ramke danych DIF, wykres i dane uzyte w analizie.
#'
#' @examples
#' # run_dif_pair(data_items, group_vec, theta_vec, "K", "M", "K vs M")
#'
#' @export
run_dif_pair <- function(
    data_items,
    group_vec,
    theta_vec,
    group_ref,
    group_focal,
    label,
    model_name = NULL
) {

  mask <- group_vec %in% c(group_ref, group_focal) & !is.na(group_vec)

  d_items <- data_items[mask, , drop = FALSE]
  d_group <- group_vec[mask]
  d_theta <- theta_vec[mask]

  good_items <- sapply(d_items, function(x) {
    var(x, na.rm = TRUE) > 0 && sum(!is.na(x)) >= 20
  })

  d_items <- d_items[, good_items, drop = FALSE]

  if (ncol(d_items) < 3) {
    return(list(
      status = make_status(FALSE, "too_few_items", "Za malo wspolnych itemow do analizy DIF."),
      label = label,
      group_ref = group_ref,
      group_focal = group_focal,
      model_name = model_name
    ))
  }

  complete <- rowSums(!is.na(d_items)) > 0

  d_items <- d_items[complete, , drop = FALSE]
  d_group <- d_group[complete]
  d_theta <- d_theta[complete]

  group_numeric <- ifelse(d_group == group_ref, 0, 1)

  dif_result <- tryCatch(
    sirt::dif.logistic.regression(
      dat = d_items,
      score = d_theta,
      group = group_numeric
    ),
    error = function(e) e
  )

  if (inherits(dif_result, "error")) {
    return(list(
      status = make_status(FALSE, "dif_error", conditionMessage(dif_result)),
      label = label,
      group_ref = group_ref,
      group_focal = group_focal,
      model_name = model_name
    ))
  }

  dif_df <- data.frame(
    Item = dif_result$item,
    pdiff_adj = round(dif_result$pdiff.adj, 4),
    ETS = dif_result$DIF.ETS,
    stringsAsFactors = FALSE
  )

  dif_df$Interpretacja <- ""
  dif_df$Interpretacja[dif_df$ETS == "A"] <- "Pomijalne DIF"
  dif_df$Interpretacja[dif_df$ETS == "B"] <- "Umiarkowane DIF"
  dif_df$Interpretacja[dif_df$ETS == "C"] <- "Duze DIF"
  dif_df$pdiff_for_plot <- dif_result$pdiff.adj

  p_dif <- NULL

  if (nrow(dif_df) > 0) {
    p_dif <- ggplot2::ggplot(
      dif_df,
      ggplot2::aes(x = stats::reorder(.data$Item, .data$pdiff_for_plot), y = .data$pdiff_for_plot, fill = .data$ETS)
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_hline(
        yintercept = c(-1.5, -1, 1, 1.5),
        linetype = "dashed",
        color = c("red", "orange", "orange", "red"),
        alpha = 0.5
      ) +
      ggplot2::scale_fill_manual(values = c("A" = "green4", "B" = "orange", "C" = "red")) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste("DIF:", label),
        x = "Item",
        y = "Adjusted p-difference",
        fill = "Klasyfikacja ETS"
      ) +
      ggplot2::theme_minimal()
  }

  list(
    status = make_status(TRUE, "ok", NA_character_),
    label = label,
    group_ref = group_ref,
    group_focal = group_focal,
    n_ref = sum(group_numeric == 0),
    n_focal = sum(group_numeric == 1),
    n_items = ncol(d_items),
    model_name = model_name,
    dif_result = dif_result,
    dif_df = dif_df,
    plots = list(dif = p_dif),
    data_items = d_items
  )
}

#' @title Analiza DIF dla wszystkich par grup
#'
#' @description Uruchamia analize DIF dla wszystkich par wartosci zmiennej grupujacej, osobno dla wersji testu lub dla calego zestawu danych, zaleznie od ustawien.
#'
#' @param raw_data Ramka danych z danymi zrodlowymi, w tym zmienna grupujaca.
#' @param items_data Ramka danych lub macierz z odpowiedziami itemowymi.
#' @param irt_results Lista wynikow IRT, zwykle zawierajaca element `all` lub elementy dla wersji testu.
#' @param dif_group_var Nazwa zmiennej grupujacej uzywanej w analizie DIF.
#' @param group_var Alternatywna nazwa zmiennej grupujacej, uzywana gdy `has_groups = TRUE`.
#' @param has_groups Wartosc logiczna informujaca, czy dane maja zdefiniowana zmienna grupujaca.
#' @param has_versions Wartosc logiczna informujaca, czy analize wykonac osobno dla wykrytych wersji testu.
#' @param detected_version_col Nazwa kolumny w `raw_data` zawierajacej wykryta wersje testu.
#'
#' @return Lista zawierajaca status, nazwe zmiennej grupujacej, poziomy grup oraz wyniki analiz DIF dla par grup.
#'
#' @examples
#' # run_dif_analysis(raw_data, items_data, list(all = irt), dif_group_var = "plec")
#'
#' @export
run_dif_analysis <- function(
    raw_data,
    items_data,
    irt_results,
    dif_group_var = NULL,
    group_var = NULL,
    has_groups = FALSE,
    has_versions = FALSE,
    detected_version_col = "detected_version"
) {

  if (is.null(dif_group_var) && has_groups) {
    dif_group_var <- group_var
  }

  if (is.null(dif_group_var) || !dif_group_var %in% names(raw_data)) {
    return(list(
      status = make_status(FALSE, "missing_group_var", "Analiza DIF pominieta - nie podano zmiennej grupujacej."),
      results = list()
    ))
  }

  dif_groups <- raw_data[[dif_group_var]]
  unique_groups <- sort(unique(dif_groups[!is.na(dif_groups)]))

  if (length(unique_groups) < 2) {
    return(list(
      status = make_status(FALSE, "too_few_groups", "Zmienna grupujaca ma mniej niz 2 unikalne wartosci."),
      results = list()
    ))
  }

  dif_results <- list()

  if (has_versions) {

    for (v in names(irt_results)) {

      label_v <- ifelse(v == "all", "", paste(" (Wersja", v, ")"))
      v_irt <- irt_results[[v]]
      if (is.null(v_irt) || isFALSE(v_irt$status$ok[1])) next

      v_data <- v_irt$data_items
      v_theta <- v_irt$theta_scores[, 1]

      if (v != "all") {
        if (!detected_version_col %in% names(raw_data)) next
        v_rows <- which(raw_data[[detected_version_col]] == as.numeric(v))
        v_kept <- v_rows %in% as.integer(rownames(v_data))
        v_group <- dif_groups[v_rows][v_kept]
      } else {
        v_complete <- rowSums(!is.na(items_data[, colnames(v_data), drop = FALSE])) > 0
        v_group <- dif_groups[v_complete]
      }

      v_unique <- sort(unique(v_group[!is.na(v_group)]))

      if (length(v_unique) >= 2) {
        for (i in 1:(length(v_unique) - 1)) {
          for (j in (i + 1):length(v_unique)) {

            pair_label <- paste(v_unique[i], "vs", v_unique[j], label_v)

            result <- run_dif_pair(
              v_data,
              v_group,
              v_theta,
              v_unique[i],
              v_unique[j],
              pair_label,
              model_name = v_irt$preferred_name
            )

            dif_results[[pair_label]] <- result
          }
        }
      }
    }

  } else {

    irt_all <- irt_results[["all"]]

    if (!is.null(irt_all) && isTRUE(irt_all$status$ok[1])) {

      theta_all <- irt_all$theta_scores[, 1]
      data_all <- irt_all$data_items

      complete_rows <- rowSums(!is.na(items_data[, colnames(data_all), drop = FALSE])) > 0
      dif_group_filtered <- dif_groups[complete_rows]

      for (i in 1:(length(unique_groups) - 1)) {
        for (j in (i + 1):length(unique_groups)) {

          pair_label <- paste(unique_groups[i], "vs", unique_groups[j])

          result <- run_dif_pair(
            data_all,
            dif_group_filtered,
            theta_all,
            unique_groups[i],
            unique_groups[j],
            pair_label,
            model_name = irt_all$preferred_name
          )

          dif_results[[pair_label]] <- result
        }
      }
    }
  }

  list(
    status = make_status(TRUE, "ok", NA_character_),
    dif_group_var = dif_group_var,
    groups = unique_groups,
    results = dif_results
  )
}

# ============================================================================
# EKSPORT DO EXCELA
# ============================================================================

#' @title Eksport wynikow analiz psychometrycznych do Excela
#'
#' @description Zbiera wyniki analiz CTT, IRT, item fit, DIF oraz wyniki osob i zapisuje je jako osobne arkusze w pliku XLSX. Funkcja zwraca metadane eksportu zamiast wypisywac komunikaty do raportu.
#'
#' @param ctt_results Lista wynikow CTT, zwykle zwroconych przez `run_ctt_for_items()`.
#' @param irt_results Lista wynikow IRT, zwykle zwroconych przez `run_irt_for_items()`.
#' @param fit_results Lista wynikow item fit, zwykle zwroconych przez `run_item_fit()`.
#' @param dif_results Lista wynikow DIF albo obiekt zwrocony przez `run_dif_analysis()`.
#' @param raw_data Ramka danych z danymi zrodlowymi.
#' @param data_path Sciezka do pliku danych wejsciowych, uzywana do domyslnej nazwy pliku wynikowego.
#' @param id_var Opcjonalna nazwa zmiennej identyfikatora osoby.
#' @param group_var Opcjonalna nazwa zmiennej grupujacej dopisywanej do wynikow osob.
#' @param has_versions Wartosc logiczna informujaca, czy dopisywac wykryta wersje testu do wynikow osob.
#' @param detected_version_col Nazwa kolumny w `raw_data` zawierajacej wykryta wersje testu.
#' @param output_file Opcjonalna sciezka do pliku XLSX. Gdy `NULL`, nazwa jest tworzona na podstawie `data_path`.
#'
#' @return Lista zawierajaca status eksportu, sciezke do pliku wynikowego oraz nazwy utworzonych arkuszy.
#'
#' @examples
#' # export_results_to_excel(ctt_results, irt_results, raw_data = raw_data, data_path = "dane.csv")
#'
#' @export
export_results_to_excel <- function(
    ctt_results,
    irt_results,
    fit_results = list(),
    dif_results = list(),
    raw_data,
    data_path,
    id_var = NULL,
    group_var = NULL,
    has_versions = FALSE,
    detected_version_col = "detected_version",
    output_file = NULL
) {

  export_sheets <- list()

  for (name in names(ctt_results)) {
    r <- ctt_results[[name]]
    if (is.null(r) || isFALSE(r$status$ok[1])) next

    label <- ifelse(name == "all", "CTT", paste0("CTT_v", name))
    export_sheets[[safe_sheet_name(label)]] <- r$item_stats
  }

  for (name in names(irt_results)) {
    r <- irt_results[[name]]
    if (is.null(r) || isFALSE(r$status$ok[1])) next

    export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_1PL", paste0("IRT_1PL_v", name)))]] <- r$params_1pl_df
    export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_2PL", paste0("IRT_2PL_v", name)))]] <- r$params_2pl_df

    if (!is.null(r$params_3pl_df)) {
      export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_3PL", paste0("IRT_3PL_v", name)))]] <- r$params_3pl_df
    }
  }

  person_scores_all <- list()

  for (name in names(irt_results)) {

    r <- irt_results[[name]]
    if (is.null(r) || isFALSE(r$status$ok[1])) next

    d <- r$data_items
    row_ids <- match(rownames(d), rownames(raw_data))

    raw_score <- rowSums(d, na.rm = TRUE)
    n_answered <- rowSums(!is.na(d))
    percent_score <- raw_score / n_answered

    fs <- mirt::fscores(
      r$preferred_model,
      method = "EAP",
      full.scores.SE = TRUE
    )

    theta <- fs[, 1]
    theta_se <- if (ncol(fs) >= 2) fs[, 2] else NA_real_

    person_df <- data.frame(
      raw_score = raw_score,
      percent_score = round(percent_score, 4),
      raw_score_z = round(z_score(raw_score), 4),
      percent_score_z = round(z_score(percent_score), 4),
      theta_EAP = round(theta, 4),
      theta_SE = round(theta_se, 4),
      theta_z = round(z_score(theta), 4),
      wybrany_model = r$preferred_name,
      stringsAsFactors = FALSE
    )

    if (!is.null(id_var) && id_var %in% names(raw_data)) {
      person_df <- cbind(
        ID = raw_data[[id_var]][row_ids],
        person_df
      )
    }

    if (!is.null(group_var) && group_var %in% names(raw_data)) {
      person_df$grupa <- raw_data[[group_var]][row_ids]
    }

    if (has_versions && detected_version_col %in% names(raw_data)) {
      person_df$wersja <- raw_data[[detected_version_col]][row_ids]
    }

    person_scores_all[[name]] <- person_df
  }

  if (length(person_scores_all) > 0) {
    export_sheets[["Wyniki_osob"]] <- do.call(rbind, person_scores_all)
  }

  for (name in names(fit_results)) {

    r <- fit_results[[name]]
    if (is.null(r) || isFALSE(r$status$ok[1])) next

    label <- ifelse(name == "all", "ItemFit", paste0("ItemFit_v", name))

    if (!is.null(r$sx2_df) && !is.null(r$infit_df)) {
      fit_combined <- merge(
        r$sx2_df[, c("Item", "S_X2", "p")],
        r$infit_df[, c("Item", "Infit_MNSQ", "Outfit_MNSQ")],
        by = "Item",
        all = TRUE
      )

      names(fit_combined) <- c("Item", "S_X2", "p_SX2", "Infit", "Outfit")
      export_sheets[[safe_sheet_name(label)]] <- fit_combined
    }
  }

  dif_list <- if (!is.null(dif_results$results)) dif_results$results else dif_results

  if (length(dif_list) > 0) {
    for (name in names(dif_list)) {
      r <- dif_list[[name]]
      if (is.data.frame(r)) {
        export_sheets[[safe_sheet_name(paste0("DIF_", name))]] <- r
      } else if (is.list(r) && !is.null(r$dif_df) && isTRUE(r$status$ok[1])) {
        export_sheets[[safe_sheet_name(paste0("DIF_", name))]] <- r$dif_df
      }
    }
  }

  if (is.null(output_file)) {
    output_file <- sub("\\.[^.]+$", "_wyniki.xlsx", basename(data_path))
  }

  writexl::write_xlsx(export_sheets, path = output_file)

  list(
    status = make_status(TRUE, "ok", NA_character_),
    output_file = output_file,
    sheets = names(export_sheets)
  )
}
