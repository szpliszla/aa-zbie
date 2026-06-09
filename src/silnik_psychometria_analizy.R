# ============================================================================
# FUNKCJE POMOCNICZE - RAPORT PSYCHOMETRYCZNY
# ============================================================================
# ============================================================================
# ANALIZA CTT
# ============================================================================

run_ctt_for_items <- function(
    data_items,
    label = "Caly test",
    correlation_threshold_negative = 0
) {

  vars <- sapply(data_items, var, na.rm = TRUE)
  zero_var <- names(vars[vars == 0 | is.na(vars)])

  if (length(zero_var) > 0) {
    cat("Wykluczono", length(zero_var), "itemow z zerowa wariancja:",
        paste(zero_var, collapse = ", "), "\n\n")
    data_items <- data_items[, !names(data_items) %in% zero_var, drop = FALSE]
  }

  if (ncol(data_items) < 3) {
    cat("Za malo itemow do analizy CTT (minimum 3).\n\n")
    return(NULL)
  }

  complete_rows <- rowSums(!is.na(data_items)) > 0
  data_items <- data_items[complete_rows, , drop = FALSE]

  if (nrow(data_items) < 10) {
    cat("Za malo obserwacji do analizy CTT (minimum 10).\n\n")
    return(NULL)
  }

  cat("### CTT:", label, "\n\n")
  cat("- N osob:", nrow(data_items), "\n")
  cat("- N itemow:", ncol(data_items), "\n\n")

  alpha_result <- tryCatch(
    psych::alpha(data_items, check.keys = FALSE),
    error = function(e) {
      cat("**Blad w obliczaniu alpha:**", e$message, "\n\n")
      return(NULL)
    }
  )

  if (is.null(alpha_result)) return(NULL)

  omega_val <- tryCatch({
    om <- psych::omega(data_items, nfactors = 1, plot = FALSE, warnings = FALSE)
    om$omega.tot
  }, error = function(e) NA)

  alpha_val <- alpha_result$total$raw_alpha

  alpha_interp <- ifelse(alpha_val >= 0.9, "Doskonala",
                         ifelse(alpha_val >= 0.8, "Dobra",
                                ifelse(alpha_val >= 0.7, "Akceptowalna",
                                       ifelse(alpha_val >= 0.6, "Watpliwa", "Niska"))))

  cat("#### Rzetelnosc\n\n")
  cat("| Wskaznik | Wartosc | Interpretacja |\n")
  cat("|----------|---------|---------------|\n")
  cat(sprintf("| Alpha Cronbacha | %.3f | %s |\n", alpha_val, alpha_interp))

  if (!is.na(omega_val)) {
    omega_interp <- ifelse(omega_val >= 0.8, "Dobra",
                           ifelse(omega_val >= 0.7, "Akceptowalna", "Niska"))
    cat(sprintf("| Omega McDonalda | %.3f | %s |\n", omega_val, omega_interp))
  }

  cat("\n")

  item_stats <- data.frame(
    Item = rownames(alpha_result$item.stats),
    N = colSums(!is.na(data_items)),
    Trudnosc_p = round(colMeans(data_items, na.rm = TRUE), 3),
    r_cor = round(alpha_result$item.stats$r.cor, 3),
    r_drop = round(alpha_result$item.stats$r.drop, 3),
    Alpha_bez_itemu = round(alpha_result$alpha.drop$raw_alpha, 3)
  )

  item_stats$Ocena <- ""
  item_stats$Ocena[item_stats$r_cor < 0] <- "USUN (ujemna korelacja)"
  item_stats$Ocena[item_stats$r_cor >= 0 & item_stats$r_cor < 0.10] <- "Slaby"
  item_stats$Ocena[item_stats$r_cor >= 0.10 & item_stats$r_cor < 0.20] <- "Watpliwy"
  item_stats$Ocena[item_stats$r_cor >= 0.20 & item_stats$r_cor < 0.30] <- "Akceptowalny"
  item_stats$Ocena[item_stats$r_cor >= 0.30] <- "Dobry"
  #LC: fix — jawna etykieta dla itemów z brakującym r_cor
  item_stats$Ocena[is.na(item_stats$r_cor)] <- "Brak danych"

  print(knitr::kable(item_stats, row.names = FALSE))
  cat("\n")

  ocena_counts <- as.data.frame(table(Ocena = item_stats$Ocena))
  names(ocena_counts)[2] <- "Liczba_itemow"
  print(knitr::kable(ocena_counts, row.names = FALSE))
  cat("\n")

  p <- ggplot2::ggplot(item_stats, ggplot2::aes(x = Trudnosc_p, y = r_cor)) +
    ggplot2::geom_point(ggplot2::aes(color = Ocena), size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = Item), size = 2.5, vjust = -1, alpha = 0.7) +
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

  print(p)
  cat("\n\n")

  return(list(
    alpha = alpha_val,
    omega = omega_val,
    item_stats = item_stats,
    weak_items = item_stats$Item[item_stats$r_cor < correlation_threshold_negative],
    data_items = data_items
  ))
}

# ============================================================================
# SEKWENCYJNA ELIMINACJA ITEMOW
# ============================================================================

sequential_elimination <- function(
    data_items,
    label = "Caly test",
    thresholds = c(0, 0.10, 0.15)
) {

  if (ncol(data_items) < 3) return(NULL)

  complete_rows <- rowSums(!is.na(data_items)) > 0
  data_items <- data_items[complete_rows, , drop = FALSE]

  steps <- list()
  current_data <- data_items
  all_removed <- c()

  step_names <- c(
    "Ujemne korelacje (r <= 0)",
    "Bardzo niska korelacja (r <= 0.10)", #fix: kod robi "<=" zamiast "<"
    "Niska korelacja (r <= 0.15)" #fix: j.w.
  )

  for (s in seq_along(thresholds)) {
    if (ncol(current_data) < 3) break

    alpha_res <- tryCatch(
      psych::alpha(current_data, check.keys = FALSE),
      error = function(e) NULL
    )

    if (is.null(alpha_res)) break

    r_cors <- alpha_res$item.stats$r.cor
    names(r_cors) <- rownames(alpha_res$item.stats)

    to_remove <- names(r_cors[!is.na(r_cors) & r_cors <= thresholds[s]])
    to_remove_na <- names(r_cors[is.na(r_cors)])
    to_remove <- unique(c(to_remove, to_remove_na))

    alpha_val <- alpha_res$total$raw_alpha

    steps[[s]] <- list(
      step = step_names[s],
      n_items = ncol(current_data),
      alpha = alpha_val,
      removed = to_remove
    )

    if (length(to_remove) > 0) {
      current_data <- current_data[, !names(current_data) %in% to_remove, drop = FALSE]
      all_removed <- c(all_removed, to_remove)
    }
  }

  if (ncol(current_data) >= 3) {
    alpha_final <- tryCatch(
      psych::alpha(current_data, check.keys = FALSE),
      error = function(e) NULL
    )

    if (!is.null(alpha_final)) {
      steps[[length(steps) + 1]] <- list(
        step = "Po eliminacji",
        n_items = ncol(current_data),
        alpha = alpha_final$total$raw_alpha,
        removed = character(0)
      )
    }
  }

  cat("####", label, "\n\n")
  cat("| Krok | N itemow | Alpha | Usunieto |\n")
  cat("|------|----------|-------|----------|\n")

  for (st in steps) {
    removed_str <- ifelse(length(st$removed) == 0, "-", paste(st$removed, collapse = ", "))
    cat(sprintf("| %s | %d | %.3f | %s |\n", st$step, st$n_items, st$alpha, removed_str))
  }

  cat("\n")

  return(list(
    remaining_items = names(current_data),
    removed_items = all_removed,
    final_data = current_data
  ))
}

# ============================================================================
# PARAMETRY IRT
# ============================================================================

make_params_table <- function(model, model_name) {

  item_params <- coef(model, simplify = TRUE, IRTpars = TRUE)$items

  params_df <- data.frame(
    Item = rownames(item_params),
    a_dyskryminacja = if ("a" %in% colnames(item_params)) round(item_params[, "a"], 3) else NA,
    b_trudnosc = if ("b" %in% colnames(item_params)) round(item_params[, "b"], 3) else NA,
    g_zgadywanie = if ("g" %in% colnames(item_params)) round(item_params[, "g"], 3) else NA
  )

  params_df$Ocena_a <- ""

  has_a <- !is.na(params_df$a_dyskryminacja)

  params_df$Ocena_a[has_a & params_df$a_dyskryminacja < 0.20] <- "Bardzo slaby"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.20 & params_df$a_dyskryminacja < 0.50] <- "Slaby"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.50 & params_df$a_dyskryminacja < 0.80] <- "Umiarkowany"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 0.80 & params_df$a_dyskryminacja < 1.50] <- "Dobry"
  params_df$Ocena_a[has_a & params_df$a_dyskryminacja >= 1.50] <- "Bardzo dobry"

  cat("##### Model", model_name, "\n\n")
  print(knitr::kable(params_df, row.names = FALSE))
  cat("\n")

  return(params_df)
}

# ============================================================================
# ANALIZA IRT
# ============================================================================

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
    cat("Za malo danych do analizy IRT (min. 3 itemy, 50 osob).\n\n")
    return(NULL)
  }

  cat("### IRT:", label, "\n\n")
  cat("- N osob:", nrow(data_items), "\n")
  cat("- N itemow:", ncol(data_items), "\n\n")

  n_items <- ncol(data_items)
  #LC: uproszczenie specyfikacji modelu
  model_spec <- paste0("F = 1-", n_items)

  cat("#### Model 1PL\n\n")

  model_1pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "1PL", verbose = FALSE),
    error = function(e) {
      cat("**Blad w dopasowaniu modelu 1PL:**", e$message, "\n\n")
      return(NULL)
    }
  )

  if (is.null(model_1pl)) return(NULL)

  cat("#### Model 2PL\n\n")

  model_2pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "2PL", verbose = FALSE),
    error = function(e) {
      cat("**Blad w dopasowaniu modelu 2PL:**", e$message, "\n\n")
      return(NULL)
    }
  )

  if (is.null(model_2pl)) return(NULL)

  cat("#### Model 3PL\n\n")

  model_3pl <- tryCatch(
    mirt::mirt(data_items, model = model_spec, itemtype = "3PL", verbose = FALSE),
    error = function(e) {
      cat("**Blad w dopasowaniu modelu 3PL:**", e$message, "\n\n")
      return(NULL)
    }
  )

  cat("#### Porownanie modeli 1PL vs 2PL vs 3PL\n\n")

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
    df = anova_result$df
  )

  print(knitr::kable(comparison_df, row.names = FALSE))
  cat("\n")

  cat("#### Testy ilorazu wiarogodnosci (LRT)\n\n")

  lrt_labels <- c("1PL vs 2PL", "2PL vs 3PL")[seq_len(nrow(anova_result) - 1)]

  lrt_df <- data.frame(
    Porownanie = lrt_labels,
    Chi2 = round(anova_result$X2[-1], 2),
    df = anova_result$df[-1] - anova_result$df[-nrow(anova_result)],
    p = round(anova_result$p[-1], 4)
  )

  print(knitr::kable(lrt_df, row.names = FALSE))
  cat("\n")

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

  cat("**Wybrany model:**", preferred_name, "\n\n")

  cat("#### Parametry itemow IRT\n\n")

  params_1pl_df <- make_params_table(model_1pl, "1PL")
  params_2pl_df <- make_params_table(model_2pl, "2PL")

  if (!is.null(model_3pl)) {
    params_3pl_df <- make_params_table(model_3pl, "3PL")
  } else {
    params_3pl_df <- NULL
  }

  theta_scores <- mirt::fscores(preferred_model, method = "EAP")
  theta_vals <- theta_scores[, 1]

  if (show_plots) {

    cat("#### Wykresy IRT dla wybranego modelu:", preferred_name, "\n\n")

    print(plot(
      preferred_model,
      type = "info",
      facet_items = FALSE,
      main = paste("Funkcja informacyjna testu -", label, "-", preferred_name)
    ))

    n_per_plot <- min(ncol(data_items), 12)

    for (start_idx in seq(1, ncol(data_items), by = n_per_plot)) {
      end_idx <- min(start_idx + n_per_plot - 1, ncol(data_items))

      print(plot(
        preferred_model,
        type = "trace",
        which.items = start_idx:end_idx,
        main = paste("ICC -", label, "-", preferred_name, "- itemy", start_idx, "do", end_idx),
        facet_items = TRUE,
        auto.key = list(points = FALSE, lines = TRUE)
      ))
    }

    if (show_empirical_icc) {

      cat("##### Empiryczne dopasowanie ICC do danych\n\n")

      breaks_theta <- unique(
        quantile(theta_vals, probs = seq(0, 1, 0.1), na.rm = TRUE)
      )

      if (length(breaks_theta) >= 3) {

        theta_groups <- cut(
          theta_vals,
          breaks = breaks_theta,
          include.lowest = TRUE
        )

        empirical_data <- data.frame(
          theta = theta_vals,
          group = theta_groups
        )

        n_per_plot_emp <- min(ncol(data_items), 6)

        for (start_idx in seq(1, ncol(data_items), by = n_per_plot_emp)) {

          end_idx <- min(start_idx + n_per_plot_emp - 1, ncol(data_items))

          par(mfrow = c(2, 3))

          for (i in start_idx:end_idx) {

            item_name <- colnames(data_items)[i]
            item_resp <- data_items[, i]

            tmp <- empirical_data
            tmp$response <- item_resp

            emp <- aggregate(
              cbind(theta, response) ~ group,
              data = tmp,
              FUN = mean,
              na.rm = TRUE
            )

            theta_grid <- seq(min(theta_vals), max(theta_vals), length.out = 200)

            item_obj <- mirt::extract.item(preferred_model, i)
            prob <- mirt::probtrace(item_obj, Theta = matrix(theta_grid))[, 2]

            plot(
              theta_grid,
              prob,
              type = "l",
              lwd = 2,
              ylim = c(0, 1),
              xlab = expression(theta),
              ylab = "P(poprawnej)",
              main = item_name
            )

            points(
              emp$theta,
              emp$response,
              pch = 19,
              col = "red"
            )
          }

          par(mfrow = c(1, 1))
        }
      }
    }

    hist(
      theta_vals,
      breaks = 30,
      col = "steelblue",
      border = "white",
      main = paste("Rozklad theta (EAP) -", label, "-", preferred_name),
      xlab = "Zdolnosc (theta)",
      ylab = "Liczba osob"
    )

    abline(v = mean(theta_vals), col = "red", lwd = 2)
  }

  return(list(
    model_1pl = model_1pl,
    model_2pl = model_2pl,
    model_3pl = model_3pl,
    preferred_model = preferred_model,
    preferred_name = preferred_name,
    params_1pl_df = params_1pl_df,
    params_2pl_df = params_2pl_df,
    params_3pl_df = params_3pl_df,
    theta_scores = theta_scores,
    anova = anova_result,
    lrt = lrt_df,
    data_items = data_items
  ))
}

# ============================================================================
# ITEM FIT
# ============================================================================

run_item_fit <- function(
    irt_result,
    label = "Caly test",
    run_pvq1 = FALSE,
    pvq1_n_max = 500,
    pvq1_items_max = 30
) {

  if (is.null(irt_result)) {
    cat("Brak modelu IRT - pominieto item fit.\n\n")
    return(NULL)
  }

  model <- irt_result$preferred_model
  model_name <- irt_result$preferred_name

  cat("### Item Fit:", label, "\n\n")
  cat("Item fit obliczono dla wybranego modelu IRT:", model_name, "\n\n")

  cat("#### Statystyka S-X2 (Orlando-Thissen)\n\n")

  sx2_result <- tryCatch(
    mirt::itemfit(model, fit_stats = "S_X2"),
    error = function(e) {
      cat("Blad w obliczaniu S-X2:", e$message, "\n\n")
      return(NULL)
    }
  )

  if (!is.null(sx2_result)) {

    sx2_col <- grep("S_X2", names(sx2_result), value = TRUE)[1]
    df_col <- grep("df", names(sx2_result), value = TRUE)[1]
    p_col <- grep("^p", names(sx2_result), value = TRUE)[1]

    if (!is.na(sx2_col) && !is.na(df_col) && !is.na(p_col)) {

      sx2_df <- data.frame(
        Item = rownames(sx2_result),
        S_X2 = round(sx2_result[[sx2_col]], 3),
        df = sx2_result[[df_col]],
        p = round(sx2_result[[p_col]], 4)
      )

      sx2_df$Dopasowanie <- ifelse(
        sx2_df$p >= 0.05,
        "OK",
        ifelse(sx2_df$p >= 0.01, "Watpliwe", "Zle")
      )

      print(knitr::kable(sx2_df, row.names = FALSE))
      cat("\n")
    }
  }

  cat("#### Statystyki Infit i Outfit\n\n")

  infit_result <- tryCatch(
    mirt::itemfit(model, fit_stats = "infit"),
    error = function(e) {
      cat("Blad w obliczaniu infit/outfit:", e$message, "\n\n")
      return(NULL)
    }
  )

  if (!is.null(infit_result)) {

    infit_col <- grep("infit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]
    outfit_col <- grep("outfit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]

    if (!is.na(infit_col) && !is.na(outfit_col)) {

      infit_df <- data.frame(
        Item = rownames(infit_result),
        Infit_MNSQ = round(infit_result[[infit_col]], 3),
        Outfit_MNSQ = round(infit_result[[outfit_col]], 3)
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

      print(knitr::kable(infit_df, row.names = FALSE))
      cat("\n")
    }
  }

  n_obs <- nrow(irt_result$data_items)
  n_items_fit <- ncol(irt_result$data_items)
  pvq1_result <- NULL

  if (run_pvq1 && n_obs <= pvq1_n_max && n_items_fit <= pvq1_items_max) {

    cat("#### Statystyka PV-Q1* (Chalmers & Ng, 2017)\n\n")

    pvq1_result <- tryCatch(
      mirt::itemfit(model, fit_stats = "PV_Q1*"),
      error = function(e) {
        cat("Blad w obliczaniu PV-Q1*:", e$message, "\n\n")
        return(NULL)
      }
    )

  } else {
    cat("#### PV-Q1* pominieto\n\n")
  }

  cat("#### Wykres zbiorczy item fit\n\n")

  if (!is.null(infit_result) && !is.null(sx2_result)) {

    p_col <- grep("^p", names(sx2_result), value = TRUE)[1]
    infit_col <- grep("infit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]
    outfit_col <- grep("outfit", names(infit_result), ignore.case = TRUE, value = TRUE)[1]

    if (!is.na(p_col) && !is.na(infit_col) && !is.na(outfit_col)) {

      fit_plot_data <- data.frame(
        Item = rownames(infit_result),
        Infit = infit_result[[infit_col]],
        Outfit = infit_result[[outfit_col]],
        S_X2_p = sx2_result[[p_col]]
      )

      p_fit <- ggplot2::ggplot(fit_plot_data, ggplot2::aes(x = Infit, y = Outfit)) +
        ggplot2::geom_point(ggplot2::aes(color = S_X2_p < 0.05), size = 3) +
        ggplot2::geom_text(ggplot2::aes(label = Item), size = 2, vjust = -1, alpha = 0.6) +
        ggplot2::geom_hline(yintercept = c(0.70, 1.30), linetype = "dashed", color = "red", alpha = 0.5) +
        ggplot2::geom_vline(xintercept = c(0.70, 1.30), linetype = "dashed", color = "red", alpha = 0.5) +
        ggplot2::labs(
          title = paste("Mapa dopasowania itemow -", label),
          x = "Infit MNSQ",
          y = "Outfit MNSQ",
          color = "S-X2 p < 0.05"
        ) +
        ggplot2::theme_minimal()

      print(p_fit)
      cat("\n\n")
    }
  }

  return(list(
    sx2 = sx2_result,
    infit = infit_result,
    pvq1 = pvq1_result
  ))
}

# ============================================================================
# ANALIZA DIF
# ============================================================================

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
    cat("Za malo wspolnych itemow do analizy DIF.\n\n")
    return(NULL)
  }

  complete <- rowSums(!is.na(d_items)) > 0

  d_items <- d_items[complete, , drop = FALSE]
  d_group <- d_group[complete]
  d_theta <- d_theta[complete]

  group_numeric <- ifelse(d_group == group_ref, 0, 1)

  cat("####", label, "\n\n")
  cat("- Grupa referencyjna (0):", group_ref, "(n =", sum(group_numeric == 0), ")\n")
  cat("- Grupa fokalna (1):", group_focal, "(n =", sum(group_numeric == 1), ")\n")
  cat("- Itemy:", ncol(d_items), "\n")

  if (!is.null(model_name)) {
    cat("- Theta oszacowano modelem:", model_name, "\n")
  }

  cat("\n")

  dif_result <- tryCatch(
    sirt::dif.logistic.regression(
      dat = d_items,
      score = d_theta,
      group = group_numeric
    ),
    error = function(e) {
      cat("**Blad w analizie DIF:**", e$message, "\n\n")
      return(NULL)
    }
  )

  if (is.null(dif_result)) return(NULL)

  dif_df <- data.frame(
    Item = dif_result$item,
    pdiff_adj = round(dif_result$pdiff.adj, 4),
    ETS = dif_result$DIF.ETS
  )

  dif_df$Interpretacja <- ""
  dif_df$Interpretacja[dif_df$ETS == "A"] <- "Pomijalne DIF"
  dif_df$Interpretacja[dif_df$ETS == "B"] <- "Umiarkowane DIF"
  dif_df$Interpretacja[dif_df$ETS == "C"] <- "Duze DIF"

  print(knitr::kable(dif_df, row.names = FALSE))
  cat("\n")

  if (nrow(dif_df) > 0) {

    dif_df$pdiff_for_plot <- dif_result$pdiff.adj

    p_dif <- ggplot2::ggplot(
      dif_df,
      ggplot2::aes(x = reorder(Item, pdiff_for_plot), y = pdiff_for_plot, fill = ETS)
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

    print(p_dif)
    cat("\n\n")
  }

  return(dif_df)
}

run_dif_analysis <- function(
    raw_data,
    items_data,
    irt_results,
    dif_group_var = NULL,
    group_var = NULL,
    has_groups = FALSE,
    has_versions = FALSE
) {

  if (is.null(dif_group_var) && has_groups) {
    dif_group_var <- group_var
  }

  if (is.null(dif_group_var) || !dif_group_var %in% names(raw_data)) {
    cat("**Analiza DIF pominieta** - nie podano zmiennej grupujacej.\n\n")
    return(list())
  }

  dif_groups <- raw_data[[dif_group_var]]
  unique_groups <- sort(unique(dif_groups[!is.na(dif_groups)]))

  if (length(unique_groups) < 2) {
    cat("**BLAD:** Zmienna grupujaca ma mniej niz 2 unikalne wartosci.\n\n")
    return(list())
  }

  dif_results <- list()

  if (has_versions) {

    for (v in names(irt_results)) {

      label_v <- ifelse(v == "all", "", paste(" (Wersja", v, ")"))
      v_irt <- irt_results[[v]]
      v_data <- v_irt$data_items
      v_theta <- v_irt$theta_scores[, 1]

      if (v != "all") {
        v_rows <- which(raw_data$detected_version == as.numeric(v))
        #LC: fix — filtracja v_group do wierszy zachowanych po IRT
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

            if (!is.null(result)) {
              dif_results[[pair_label]] <- result
            }
          }
        }
      }
    }

  } else {

    irt_all <- irt_results[["all"]]

    if (!is.null(irt_all)) {

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

          if (!is.null(result)) {
            dif_results[[pair_label]] <- result
          }
        }
      }
    }
  }

  return(dif_results)
}

# ============================================================================
# EKSPORT DO EXCELA
# ============================================================================

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
    output_file = NULL
) {

  export_sheets <- list()

  for (name in names(ctt_results)) {
    label <- ifelse(name == "all", "CTT", paste0("CTT_v", name))
    export_sheets[[safe_sheet_name(label)]] <- ctt_results[[name]]$item_stats
  }

  for (name in names(irt_results)) {
    r <- irt_results[[name]]

    export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_1PL", paste0("IRT_1PL_v", name)))]] <- r$params_1pl_df
    export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_2PL", paste0("IRT_2PL_v", name)))]] <- r$params_2pl_df

    if (!is.null(r$params_3pl_df)) {
      export_sheets[[safe_sheet_name(ifelse(name == "all", "IRT_3PL", paste0("IRT_3PL_v", name)))]] <- r$params_3pl_df
    }
  }

  person_scores_all <- list()

  for (name in names(irt_results)) {

    r <- irt_results[[name]]
    d <- r$data_items

    #LC: fix — match() zamiast as.integer() dla nienumerycznych rownames
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
    theta_se <- if (ncol(fs) >= 2) fs[, 2] else NA

    person_df <- data.frame(
      raw_score = raw_score,
      percent_score = round(percent_score, 4),
      raw_score_z = round(z_score(raw_score), 4),
      percent_score_z = round(z_score(percent_score), 4),
      theta_EAP = round(theta, 4),
      theta_SE = round(theta_se, 4),
      theta_z = round(z_score(theta), 4),
      wybrany_model = r$preferred_name
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

    if (has_versions && "detected_version" %in% names(raw_data)) {
      person_df$wersja <- raw_data$detected_version[row_ids]
    }

    person_scores_all[[name]] <- person_df
  }

  if (length(person_scores_all) > 0) {
    export_sheets[["Wyniki_osob"]] <- do.call(rbind, person_scores_all)
  }

  for (name in names(fit_results)) {

    r <- fit_results[[name]]
    label <- ifelse(name == "all", "ItemFit", paste0("ItemFit_v", name))

    if (!is.null(r$sx2) && !is.null(r$infit)) {

      sx2_col <- grep("S_X2", names(r$sx2), value = TRUE)[1]
      p_col <- grep("^p", names(r$sx2), value = TRUE)[1]
      infit_col <- grep("infit", names(r$infit), ignore.case = TRUE, value = TRUE)[1]
      outfit_col <- grep("outfit", names(r$infit), ignore.case = TRUE, value = TRUE)[1]

      if (!is.na(sx2_col) && !is.na(p_col) &&
          !is.na(infit_col) && !is.na(outfit_col)) {

        fit_combined <- merge(
          data.frame(
            Item = rownames(r$sx2),
            S_X2 = round(r$sx2[[sx2_col]], 3),
            p_SX2 = round(r$sx2[[p_col]], 4)
          ),
          data.frame(
            Item = rownames(r$infit),
            Infit = round(r$infit[[infit_col]], 3),
            Outfit = round(r$infit[[outfit_col]], 3)
          ),
          by = "Item",
          all = TRUE
        )

        export_sheets[[safe_sheet_name(label)]] <- fit_combined
      }
    }
  }

  if (length(dif_results) > 0) {
    for (name in names(dif_results)) {
      export_sheets[[safe_sheet_name(paste0("DIF_", name))]] <- dif_results[[name]]
    }
  }

  if (is.null(output_file)) {
    output_file <- sub("\\.[^.]+$", "_wyniki.xlsx", basename(data_path))
  }

  writexl::write_xlsx(export_sheets, path = output_file)

  cat(sprintf("Wyniki zapisane do: **%s**\n\n", output_file))

  return(output_file)
}
