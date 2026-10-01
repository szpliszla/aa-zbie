# Testy jednostkowe kluczowych poprawek z przeglądu testowego
# Obejmują: K3, K4, P2, P3, D4, D10, D11, 4.2

# -- helpers --

load_mixed_data <- function() {
  dp <- test_path("../../inst/extdata/mixed_data.csv")
  if (!file.exists(dp)) {
    dp <- test_path("../../00_pkg_src/aazbie/inst/extdata/mixed_data.csv")
  }
  d <- read_psych_data(dp)
  item_cols <- identify_item_columns(d, "zad_")
  val <- validate_items_data(d, item_cols)
  list(raw = d, val = val)
}

load_binary_data <- function() {
  dp <- test_path("../../inst/extdata/math_data.csv")
  if (!file.exists(dp)) {
    dp <- test_path("../../00_pkg_src/aazbie/inst/extdata/math_data.csv")
  }
  d <- read_psych_data(dp)
  item_cols <- identify_item_columns(d, "mat_")
  val <- validate_items_data(d, item_cols)
  list(raw = d, val = val)
}

# ==========================================================================
# K3: get_model_ic — LogLik i df poprawne dla modelu politomicznego
# ==========================================================================

test_that("K3: IRT politomiczny zwraca poprawne LogLik i df", {
  m <- load_mixed_data()
  irt <- run_irt_for_items(
    m$val$items_data,
    item_type = m$val$item_type,
    item_max_scores = m$val$item_max_scores,
    show_plots = FALSE
  )
  expect_true(isTRUE(irt$status$ok))
  expect_false(is.null(irt$comparison_df))
  expect_true(all(!is.na(irt$comparison_df$LogLik)))
  expect_true(all(!is.na(irt$comparison_df$df)))
})

# ==========================================================================
# P2: run_item_fit — nazwy zadań, nie numery wierszy
# ==========================================================================

test_that("P2: item fit zwraca nazwy zadan w kolumnie Item", {
  m <- load_binary_data()
  irt <- run_irt_for_items(
    m$val$items_data,
    item_type = "binary",
    show_plots = FALSE
  )
  fit <- run_item_fit(irt, run_pvq1 = FALSE)
  expect_true(isTRUE(fit$status$ok))

  if (!is.null(fit$infit_df)) {
    items <- fit$infit_df$Item
    expect_false(all(items %in% as.character(seq_along(items))),
                 info = "Item powinien zawierać nazwy, nie numery")
    expect_true(all(items %in% colnames(irt$data_items)))
  }

  if (!is.null(fit$sx2_df)) {
    expect_true(all(fit$sx2_df$Item %in% colnames(irt$data_items)))
  }
})

# ==========================================================================
# P3: S-X2 z brakami danych — na.rm = TRUE
# ==========================================================================

# mirt liczy S-X2 tylko na pelnych wzorcach odpowiedzi (na.rm = TRUE usuwa
# wiersze z brakami). Przy kilku wersjach testu, jak w math_data, nie zostaje
# zaden pelny wiersz.
test_that("P3: S-X2 liczy sie przy sporadycznych brakach", {
  dat <- sim_binary(500, 10, seed = 6148)
  dat[matrix(stats::runif(500 * 10) < 0.02, nrow = 500)] <- NA
  n_complete <- sum(stats::complete.cases(dat))
  expect_gt(n_complete, 0)
  expect_lt(n_complete, nrow(dat))

  irt <- run_irt_for_items(dat, item_type = "binary", show_plots = FALSE)
  fit <- run_item_fit(irt, run_pvq1 = FALSE)

  expect_true(isTRUE(fit$sx2_status$ok), info = fit$sx2_status$message)
  expect_setequal(fit$sx2_df$Item, names(dat))
  expect_true(all(is.finite(fit$sx2_df$S_X2)))
  expect_true(all(is.finite(fit$sx2_df$p)))
  expect_equal(fit$sx2_na_info$N_complete, n_complete)
})

test_that("P3: bez pelnych wierszy S-X2 zwraca czytelny status", {
  dat <- sim_binary(400, 12, seed = 2957)
  zeszyt <- rep(1:2, each = 200)
  dat[zeszyt == 1, 7:12] <- NA
  dat[zeszyt == 2, 1:6] <- NA
  expect_false(any(stats::complete.cases(dat)))

  irt <- run_irt_for_items(dat, item_type = "binary", show_plots = FALSE)
  fit <- run_item_fit(irt, run_pvq1 = FALSE)

  expect_identical(fit$sx2_status$code, "sx2_no_complete_rows")
  expect_identical(
    fit$sx2_status$message,
    t("item_fit.msg.sx2_no_complete_rows")
  )
  expect_null(fit$sx2_df)
  expect_equal(fit$sx2_na_info$N_complete, 0)
  expect_true(isTRUE(fit$infit_status$ok))
})

# ==========================================================================
# K4: detect_test_versions — losowe braki nie dają fałszywych wersji
# ==========================================================================

test_that("K4: losowe braki nie powoduja falszywego podzialu na wersje", {
  set.seed(7193)
  n <- 100
  p <- 10
  dat <- as.data.frame(matrix(
    sample(0:1, n * p, replace = TRUE),
    nrow = n
  ))
  names(dat) <- paste0("it_", seq_len(p))
  # Wstaw ~5% losowych braków
  for (j in seq_len(p)) {
    dat[sample(n, round(n * 0.05)), j] <- NA
  }

  res <- detect_test_versions(
    raw_data = dat,
    items_data = dat,
    item_cols = names(dat),
    max_unclassified_prop = 0.20
  )

  # Przy 5% braków i 10 itemach większość wzorców będzie unikalna,
  # więc >20% osób będzie unclassified i podział powinien się wyłączyć
  expect_false(res$has_multiple_versions)
})

test_that("K4: automatyczne wylaczenie podzialu przy >20% unclassified", {
  set.seed(2841)
  n <- 200
  p <- 15
  dat <- as.data.frame(matrix(
    sample(0:1, n * p, replace = TRUE),
    nrow = n
  ))
  names(dat) <- paste0("it_", seq_len(p))
  # Wstaw 10% losowych braków — wiele unikalnych wzorców
  for (j in seq_len(p)) {
    dat[sample(n, round(n * 0.10)), j] <- NA
  }

  res <- detect_test_versions(
    raw_data = dat,
    items_data = dat,
    item_cols = names(dat),
    max_unclassified_prop = 0.20
  )

  if (isTRUE(res$version_detection_safety$Auto_detection_disabled)) {
    expect_false(res$has_multiple_versions)
  }
})

# ==========================================================================
# 4.2: sequential_elimination — timeout
# ==========================================================================

test_that("4.2: sequential_elimination lapie timeout", {
  # Macierz na tyle duża, żeby alpha trwała >0.001s
  set.seed(5512)
  big <- as.data.frame(matrix(
    sample(0:1, 500 * 30, replace = TRUE),
    nrow = 500
  ))
  names(big) <- paste0("q", seq_len(30))

  res <- sequential_elimination(big, alpha_timeout = 0.001)

  # Timeout mógł wystąpić lub nie — jeśli wystąpił, status musi być poprawny
  if (res$status$code == "alpha_timeout") {
    expect_false(isTRUE(res$status$ok))
    expect_equal(res$status$message, sprintf(t("ctt.msg.alpha_timeout"), 0.001))
  } else {
    expect_true(isTRUE(res$status$ok))
  }
})

# ==========================================================================
# D4: make_params_table — Ocena_a = "Ustalone (1PL)" dla 1PL
# ==========================================================================

test_that("D4: Ocena_a dla 1PL mowi Ustalone", {
  m <- load_binary_data()
  irt <- run_irt_for_items(
    m$val$items_data,
    item_type = "binary",
    show_plots = FALSE
  )
  params_1pl <- irt$params_1pl_df
  expect_true(all(params_1pl$Ocena_a == t("irt.rating_fixed_1pl")))
})

# ==========================================================================
# D10: validate_items_data — komunikat dla luki w kategoriach
# ==========================================================================

test_that("D10: wykluczenie itemu z luka zawiera obserwowane wartosci", {
  dat <- data.frame(
    ok1 = c(0, 1, 0, 1, 0),
    ok2 = c(1, 0, 1, 1, 0),
    ok3 = c(0, 1, 1, 0, 1),
    bad = c(0, 2, 0, 2, 0)  # luka: brak 1
  )
  val <- validate_items_data(dat, names(dat))

  expect_true("bad" %in% val$non_binary_items)
  issues_text <- paste(val$validation_issues, collapse = " ")
  expect_true(grepl("0,2", issues_text) || grepl("0, 2", issues_text),
              info = "Komunikat powinien zawierac obserwowane wartosci")
})

# ==========================================================================
# D11: item_max_scores przefiltrowane po odsiewie zero-var
# ==========================================================================

test_that("D11: item_max_scores nie zawiera usunietych itemow", {
  dat <- data.frame(
    ok1 = c(0, 1, 0, 1, 0),
    ok2 = c(1, 0, 1, 1, 0),
    ok3 = c(0, 1, 1, 0, 1),
    zerovar = c(1, 1, 1, 1, 1)  # zerowa wariancja
  )
  val <- validate_items_data(dat, names(dat))

  expect_false("zerovar" %in% val$item_cols)
  expect_false("zerovar" %in% names(val$item_max_scores))
  expect_false("zerovar" %in% names(val$n_categories))
  expect_equal(length(val$item_max_scores), length(val$item_cols))
})
