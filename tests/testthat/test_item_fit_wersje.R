test_that("sx2_fixed_pars odtwarza mirt::itemfit() na pelnych danych", {
  dat <- sim_binary(500, 8, seed = 7302)
  mod <- mirt::mirt(dat, 1, itemtype = "2PL", verbose = FALSE)

  direct <- mirt::itemfit(mod, fit_stats = "S_X2")
  fixed <- sx2_fixed_pars(mod, dat)

  expect_equal(fixed$S_X2, direct$S_X2)
  expect_equal(fixed$df.S_X2, direct$df.S_X2)
  expect_equal(fixed$p.S_X2, direct$p.S_X2)
})

test_that("S-X2 per wersja: kazdy item w swojej wersji, na osobach tej wersji", {
  b <- sim_booklets(seed = 4417)
  # Puste wiersze na poczatku sprawdzaja mapowanie wersji na wiersze, ktore
  # zostaja po odrzuceniu pustych wierszy w run_irt_for_items()
  dat <- rbind(b$dat[1:3, ] * NA, b$dat)
  zeszyt <- c(rep("B", 3), b$zeszyt)

  irt <- run_irt_for_items(dat, item_type = "binary", show_plots = FALSE)
  fit <- run_item_fit(irt, run_pvq1 = FALSE, versions = zeszyt)

  expect_identical(fit$sx2_status$code, "ok")
  expect_setequal(fit$sx2_df$Item, names(dat))
  expect_equal(anyDuplicated(fit$sx2_df$Item), 0)
  expect_setequal(fit$sx2_df$Item[fit$sx2_df$Wersja == "A"], paste0("it_", 1:6))
  expect_true(all(is.finite(fit$sx2_df$S_X2)))
  expect_equal(fit$sx2_na_info$Wersja, c("A", "B"))
  expect_equal(fit$sx2_na_info$N_total, c(200, 200))
  expect_equal(fit$sx2_na_info$N_complete, c(200, 200))
})

test_that("S-X2 per wersja z itemami wspolnymi; bledna dlugosc versions", {
  b <- sim_booklets(seed = 5830, anchors = 5:8)
  irt <- run_irt_for_items(b$dat, item_type = "binary", show_plots = FALSE)
  fit <- run_item_fit(irt, run_pvq1 = FALSE, versions = b$zeszyt)

  expect_identical(fit$sx2_status$code, "ok")
  expect_equal(nrow(fit$sx2_df), 16)
  expect_equal(
    as.vector(table(fit$sx2_df$Item)[paste0("it_", 5:8)]),
    rep(2, 4)
  )
  expect_equal(anyDuplicated(fit$fit_plot_data$Item), 0)

  expect_error(
    run_item_fit(irt, run_pvq1 = FALSE, versions = b$zeszyt[-1]),
    t("item_fit.msg.versions_length"),
    fixed = TRUE
  )
})

test_that("S-X2 per wersja: wersja bez pelnych wierszy daje status czesciowy", {
  b <- sim_booklets(seed = 3094)
  # W wersji B kazda osoba pomija inny item, wiec zaden wiersz B nie jest pelny
  rows_b <- which(b$zeszyt == "B")
  for (k in seq_along(rows_b)) {
    b$dat[rows_b[k], 7 + (k %% 6)] <- NA
  }

  irt <- run_irt_for_items(b$dat, item_type = "binary", show_plots = FALSE)
  fit <- run_item_fit(irt, run_pvq1 = FALSE, versions = b$zeszyt)

  expect_true(isTRUE(fit$sx2_status$ok))
  expect_identical(fit$sx2_status$code, "sx2_partial")
  expect_match(fit$sx2_status$message, "B (", fixed = TRUE)
  expect_setequal(fit$sx2_df$Item, paste0("it_", 1:6))
  expect_equal(fit$sx2_version_status$ok, c(TRUE, FALSE))
})
