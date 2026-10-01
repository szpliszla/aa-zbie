# DIF path in mirt (2PL + GPCM, uniform DIF only). The example reports never
# reach it: in mixed_data the "plec" groups have 89 and 111 people, while
# render_report() uses min_group_n = 100. Hence min_group_n = 50 here.

wczytaj_mixed <- function() {
  raw <- utils::read.csv(system.file("extdata", "mixed_data.csv", package = "aazbie"))
  list(raw = raw, items = raw[grep("^zad_", names(raw))])
}

test_that("run_dif_analysis(): DIF w mirt dla itemow mieszanych (2PL + GPCM)", {
  skip_on_cran()
  d <- wczytaj_mixed()
  irt <- run_irt_for_items(d$items, show_plots = FALSE)
  res <- run_dif_analysis(
    d$raw, d$items, list(all = irt),
    dif_group_var = "plec", min_group_n = 50
  )
  expect_true(res$status$ok)
  expect_length(res$results, 1)

  r <- res$results[[1]]
  expect_true(r$status$ok, info = r$status$message)
  expect_identical(r$method, "mirt_lrt")
  expect_identical(r$dif_model, "2PL + GPCM")
  # Thresholds only (no a1): uniform DIF
  expect_setequal(r$dif_parameters, c("d", "d1", "d2"))
  expect_true(r$base_converged)

  expect_equal(nrow(r$dif_df), 15)
  expect_false(anyNA(r$dif_df$p_holm))
  # One threshold per 2PL item, two per 3-category GPCM item
  expect_equal(r$dif_df$df, rep(c(1, 2), c(10, 5)))
  expect_type(r$dif_df$converged, "logical")
  expect_true(all(r$dif_df$converged))
  expect_length(r$items_not_converged, 0)
})

test_that("run_dif_pair(): DIF w mirt dla samych itemow politomicznych (GPCM)", {
  skip_on_cran()
  d <- wczytaj_mixed()
  poly <- d$items[grep("^zad_p", names(d$items))]
  r <- run_dif_pair(
    poly, d$raw$plec, theta_vec = rep(0, nrow(poly)),
    group_ref = 0, group_focal = 1, label = "0 vs 1",
    min_group_n = 50
  )
  expect_true(r$status$ok, info = r$status$message)
  expect_identical(r$item_type, "polytomous")
  expect_identical(r$dif_model, "GPCM")
  expect_setequal(r$dif_parameters, c("d1", "d2"))
  expect_equal(nrow(r$dif_df), 5)
  expect_false(anyNA(r$dif_df$p_holm))
  expect_type(r$dif_df$converged, "logical")
})
