test_that("domyslnym jezykiem jest angielski", {
  expect_identical(formals(wczytaj_tlumaczenia)$jezyk, "en")
  expect_identical(formals(render_report)$jezyk, "en")

  rmd <- system.file("reports", "psychometria_raport.Rmd", package = "aazbie")
  expect_identical(rmarkdown::yaml_front_matter(rmd)$params$jezyk, "en")

  # t() wczytuje jezyk domyslny tylko wtedy, gdy opcja nie jest ustawiona
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(t("irt.rating_fixed_1pl"), "Fixed (1PL)")
})
