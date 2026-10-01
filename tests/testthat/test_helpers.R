test_that("fmt_table zwraca tabele kableExtra", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  wczytaj_tlumaczenia("en")

  wynik <- fmt_table(data.frame(Item = "i1", Trudnosc_p = 0.5))

  expect_s3_class(wynik, "kableExtra")
})

test_that("fmt_table tlumaczy naglowki kolumn przez tokeny col.*", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  wczytaj_tlumaczenia("en")

  wynik <- paste(fmt_table(data.frame(N_kategorii = 3L)), collapse = "\n")

  expect_match(wynik, "N_categories", fixed = TRUE)
  expect_no_match(wynik, "N_kategorii", fixed = TRUE)
})

test_that("fmt_table przekazuje dodatkowe argumenty dalej", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  wczytaj_tlumaczenia("en")

  wynik <- paste(fmt_table(data.frame(Wartosc = 0.123456), digits = 2), collapse = "\n")

  expect_match(wynik, "0.12", fixed = TRUE)
  expect_no_match(wynik, "0.123", fixed = TRUE)
})
