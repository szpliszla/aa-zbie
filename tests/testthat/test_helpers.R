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

test_that("show_table wypisuje kod HTML tabeli", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  wczytaj_tlumaczenia("en")

  wynik <- paste(capture.output(show_table(data.frame(Trudnosc_p = 0.5))), collapse = "\n")

  expect_match(wynik, "<table", fixed = TRUE)
  expect_match(wynik, "Difficulty_p", fixed = TRUE)
})

test_that("szablon wyswietla tabele przez show_table, nie print(fmt_table())", {
  # print.kableExtra sends tables to the Viewer in interactive sessions,
  # so they would be missing from reports rendered from the RStudio console.
  rmd <- readLines(
    system.file("reports", "psychometria_raport.Rmd", package = "aazbie"),
    encoding = "UTF-8", warn = FALSE
  )

  expect_false(any(grepl("print(fmt_table(", rmd, fixed = TRUE)))
  expect_gt(sum(grepl("show_table(", rmd, fixed = TRUE)), 10)
})

test_that("t_logiczne zamienia kolumny logiczne na tekst w jezyku raportu", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  df <- data.frame(Item = c("i1", "i2", "i3"), DIF_signal = c(TRUE, FALSE, NA))

  wczytaj_tlumaczenia("en")
  expect_identical(t_logiczne(df)$DIF_signal, c("Yes", "No", NA))
  expect_identical(t_logiczne(df)$Item, df$Item)

  wczytaj_tlumaczenia("pl")
  expect_identical(t_logiczne(df)$DIF_signal, c("Tak", "Nie", NA))
})
