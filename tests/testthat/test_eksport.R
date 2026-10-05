test_that("excel_file_name dodaje sufiks _results_ ze znacznikiem czasu", {
  czas <- as.POSIXct("2026-10-05 10:28:19")

  expect_identical(
    excel_file_name("dane/math_data.csv", czas),
    "math_data_results_2026-10-05_102819.xlsx"
  )
  expect_match(excel_file_name("dane.xlsx"), "^dane_results_\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.xlsx$")
})

test_that("export_results_to_excel tlumaczy naglowki i wartosci logiczne", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  wczytaj_tlumaczenia("en")

  ok <- make_status(TRUE, "ok", NA_character_)
  ctt <- list(all = list(
    status = ok,
    item_stats = data.frame(Item = "i1", Trudnosc_p = 0.5, Alpha_bez_itemu = 0.7)
  ))
  dif <- list(results = list(a_vs_b = list(
    status = ok,
    dif_df = data.frame(
      Item = c("i1", "i2"),
      DIF_signal = c(TRUE, FALSE),
      Interpretacja = c("x", "y")
    )
  )))
  plik <- tempfile(fileext = ".xlsx")
  on.exit(unlink(plik), add = TRUE)

  wynik <- export_results_to_excel(
    ctt_results = ctt, irt_results = list(), dif_results = dif,
    raw_data = data.frame(i1 = 1), data_path = "dane.csv",
    output_file = plik
  )

  expect_true(wynik$status$ok)
  expect_identical(names(readxl::read_excel(plik, "CTT")), c("Item", "Difficulty_p", "Alpha_if_deleted"))
  arkusz_dif <- readxl::read_excel(plik, wynik$sheets[2])
  expect_identical(names(arkusz_dif), c("Item", "DIF_signal", "Interpretation"))
  expect_identical(arkusz_dif$DIF_signal, c("Yes", "No"))
})

test_that("nazwa arkusza wynikow osob jest tlumaczona", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)

  wczytaj_tlumaczenia("en")
  expect_identical(t("export.sheet_person_scores"), "Person_scores")
  wczytaj_tlumaczenia("pl")
  expect_identical(t("export.sheet_person_scores"), "Wyniki_osob")
})
