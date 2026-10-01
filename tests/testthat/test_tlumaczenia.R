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

test_that("tokeny w tlumaczenia.csv sa unikalne i maja oba tlumaczenia", {
  tr <- utils::read.csv(
    system.file("reports", "tlumaczenia.csv", package = "aazbie"),
    sep = ";", stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = character(0), fileEncoding = "UTF-8-BOM"
  )
  expect_identical(tr$token[duplicated(tr$token)], character(0))
  expect_identical(tr$token[!nzchar(tr$en) | !nzchar(tr$pl)], character(0))
})

test_that("tokeny uzyte w pakiecie i szablonie sa w tlumaczenia.csv", {
  old <- options(tlumaczenia = NULL)
  on.exit(options(old), add = TRUE)
  tokeny_csv <- names(wczytaj_tlumaczenia("en"))

  ns <- asNamespace("aazbie")
  kod <- unlist(lapply(ls(ns, all.names = TRUE), function(nazwa) {
    obj <- get(nazwa, envir = ns)
    if (is.function(obj)) deparse(obj) else character(0)
  }))
  rmd <- readLines(
    system.file("reports", "psychometria_raport.Rmd", package = "aazbie"),
    encoding = "UTF-8", warn = FALSE
  )

  # Only literal tokens; dynamic ones (e.g. paste0("common.item_type.", x)) are skipped
  wzorzec <- "\\bt\\((['\"])([A-Za-z0-9_.]+)\\1\\)"
  zrodla <- c(kod, rmd)
  trafienia <- unlist(regmatches(zrodla, gregexpr(wzorzec, zrodla, perl = TRUE)))
  tokeny <- unique(sub(wzorzec, "\\2", trafienia, perl = TRUE))

  expect_gt(length(tokeny), 100)
  expect_identical(setdiff(tokeny, tokeny_csv), character(0))
})
