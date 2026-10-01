# Polish phrases from tlumaczenia.csv found in the visible text of an HTML
# report (code blocks, scripts and styles are skipped). An English report should
# return character(0). Only texts that have a token in the CSV are detected.
polskie_frazy <- function(html_path, min_nchar = 12) {
  tr <- utils::read.csv(
    system.file("reports", "tlumaczenia.csv", package = "aazbie"),
    sep = ";", stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = character(0), fileEncoding = "UTF-8-BOM"
  )

  normalizuj <- function(s) {
    s <- gsub("[*`\\\\]", "", s)
    trimws(gsub("\\s+", " ", s))
  }

  # Fixed text between sprintf() placeholders
  fragmenty <- function(s) {
    trimws(unlist(strsplit(normalizuj(s), "%[-+ 0#]*[0-9]*(\\.[0-9]+)?[dsf]")))
  }

  pl <- unique(unlist(lapply(tr$pl, fragmenty)))
  en <- paste(normalizuj(tr$en), collapse = "\n")
  pl <- pl[nchar(pl) >= min_nchar & !vapply(pl, grepl, logical(1), x = en, fixed = TRUE)]

  html <- paste(readLines(html_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  html <- gsub("(?s)<(script|style|pre)\\b.*?</\\1>", " ", html, perl = TRUE)
  tekst <- gsub("<[^>]+>", "", html)
  encje <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&amp;" = "&")
  for (e in names(encje)) {
    tekst <- gsub(e, encje[[e]], tekst, fixed = TRUE)
  }
  tekst <- normalizuj(tekst)

  pl[vapply(pl, grepl, logical(1), x = tekst, fixed = TRUE)]
}
