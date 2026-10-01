# ============================================================================
# REPORT TRANSLATIONS
# ============================================================================

#' @title Wczytanie tlumaczen raportu
#'
#' @description Wczytuje tlumaczenia z pliku CSV (separator `;`, kolumna
#' `token` oraz po jednej kolumnie na kazdy jezyk, np. `en`, `pl`), buduje
#' wektor tlumaczen w wybranym jezyku, nazwany tokenami, i zapisuje go jako
#' opcje konfiguracyjna `tlumaczenia`. Z opcji korzysta funkcja [t()].
#'
#' @param jezyk Kod jezyka odpowiadajacy nazwie kolumny w pliku tlumaczen,
#'   np. `"en"` (domyslnie) lub `"pl"`.
#' @param sciezka Opcjonalna sciezka do pliku CSV z tlumaczeniami. Domyslnie
#'   plik `tlumaczenia.csv` z katalogu `reports` zainstalowanego pakietu.
#'
#' @return Niewidocznie: nazwany wektor tekstowy z tlumaczeniami.
#'
#' @examples
#' wczytaj_tlumaczenia("pl")
#' t("common.whole_test")
#' wczytaj_tlumaczenia()
#'
#' @export
wczytaj_tlumaczenia <- function(jezyk = "en", sciezka = NULL) {
  if (is.null(sciezka)) {
    sciezka <- system.file("reports", "tlumaczenia.csv", package = "aazbie")
  }

  if (!nzchar(sciezka) || !file.exists(sciezka)) {
    stop("Nie znaleziono pliku z tlumaczeniami: ", sciezka, call. = FALSE)
  }

  # UTF-8-BOM reads files with and without a BOM (Excel saves "CSV UTF-8" with a BOM)
  tlumaczenia <- utils::read.csv(
    sciezka,
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(0),
    fileEncoding = "UTF-8-BOM")

  jezyki <- setdiff(names(tlumaczenia), "token")

  if (!jezyk %in% jezyki) {
    stop(
      "Nieobslugiwany jezyk: '", jezyk, "'. Dostepne: ",
      paste(jezyki, collapse = ", "),
      call. = FALSE
    )
  }

  tlumaczenia <- stats::setNames(tlumaczenia[[jezyk]], tlumaczenia$token)
  options(tlumaczenia = tlumaczenia)
  invisible(tlumaczenia)
}

#' @title Tlumaczenie tokena
#'
#' @description Zwraca tlumaczenie tokena w jezyku wybranym przez
#' [wczytaj_tlumaczenia()]. Jezeli tlumaczenia nie zostaly jeszcze wczytane,
#' wczytywany jest jezyk domyslny (angielski). Dla tokenow nieobecnych w pliku tlumaczen
#' zwracany jest sam token, dzieki czemu braki sa widoczne w raporcie.
#'
#' Funkcja przeslania `base::t()`. Dla argumentow nietekstowych oraz macierzy
#' przekazuje wywolanie do `base::t()`, wiec transpozycja dziala jak dotad.
#'
#' @param token Wektor tekstowy z tokenami.
#'
#' @return Wektor tekstowy z tlumaczeniami, tej samej dlugosci co `token`.
#'
#' @examples
#' t("common.whole_test")
#'
#' @export
t <- function(token) {
  if (!is.character(token) || !is.null(dim(token))) {
    return(base::t(token))
  }

  tlumaczenia <- getOption("tlumaczenia")
  if (is.null(tlumaczenia)) {
    tlumaczenia <- wczytaj_tlumaczenia()
  }

  wynik <- unname(tlumaczenia[token])
  brak <- is.na(wynik)
  wynik[brak] <- token[brak]
  wynik
}

#' @title Tlumaczenie nazw kolumn ramki danych
#'
#' @description Zamienia nazwy kolumn na tlumaczenia tokenow
#' `col.<nazwa_kolumny>`. Kolumny bez tokena zachowuja oryginalna nazwe.
#' Funkcja sluzy wylacznie do wyswietlania tabel - w kodzie pakietu nazwy
#' kolumn pozostaja niezmienione.
#'
#' @param df Ramka danych.
#'
#' @return Ramka danych z przetlumaczonymi nazwami kolumn.
#'
#' @examples
#' t_kolumny(data.frame(Item = "i1", Trudnosc_p = 0.5))
#'
#' @export
t_kolumny <- function(df) {
  if (length(names(df)) == 0) {
    return(df)
  }

  tokeny <- paste0("col.", names(df))
  nazwy <- t(tokeny)
  brak <- nazwy == tokeny
  nazwy[brak] <- names(df)[brak]
  names(df) <- nazwy
  df
}
