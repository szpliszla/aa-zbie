#' @title Formatowanie tabeli do raportu
#'
#' @description Tlumaczy nazwy kolumn (tokeny `col.*`, zob. [t_kolumny()])
#' i wartosci logiczne (zob. [t_logiczne()]) oraz formatuje ramke danych
#' jako tabele kableExtra w stylu striped/hover/condensed. Do wyswietlenia
#' tabeli w raporcie sluzy [show_table()].
#'
#' @param df Ramka danych.
#' @param ... Dodatkowe argumenty przekazywane do [kableExtra::kbl()].
#'
#' @return Obiekt klasy `kableExtra`.
#'
#' @examples
#' fmt_table(data.frame(Item = "i1", Trudnosc_p = 0.5))
#'
#' @export
fmt_table <- function(df, ...) {
  df <- t_kolumny(t_logiczne(df))
  tabela <- kableExtra::kbl(df, row.names = FALSE, ...)
  kableExtra::kable_styling(
    tabela,
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13
  )
}

#' @title Wyswietlenie tabeli w raporcie
#'
#' @description Formatuje ramke danych przez [fmt_table()] i wypisuje kod
#' HTML tabeli (do chunkow z `results = "asis"`). Nie korzysta z metody
#' `print()` kableExtra, ktora w sesji interaktywnej (np. render z konsoli
#' RStudio) wysyla tabele do panelu Viewer zamiast do raportu.
#'
#' @param df Ramka danych.
#' @param ... Dodatkowe argumenty przekazywane do [fmt_table()].
#'
#' @return Niewidocznie `df`. Funkcja jest wywolywana dla efektu ubocznego.
#'
#' @examples
#' show_table(data.frame(Item = "i1", Trudnosc_p = 0.5))
#'
#' @export
show_table <- function(df, ...) {
  cat(as.character(fmt_table(df, ...)), "\n\n", sep = "")
  invisible(df)
}
