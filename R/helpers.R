#' @title Formatowanie tabeli do raportu
#'
#' @description Tlumaczy nazwy kolumn (tokeny `col.*`, zob. [t_kolumny()])
#' i formatuje ramke danych jako tabele kableExtra w stylu
#' striped/hover/condensed.
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
  df <- t_kolumny(df)
  tabela <- kableExtra::kbl(df, row.names = FALSE, ...)
  kableExtra::kable_styling(
    tabela,
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13
  )
}
