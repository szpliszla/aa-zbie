#' @title Generowanie raportu psychometrycznego
#'
#' @description
#' Renderuje raport psychometryczny na podstawie wskazanego pliku danych.
#' Funkcja przekazuje parametry do szablonu R Markdown raportu, który następnie
#' wczytuje dane, identyfikuje itemy, wykonuje walidację oraz uruchamia analizy
#' psychometryczne.
#'
#' Dane wejściowe powinny być zapisane w formacie obsługiwanym przez funkcje
#' wczytujące dane w pakiecie, np. CSV, RDS, XLS, XLSX albo DTA. Zbiór danych
#' powinien mieć układ prostokątny: jeden wiersz odpowiada jednej obserwacji
#' badanej osoby, ucznia lub respondenta, a kolumny odpowiadają zmiennym.
#' Kolumny z itemami testowymi powinny mieć wspólny prefiks przekazany przez
#' argument `item_prefix`, np. `mat_` dla itemów `mat_1`, `mat_2`, `mat_3`.
#' Itemy powinny być zakodowane binarnie, gdzie `1` oznacza odpowiedź poprawną,
#' a `0` odpowiedź niepoprawną. Braki danych powinny być zapisane jako `NA`
#' albo w sposób możliwy do poprawnego odczytania jako braki danych przez R.
#'
#' Zbiór danych może dodatkowo zawierać kolumnę identyfikatora osoby, kolumnę
#' grupującą obserwacje oraz kolumnę używaną do analizy DIF. Nazwy tych kolumn
#' należy przekazać odpowiednio przez argumenty `id_var`, `group_var` oraz
#' `dif_group_var`.
#'
#' @param output_path Jednoelementowy wektor tekstowy ze ścieżką do pliku,
#'   pod którą ma zostać zapisany wygenerowany raport. Ścieżka może być względna
#'   względem katalogu roboczego projektu albo absolutna.
#' @param data_path Jednoelementowy wektor tekstowy ze ścieżką do pliku danych
#'   wejściowych. Plik powinien zawierać dane w układzie: wiersze jako
#'   obserwacje, kolumny jako zmienne. Kolumny itemów powinny mieć wspólny
#'   prefiks wskazany w `item_prefix`.
#' @param item_prefix Jednoelementowy wektor tekstowy określający prefiks nazw
#'   kolumn z itemami testowymi, np. `"mat_"`. Do analizy zostaną wybrane
#'   kolumny, których nazwy zaczynają się od tego prefiksu.
#' @param group_var Jednoelementowy wektor tekstowy z nazwą zmiennej dzielącej
#'   obserwacje na grupy, np. grupę eksperymentalną, płeć, szkołę albo inną
#'   kategorię używaną w analizach grupowych.
#' @param id_var Jednoelementowy wektor tekstowy z nazwą zmiennej jednoznacznie
#'   identyfikującej obserwację w zbiorze danych, np. identyfikator ucznia lub
#'   respondenta.
#' @param dif_group_var Jednoelementowy wektor tekstowy z nazwą zmiennej
#'   grupującej używanej w analizie DIF. Zmienna powinna mieć co najmniej dwie
#'   niepuste wartości/grupy, aby analiza DIF mogła zostać wykonana.
#'
#' @return
#' Funkcja jest wywoływana głównie dla efektu ubocznego, czyli zapisania raportu
#' pod ścieżką wskazaną w `output_path`. Zwraca wynik działania
#' `rmarkdown::render()`, czyli ścieżkę do wygenerowanego pliku raportu.
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "math_data.csv", package = "aazbie")
#' render_report("raport.html", data_path, "mat_", "grupa", "id_ucznia", "grupa")
#' }
#'
#' @export
render_report <- function(output_path, data_path, item_prefix, group_var, id_var, dif_group_var) {
  if (!fs::is_absolute_path(output_path)) {
    output_path = fs::path_join(c(getwd(), output_path))
  }
  if (!fs::is_absolute_path(data_path)) {
    data_path = fs::path_join(c(getwd(), data_path))
  }
  rmd_path <- system.file("reports", "psychometria_raport.Rmd", package = "aazbie")
  if (rmd_path == "") {
    stop("Nie znaleziono szablonu raportu w zainstalowanym pakiecie.", call. = FALSE)
  }
  rmarkdown::render(
    rmd_path,
    output_file = output_path,
    params = list(
      data_path = data_path,
      item_prefix = item_prefix,
      group_var = group_var,
      id_var = id_var,
      dif_group_var = dif_group_var
    )
  )
}
