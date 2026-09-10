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
#'
#' Itemy mogą być zakodowane binarnie (0/1) lub politomicznie (0/1/2/...).
#' Braki danych powinny być zapisane jako `NA` albo w sposób możliwy do
#' poprawnego odczytania jako braki danych przez R.
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
#' @param group_var Opcjonalny wektor tekstowy z nazwą zmiennej dzielącej
#'   obserwacje na grupy, np. grupę eksperymentalną, płeć, szkołę albo inną
#'   kategorię. Domyślnie `NULL` (brak zmiennej grupującej).
#' @param id_var Opcjonalny wektor tekstowy z nazwą zmiennej jednoznacznie
#'   identyfikującej obserwację w zbiorze danych, np. identyfikator ucznia lub
#'   respondenta. Domyślnie `NULL`.
#' @param dif_group_var Opcjonalny wektor tekstowy z nazwą zmiennej
#'   grupującej używanej w analizie DIF. Zmienna powinna mieć co najmniej dwie
#'   niepuste wartości/grupy. Domyślnie `NULL` (DIF nie jest wykonywany).
#' @param exclude_items Opcjonalny wektor tekstowy z nazwami itemów do
#'   wykluczenia z analizy. Domyślnie `NULL`.
#' @param version_var Opcjonalny wektor tekstowy z nazwą zmiennej w zbiorze
#'   danych, która wskazuje wersję testu (np. numer zeszytu). Gdy podana,
#'   raport rozdziela analizy per wersja. Gdy `NULL` (domyślnie), wersje
#'   są wykrywane heurystycznie na podstawie wzorców braków danych.
#' @param min_pattern_prop Minimalna proporcja obserwacji o danym wzorcu braków,
#'   aby wzorzec został uznany za odrębną wersję testu. Domyślnie `0.05`.
#' @param item_missing_max_prop Maksymalna dopuszczalna proporcja braków w
#'   itemie wewnątrz wersji testu. Domyślnie `0.90`.
#' @param alpha_threshold Próg alfa Cronbacha do oceny rzetelności.
#'   Domyślnie `0.70`.
#' @param discrimination_min Minimalny próg mocy różnicującej itemu.
#'   Domyślnie `0.30`.
#' @param dif_method Metoda analizy DIF. Domyślnie `"logistic"`.
#'
#' @return
#' Funkcja jest wywoływana głównie dla efektu ubocznego, czyli zapisania raportu
#' pod ścieżką wskazaną w `output_path`. Zwraca (niewidocznie) wynik działania
#' `rmarkdown::render()`, czyli ścieżkę do wygenerowanego pliku raportu.
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "math_data.csv", package = "aazbie")
#'
#' # Minimalny zestaw argumentow
#' render_report("raport.html", data_path, "mat_")
#'
#' # Z grupami i DIF
#' render_report("raport.html", data_path, "mat_",
#'               group_var = "grupa", id_var = "id_ucznia",
#'               dif_group_var = "grupa")
#'
#' # Z jawna zmienna wersji testu
#' render_report("raport.html", data_path, "mat_",
#'               version_var = "nr_zeszytu")
#' }
#'
#' @export
render_report <- function(
    output_path,
    data_path,
    item_prefix,
    group_var = NULL,
    id_var = NULL,
    dif_group_var = NULL,
    exclude_items = NULL,
    version_var = NULL,
    min_pattern_prop = 0.05,
    item_missing_max_prop = 0.90,
    alpha_threshold = 0.70,
    discrimination_min = 0.30,
    dif_method = "logistic"
) {
  if (!fs::is_absolute_path(output_path)) {
    output_path <- fs::path_join(c(getwd(), output_path))
  }
  if (!fs::is_absolute_path(data_path)) {
    data_path <- fs::path_join(c(getwd(), data_path))
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
      dif_group_var = dif_group_var,
      exclude_items = exclude_items,
      version_var = version_var,
      min_pattern_prop = min_pattern_prop,
      item_missing_max_prop = item_missing_max_prop,
      alpha_threshold = alpha_threshold,
      discrimination_min = discrimination_min,
      dif_method = dif_method
    )
  )
}
