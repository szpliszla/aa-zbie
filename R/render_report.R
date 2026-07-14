#' @title Generuje raport psychometryczny
#'
#' @description Generuje raport psychometryczny ze wskazanych danych
#'
#' @param output_path ścieżka pod którą zapisany zostanie wygenerowany raport
#' @param data_path ścieżka do pliku z danymi TODO-opisać format
#' @param item_prefix TODO-opisać
#' @param group_var nazwa zmiennej dzielącej analizowane obserwacje na grupy
#' @param id_var nazwa zmiennej unikalnie identyfikującej obserwację w zbiorze danych
#' @param dif_group_var TODO-opisać
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
