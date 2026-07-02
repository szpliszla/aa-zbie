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
#' render_report("raport.html", "data/math_data.csv", "mat_", "grupa", "id_ucznia", "grupa")
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
  codedir = utils::getSrcDirectory(function(){})
  if (length(codedir) == 0) {
    # R CMD check
    codedir = "../../00_pkg_src/aazbie/R"
  }
  rmarkdown::render(
    fs::path_join(c(codedir, '../reports/psychometria_raport.Rmd')),
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
