test_that('raport na przykładowych danych działa', {
  # the path depends on how the tests are run
  rmd_path = test_path("../../reports/psychometria_raport.Rmd")
  if (!file.exists(rmd_path)) {
    # (R CMD check variant)
    rmd_path = test_path("../../00_pkg_src/aazbie/reports/psychometria_raport.Rmd")
  }
  rmarkdown::render(
    rmd_path,
    params = list(
      data_path = "../data/math_data.csv",
      item_prefix = "mat_",
      group_var = "grupa",
      id_var = "id_ucznia",
      dif_group_var = "grupa"
    )
  )
})
