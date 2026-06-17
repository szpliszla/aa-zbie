test_that('raport na przykładowych danych działa', {
  rmarkdown::render(
    test_path("../../reports/psychometria_raport.Rmd"),
    params = list(
      data_path = "../data/math_data.csv",
      item_prefix = "mat_",
      group_var = "grupa",
      id_var = "id_ucznia",
      dif_group_var = "grupa"
    )
  )
})
