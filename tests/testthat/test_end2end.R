test_that('raport na przykładowych danych działa', {
  outpath = fs::path_join(c(tempdir(), "raport.html"))
  if (file.exists(outpath)) {
    unlink(outpath)
  }
  data_path = test_path("../../inst/extdata/math_data.csv")
  if (!file.exists(data_path)) {
    # (R CMD check variant)
    data_path = test_path("../../00_pkg_src/aazbie/inst/extdata/math_data.csv")
  }
  render_report(
      outpath,
      data_path,
      "mat_",
      "grupa",
      "id_ucznia",
      "grupa"
  )
  expect_true(file.exists(outpath))
  unlink(outpath)
})
