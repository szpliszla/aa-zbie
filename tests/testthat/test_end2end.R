test_that('raport na przykładowych danych działa', {
  outpath = fs::path_join(c(tempdir(), "raport.html"))
  if (file.exists(outpath)) {
    unlink(outpath)
  }
  render_report(
      outpath,
      system.file("extdata", "math_data.csv", package = "aazbie"),
      "mat_",
      "grupa",
      "id_ucznia",
      "grupa"
  )
  expect_true(file.exists(outpath))
})
