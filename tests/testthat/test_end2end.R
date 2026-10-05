test_that('raport na przykładowych danych binarnych działa', {
  outpath <- fs::path_join(c(tempdir(), "raport_bin.html"))
  if (file.exists(outpath)) unlink(outpath)

  data_path <- test_path("../../inst/extdata/math_data.csv")
  if (!file.exists(data_path)) {
    data_path <- test_path("../../00_pkg_src/aazbie/inst/extdata/math_data.csv")
  }

  render_report(
    outpath, data_path, "mat_",
    group_var = "grupa",
    id_var = "id_ucznia",
    dif_group_var = "grupa"
  )

  expect_true(file.exists(outpath))
  # render_report() defaults to English
  expect_identical(polskie_frazy(outpath), character(0))
  # Item tables are written as HTML (not only the static markdown tables)
  expect_gt(length(gregexpr("<table", paste(readLines(outpath, warn = FALSE), collapse = "\n"))[[1]]), 15)
  # Excel file next to the report, with a timestamp in its name
  xlsx <- list.files(dirname(outpath), "^math_data_results_.*\\.xlsx$", full.names = TRUE)
  expect_length(xlsx, 1)
  unlink(c(outpath, xlsx))
})

test_that('raport na danych politomicznych działa', {
  outpath <- fs::path_join(c(tempdir(), "raport_poly.html"))
  if (file.exists(outpath)) unlink(outpath)

  data_path <- test_path("../../inst/extdata/mixed_data.csv")
  if (!file.exists(data_path)) {
    data_path <- test_path("../../00_pkg_src/aazbie/inst/extdata/mixed_data.csv")
  }

  render_report(
    outpath, data_path, "zad_",
    group_var = "grupa",
    id_var = "id_ucznia",
    dif_group_var = "plec"
  )

  expect_true(file.exists(outpath))
  # render_report() defaults to English
  expect_identical(polskie_frazy(outpath), character(0))
  xlsx <- list.files(dirname(outpath), "^mixed_data_results_.*\\.xlsx$", full.names = TRUE)
  expect_length(xlsx, 1)
  unlink(c(outpath, xlsx))
})
