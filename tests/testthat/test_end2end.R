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
  unlink(outpath)
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
  unlink(outpath)
})
