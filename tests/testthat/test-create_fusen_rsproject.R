
tmpdir <- tempdir()

raw_rmd_template <- readLines(system.file("dev-template-full.Rmd", package = "fusen"))
raw_rmd_template_with_fusen_name <- gsub("<my_package_name>", "foosen", raw_rmd_template)
expected_dev_history_full <- parsermd::parse_rmd(raw_rmd_template_with_fusen_name)

test_that("Create fusen", {
  path_foosen <- file.path(tmpdir, "foosen")
  path_dev_history <- file.path(path_foosen, "dev/dev_history.Rmd")
  create_fusen(path_foosen, open = FALSE)

  expect_true(dir.exists(path_foosen))
  expect_true(file.exists(path_dev_history))

  actual_dev_history <- parsermd::parse_rmd(path_dev_history)
  expect_identical(
    expected_dev_history_full,
    actual_dev_history
  )
})

unlink(tmpdir, recursive = TRUE)
