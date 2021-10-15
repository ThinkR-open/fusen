
tmpdir <- tempdir()
# Start from a random working directory
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

raw_rmd_template <- readLines(system.file("dev-template-full.Rmd", package = "fusen"))
expected_rmd_template_with_fusen_name <- gsub("<my_package_name>", "foosen", raw_rmd_template)

withr::with_dir(dummypackage, {
  # browser()
  test_that("Create fusen", {
    path_foosen <- file.path(tmpdir, "foosen")
    path_dev_history <- file.path(path_foosen, "dev", "dev_history.Rmd")
    create_fusen(path_foosen, name = "full", open = FALSE)

    expect_true(dir.exists(path_foosen))
    expect_true(file.exists(path_dev_history))

    actual_dev_history <- readLines(path_dev_history)
    expect_identical(
      actual_dev_history,
      expected_rmd_template_with_fusen_name
    )
  })
})

unlink(tmpdir, recursive = TRUE)
unlink(dummypackage, recursive = TRUE)
