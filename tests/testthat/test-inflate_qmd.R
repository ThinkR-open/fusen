# Test full ----
dummypackage <- tempfile("inflate.qmd")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

# For comments with #|
skip_if_not(utils::packageVersion("knitr") >= 1.35)

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests.qmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("inflate() worked correctly with quarto qmd file", {

    # Number of files
    expect_equal(length(list.files(file.path(dummypackage, "R"))), 11)
    expect_equal(length(list.files(file.path(dummypackage, "vignettes"))), 1)
    expect_equal(length(list.files(file.path(dummypackage, "tests", "testthat"))), 4)
  })
})

# Clean
unlink(dummypackage, recursive = TRUE)
