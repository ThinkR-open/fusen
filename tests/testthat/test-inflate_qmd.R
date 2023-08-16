# Test full ----
dummypackage <- tempfile("inflate.qmd")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

# For comments with #|
skip_if_not(utils::packageVersion("knitr") >= "1.35")

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests.qmd"
  qmd_flat_file <- file.path(dummypackage, "dev", "dev-template-tests.qmd")
  file.copy(
    system.file(testfile, package = "fusen"),
    qmd_flat_file,
    # flat_file,
    overwrite = TRUE
  )
  file.remove(flat_file)

  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = qmd_flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("inflate() worked correctly with quarto qmd file", {
    # Number of files
    expect_equal(length(list.files(file.path(dummypackage, "R"))), 11)
    expect_true(file.exists(file.path(dummypackage, "R", "my-sixth-median_function.R")))
    expect_equal(length(list.files(file.path(dummypackage, "vignettes"))), 1)
    expect_true(file.exists(file.path(dummypackage, "vignettes", "get-started.Rmd")))
    expect_equal(length(list.files(file.path(dummypackage, "tests", "testthat"))), 4)
    expect_true(file.exists(file.path(dummypackage, "tests", "testthat", "test-my-sixth-median_function.R")))
  })

  test_that("config with quarto worked", {
    # config files
    config_file <- file.path(dummypackage, "dev", "config_fusen.yaml")
    config_content <- read_yaml(config_file)
    expect_equal(
      sort(config_content[["dev-template-tests.qmd"]][["path"]]),
      expected = "dev/dev-template-tests.qmd"
    )
    expect_equal(
      sort(config_content[["dev-template-tests.qmd"]][["inflate"]][["flat_file"]]),
      expected = "dev/dev-template-tests.qmd"
    )
    expect_equal(
      length(config_content[["dev-template-tests.qmd"]][["R"]]),
      expected = 11
    )
    expect_equal(
      length(config_content[["dev-template-tests.qmd"]][["tests"]]),
      expected = 4
    )
    expect_equal(
      sort(config_content[["dev-template-tests.qmd"]][["vignettes"]]),
      expected = sort(c("vignettes/get-started.Rmd"))
    )
  })
})

# Clean
unlink(dummypackage, recursive = TRUE)
