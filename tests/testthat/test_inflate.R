# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE)
# More complicated example for tests
dev_file <- file.path(dummypackage, "dev_history.Rmd")
file.copy(
  system.file("dev-template-tests.Rmd", package = "fusen"),
  dev_file,
  overwrite = TRUE
)
inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE)

test_that("inflate() worked correctly", {
  # R files
  my_median_file <- file.path(dummypackage, "R", "my_median.R")
  expect_true(file.exists(my_median_file))
  my_other_median_file <- file.path(dummypackage, "R", "my_other_median.R")
  expect_true(file.exists(my_other_median_file))
  my_third_median_file <- file.path(dummypackage, "R", "my_third_median.R")
  expect_true(file.exists(my_third_median_file))
  # examples in R files
  my_median_lines <- readLines(my_median_file)
  expect_true(all(my_median_lines[10:12] == c(
    "#' @examples", "#' my_median(2:20)", "#' my_median(1:12)"
  )))
  my_other_median_lines <- readLines(my_other_median_file)
  expect_true(all(my_other_median_lines[10:13] == c(
    "#' @examples", "#' my_other_median(1:12)",
    "#' my_other_median(8:20)", "#' my_other_median(20:50)"
  )))
  my_third_median_lines <- readLines(my_third_median_file)
  # _no example
  expect_true(all(!grepl("#' @examples", my_third_median_lines)))

  # vignette
  expect_true(file.exists(file.path(dummypackage, "vignettes", "exploration.Rmd")))

  # tests
  expect_true(file.exists(
    file.path(dummypackage, "tests", "testthat", "test_my_median.R")
  ))
  expect_true(file.exists(
    file.path(dummypackage, "tests", "testthat", "test_my_other_median.R")
  ))

  # Namespace
  expect_true(file.exists(file.path(dummypackage, "NAMESPACE")))
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)

