# Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
pkg_name <- basename(dummypackage)

# Deprecated ----
test_that("add_dev_history is deprecated but works correctly", {
  dev_file_path <- expect_warning(
    suppressMessages(
      add_dev_history(pkg = dummypackage, open = FALSE)
    ),
    regexp = "Deprecated"
  )

  flat_file <- dev_file_path[grepl("flat", dev_file_path)]

  expect_true(all(file.exists(dev_file_path)))
  expect_true(file.exists(file.path(dummypackage, "dev", "0-dev_history.Rmd")))
  expect_true(file.exists(file.path(dummypackage, ".here")))

})

