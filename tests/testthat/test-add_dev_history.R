# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# Add
dev_path <- add_dev_history(pkg = dummypackage)

test_that("add_dev_history adds dev_history.Rmd and co.", {
  expect_true(file.exists(dev_path))
  expect_true(file.exists(file.path(dummypackage, ".here")))
  
  rbuildignore_file <- file.path(dummypackage, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))

  dev_lines <- readLines(dev_path)
  expect_true(length(grep("dummypackage", dev_lines)) == 1)
  
  # Second time error
  expect_message(add_dev_history(pkg = dummypackage))
  expect_true(file.exists(file.path(dummypackage, "dev", "dev_history_2.Rmd")))
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
