# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# Add ----
dev_path <- add_dev_history(pkg = dummypackage, open = FALSE)

test_that("add_dev_history adds dev_history.Rmd and co.", {
  expect_true(file.exists(dev_path))
  expect_true(file.exists(file.path(dummypackage, ".here")))

  rbuildignore_file <- file.path(dummypackage, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))
  expect_true(any(grepl("[.]here", rbuildignore_lines)))

  dev_lines <- readLines(dev_path)
  expect_true(length(grep("dummypackage", dev_lines)) == 1)

  # Second time error
  expect_message(add_dev_history(pkg = dummypackage))
  expect_true(file.exists(file.path(dummypackage, "dev", "dev_history_2.Rmd")))
})


# Test with .Rproj and no .here, it works ----
# Create a new project
dummypackage2 <- file.path(tmpdir, "dummypackage2")
dir.create(dummypackage2)
cat("", file = file.path(dummypackage2, 'dummy.Rproj'))

# Add
dev_path <- add_dev_history(pkg = dummypackage2, open = FALSE)

test_that("add_dev_history works with .Rproj and no .here", {
  expect_true(file.exists(dev_path))
  expect_false(file.exists(file.path(dummypackage2, ".here")))

  rbuildignore_file <- file.path(dummypackage2, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))
  expect_false(any(grepl("[.]here", rbuildignore_lines)))
})

# Add failed with malformed package name ----
# Create a new project
dummypackage3 <- file.path(tmpdir, "dummy_package3")
dir.create(dummypackage3)
cat("", file = file.path(dummypackage3, 'dummy.Rproj'))

# Add
test_that("add_dev_history fails", {
  expect_error(add_dev_history(pkg = dummypackage3, open = FALSE))
})

# More complicated example for tests
# This will render the Rmd template that is supposed to build a package
# But we need to be inside a project,
# in the correct working directory,
# with the correct here()

# Create a new project
dummypackage4 <- file.path(tmpdir, "dummypackage4")
dir.create(dummypackage4, recursive = TRUE)
# Add
dev_path <- add_dev_history(pkg = dummypackage4, open = FALSE)
# Change lines asking for pkg name
lines_template <- readLines(system.file("tests-templates/dev-template-tests.Rmd", package = "fusen"))
lines_template[grepl("<my_package_name>", lines_template)] <-
  gsub("<my_package_name>", basename(dummypackage4),
       lines_template[grepl("<my_package_name>", lines_template)])
cat(enc2utf8(lines_template), file = dev_path, sep = "\n")

withr::with_dir(dummypackage4, {
  usethis::proj_set(dummypackage4)
  here:::do_refresh_here(dummypackage4)

  if (rmarkdown::pandoc_available("1.12.3")) {
    rmarkdown::render(
      input = file.path(dummypackage4, "dev/dev_history.Rmd"),
      output_file = file.path(dummypackage4, 'dev/dev_history.html'),
      envir = new.env(), quiet = TRUE)
  }
})

test_that("dev-template-tests run as markdown", {
  if (rmarkdown::pandoc_available("1.12.3")) {
    expect_true(file.exists(file.path(dummypackage4, "DESCRIPTION")))
    expect_true(file.exists(file.path(dummypackage4, "LICENSE")))
    expect_true(file.exists(file.path(dummypackage4, "inst/nyc_squirrels_sample.csv")))
    expect_true(file.exists(file.path(dummypackage4, "dev/dev_history.html")))
  }
})

# Test asciify_name ----
test_that("asciify_name works correctly", {
  expect_equal(asciify_name("123AZ"), "123az")
  expect_equal(asciify_name("123AZ", to_pkg = TRUE), "AZ")
  expect_equal(asciify_name("12A.AZ"), "12a-az")
  expect_equal(asciify_name("12A.AZ", to_pkg = TRUE), "A.AZ")
  expect_equal(asciify_name("12A_A-Za"), "12a_a-za")
  expect_equal(asciify_name("12A_A-Za", to_pkg = TRUE), "A.A.Za")
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
unlink(dummypackage2, recursive = TRUE)
unlink(dummypackage3, recursive = TRUE)
unlink(dummypackage4, recursive = TRUE)

