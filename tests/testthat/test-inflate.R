# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE)
# More complicated example for tests
file.copy(
  system.file("tests-templates/dev-template-tests.Rmd", package = "fusen"),
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
    file.path(dummypackage, "tests", "testthat", "test-my_median.R")
  ))
  expect_true(file.exists(
    file.path(dummypackage, "tests", "testthat", "test-my_other_median.R")
  ))

  # Namespace
  expect_true(file.exists(file.path(dummypackage, "NAMESPACE")))
})

# Test package no check errors ----
check_out <- rcmdcheck::rcmdcheck(dummypackage, quiet = TRUE,
                                  args = c("--no-manual"))
# stop(paste(check_out[["errors"]], check_out[["warnings"]], check_out[["notes"]], collapse = "\n---\n"))
# stop(paste(check_out[["notes"]], collapse = "\n---\n"))
test_that("inflate() output error", {
  # No errors
  expect_true(length(check_out[["errors"]]) == 0)
  # 1 warning = License
  # expect_true(length(check_out[["warnings"]]) == 1)
  expect_true(grepl("license", check_out[["warnings"]][1]))
  # No Notes
  expect_true(length(check_out[["notes"]]) ==  0)
})
# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)


# Tests no errors - no example, no tests ----
file.copy(
  system.file("tests-templates/dev-template-no-example-no-tests.Rmd", package = "fusen"),
  dev_file,
  overwrite = TRUE
)
inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE)
test_that("inflate() output error", {
  expect_true(file.exists(file.path(dummypackage, "vignettes", "exploration.Rmd")))
  expect_true(file.exists(file.path(dummypackage, "R", "my_median.R")))
  expect_true(!file.exists(file.path(dummypackage, "tests", "testthat", "test-my_median.R")))
})
# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)


# Tests no errors - empty ----
file.copy(
  system.file("tests-templates/dev-template-test-parse-nothing.Rmd", package = "fusen"),
  dev_file,
  overwrite = TRUE
)
test_that("inflate() output error", {
  expect_message(inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE))
})
# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)


# Tests errors - duplicate functions ----
file.copy(
  system.file("tests-templates/dev-template-stop-duplicate-fun.Rmd", package = "fusen"),
  dev_file,
  overwrite = TRUE
)
test_that("inflate() output error duplicate functions", {
  expect_error(inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE))
})
# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)

# Tests errors - duplicate chunk names
file.copy(
  system.file("tests-templates/dev-template-stop-duplicate-label.Rmd", package = "fusen"),
  dev_file,
  overwrite = TRUE
)
test_that("inflate() output error duplicate label names for vignette", {
  expect_error(inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE))
})
# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)


# Test no errors - inflate with .Rproj and no .here ----
file.remove(file.path(dummypackage, ".here"))
file.remove(file.path(dummypackage, ".Rbuildignore"))
cat("", file = file.path(dummypackage, 'dummy.Rproj'))

# Add
# {fusen} steps
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE)
inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE)

test_that("add_dev_history inflates with .Rproj and no .here", {
  expect_true(file.exists(dev_file))
  expect_false(file.exists(file.path(dummypackage, ".here")))
  
  rbuildignore_file <- file.path(dummypackage, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))
  expect_false(any(grepl("[.]here", rbuildignore_lines)))
  
  # R files
  my_median_file <- file.path(dummypackage, "R", "my_median.R")
  expect_true(file.exists(my_median_file))
  # vignette
  expect_true(file.exists(file.path(dummypackage, "vignettes", "exploration.Rmd")))
  # tests
  expect_true(file.exists(
    file.path(dummypackage, "tests", "testthat", "test-my_median.R")
  ))
})

# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)

# Test no errors - clean name for vignette ----
# name <- "# y  _ p n@ é ! 1"
# name <- "exploration"
# cleaned_name <- gsub("^-|-$", "",
#                      gsub("-+", "-",
#                           gsub("-_|_-", "-",
#                           gsub("[^([:alnum:]*_*-*)*]", "-", name))))
# grepl("^[[:alpha:]][[:alnum:]_-]*$", cleaned_name)
# # asciify from {usethis} usethis:::asciify()
# cleaned_name <- gsub("[^a-zA-Z0-9_-]+", "-", cleaned_name)
# usethis::use_vignette(name = cleaned_name, title = name)

inflate(pkg = dummypackage, rmd = dev_file, name = "# y  _ p n@ é ! 1", check = FALSE)
# Vignette name is also cleaned by {usethis} for special characters
vignette_path <- file.path(dummypackage, "vignettes", "y-p-n---1.Rmd")

test_that("vignette is created with clean name", {
  expect_true(file.exists(vignette_path))
  vig_lines <- readLines(vignette_path)
  expect_true(sum(grepl("# y  _ p n@ é ! 1", vig_lines, fixed = TRUE)) == 2)
  expect_true(sum(grepl("y-p-n---1", vig_lines, fixed = TRUE)) == 0)
})

# Clean R, tests and vignettes
unlink(file.path(dummypackage, "R"), recursive = TRUE)
unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
unlink(file.path(dummypackage, "tests"), recursive = TRUE)


# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Do not create a second package with {fusen} in the same session, as it will mess up with `setwd()` and {usethis} needs these `setwd()`...
