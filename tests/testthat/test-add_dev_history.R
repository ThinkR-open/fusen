# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
# unlink(dummypackage, recursive = TRUE)
dir.create(dummypackage)

# Add
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
# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Test with .Rproj and no .here, it works
# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
# unlink(dummypackage, recursive = TRUE)
dir.create(dummypackage)
cat("", file = file.path(dummypackage, 'dummy.Rproj'))

# Add
dev_path <- add_dev_history(pkg = dummypackage, open = FALSE)

test_that("add_dev_history works with .Rproj and no .here", {
  expect_true(file.exists(dev_path))
  expect_false(file.exists(file.path(dummypackage, ".here")))
  
  rbuildignore_file <- file.path(dummypackage, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))
  expect_false(any(grepl("[.]here", rbuildignore_lines)))
})
# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# More complicated example for tests
# This will render the Rmd template that is supposed to build a package
# But we need to be inside a project, 
# in the correct working directory, 
# with the correct here()

# Create a new project
# tmpdir_r <- tempfile()
# dir.create(tmpdir_r)
# tmpdir_r
# library(fusen)

tmpdir <- tempfile()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage, recursive = TRUE)
# Add
dev_path <- add_dev_history(pkg = dummypackage, open = FALSE)
# Change lines asking for pkg name
lines_template <- readLines(system.file("tests-templates/dev-template-tests.Rmd", package = "fusen"))
lines_template[grepl("<my_package_name>", lines_template)] <-
  gsub("<my_package_name>", basename(dummypackage), 
       lines_template[grepl("<my_package_name>", lines_template)])
cat(enc2utf8(lines_template), file = dev_path, sep = "\n")

old_here <- here::here(getwd())
usethis::proj_set(old_here, force = TRUE)
# old <- setwd(dummypackage)
withr::with_dir(dummypackage, {
  # browser()
  # withr::temp
  # old_proj <- usethis::proj_get()
  # if (normalizePath(old_proj) != normalizePath(dummypackage)) {
  usethis::proj_set(dummypackage)
  # }
  here:::do_refresh_here(dummypackage)
  
  rmarkdown::render(
    input = file.path(dummypackage, "dev/dev_history.Rmd"), 
    output_file = file.path(dummypackage, 'dev/dev_history.html'), 
    envir = new.env(), quiet = TRUE)

  # 
  # if (normalizePath(old_proj) != normalizePath(dummypackage)) {
  #   usethis::proj_set(old_proj)
  # }
})
# setwd(old)
here:::do_refresh_here(old_here)
usethis::proj_set(old_here, force = TRUE)

test_that("dev-template-tests run as markdown", {

  expect_true(file.exists(file.path(dummypackage, "DESCRIPTION")))
  expect_true(file.exists(file.path(dummypackage, "LICENSE")))
  expect_true(file.exists(file.path(dummypackage, "inst/nyc_squirrels_sample.csv")))
  expect_true(file.exists(file.path(dummypackage, "dev/dev_history.html")))

})

# For debug in check()
# file.copy(dummypackage, to = "~/Bureau/", recursive = TRUE)
  
# Delete dummy package
unlink(dummypackage, recursive = TRUE)
