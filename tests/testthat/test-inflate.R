# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE, open = FALSE)

usethis::with_project(dummypackage, {

  # More complicated example for tests
    testfile <- "tests-templates/dev-template-tests.Rmd"
    file.copy(
      system.file(testfile, package = "fusen"),
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
    my_sixth_median_file <- file.path(dummypackage, "R", "my.sixth.median_function.R")
    expect_true(file.exists(my_sixth_median_file))

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
    # dot in name
    my_sixth_median_lines <- readLines(my_sixth_median_file)
    expect_true(all(my_sixth_median_lines[9:11] == c(
      "#' @examples", "#' my.sixth.median_function(1:12)", "#' my.sixth.median_function(8:20)"
    )))

    # vignette
    expect_true(file.exists(file.path(dummypackage, "vignettes", "exploration.Rmd")))

    # tests
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_median.R")
    ))
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_other_median.R")
    ))
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my.sixth.median_function.R")
    ))

    # Namespace
    expect_true(file.exists(file.path(dummypackage, "NAMESPACE")))
  })
})


# Test package no check errors ----
usethis::with_project(dummypackage, {

  test_that("inflate() output error", {
    # Do not check inside check if on CRAN
    skip_on_os(os = c("windows", "solaris"))

    # If this check is run inside a not "--as-cran" check, then it wont work as expected
    check_out <- rcmdcheck::rcmdcheck(dummypackage, quiet = TRUE,
                                      args = c("--no-manual"))

    # No errors
    expect_true(length(check_out[["errors"]]) == 0)
    # 1 warning = License
    # expect_true(length(check_out[["warnings"]]) == 1)
    expect_true(grepl("license", check_out[["warnings"]][1]))
    # No Notes or only one if CRAN
    expect_true(length(check_out[["notes"]]) <= 1)
    if (length(check_out[["notes"]]) == 1) {
      # if tested as cran
      # 1 note on CRAN for new submission
      print(check_out[["notes"]])
      expect_true(grepl("New submission", check_out[["notes"]][1]))
    } else {
      expect_true(length(check_out[["notes"]]) == 0)
    }
  })
  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)
})

# Test no problem with special character in YAML ----
if (packageVersion("parsermd") > "0.1.2") {
  dummypackage.special <- file.path(tmpdir, "dummypackage.special")
  dir.create(dummypackage.special)

  # {fusen} steps
  fill_description(pkg = dummypackage.special, fields = list(Title = "Dummy Package"))
  dev_file <- add_dev_history(pkg = dummypackage.special, overwrite = TRUE, open = FALSE)

  usethis::with_project(dummypackage.special, {

    testfile <- "tests-templates/dev-template-tests-special-char.Rmd"
    file.copy(
      system.file(testfile, package = "fusen"),
      dev_file,
      overwrite = TRUE
    )

    inflate(pkg = dummypackage.special, rmd = dev_file, name = "exploration", check = FALSE)

    test_that("inflate with special yaml worked correctly", {
      # R files
      my_median_file <- file.path(dummypackage.special, "R", "my_median.R")
      expect_true(file.exists(my_median_file))
    })
  })
}

# Test no attachment and no check when asked ----
unlink(file.path(dummypackage, "DESCRIPTION"), recursive = TRUE)
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE, open = FALSE)

usethis::with_project(dummypackage, {
  inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE, document = FALSE)
  desc_lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  expect_false("Imports:" %in% desc_lines)

  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)
})


# Tests no errors - no example, no tests ----
usethis::with_project(dummypackage, {
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
})

# Tests no errors - empty ----
usethis::with_project(dummypackage, {
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
})

# Tests errors - duplicate functions ----
usethis::with_project(dummypackage, {
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
})

# Test no errors - inflate with .Rproj and no .here ----
usethis::with_project(dummypackage, {
  file.remove(file.path(dummypackage, ".here"))
  file.remove(file.path(dummypackage, ".Rbuildignore"))
  cat("", file = file.path(dummypackage, 'dummy.Rproj'))

  # Add
  # {fusen} steps
  dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE, open = FALSE)
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
})

# Test no errors - clean name for vignette ----
usethis::with_project(dummypackage, {
  inflate(pkg = dummypackage, rmd = dev_file, name = "# y  _ p n@ \u00E9 ! 1", check = FALSE)
  # Vignette name is also cleaned by {usethis} for special characters
  vignette_path <- file.path(dummypackage, "vignettes", "y-p-n---1.Rmd")

  test_that("vignette is created with clean name", {
    expect_true(file.exists(vignette_path))
    # usethis::use_vignette writes in UTF-8
    vig_lines <- readLines(vignette_path, encoding = "UTF-8")
    expect_true(sum(grepl("# y  _ p n@ \u00E9 ! 1", vig_lines, fixed = TRUE)) == 2)
    expect_true(sum(grepl("y-p-n---1", vig_lines, fixed = TRUE)) == 0)
  })

  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)


# Test stop when no DESCRIPTION file ----
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "descpackage")
dir.create(dummypackage)
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE, open = FALSE)

usethis::with_project(dummypackage, {
  test_that("stop when no DESCRIPTION file", {
    expect_error(inflate(pkg = dummypackage, rmd = dev_file, check = FALSE), "DESCRIPTION file")
  })
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
if (exists("dummypackage.special")) {
  unlink(dummypackage.special, recursive = TRUE)
}

# Do not create a second package with {fusen} in the same session, as it will mess up with `setwd()` and {usethis} needs these `setwd()`...
