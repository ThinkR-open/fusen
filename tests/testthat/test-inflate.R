# Create a new project
dummypackage <- tempfile("dummypackage")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  suppressMessages(
    inflate(pkg = dummypackage, rmd = flat_file,
            name = "Get started", check = FALSE)
  )

  test_that("inflate() worked correctly", {
    # R files
    my_median_file <- file.path(dummypackage, "R", "my_median.R")
    expect_true(file.exists(my_median_file))
    my_other_median_file <- file.path(dummypackage, "R", "my_other_median.R")
    expect_true(file.exists(my_other_median_file))
    my_third_median_file <- file.path(dummypackage, "R", "my_third_median.R")
    expect_true(file.exists(my_third_median_file))
    my_sixth_median_file <- file.path(dummypackage, "R", "my-sixth-median_function.R")
    expect_true(file.exists(my_sixth_median_file))
    myuppercasefunctionfile <- file.path(dummypackage, "R", "myuppercasefunction.R")
    expect_true(file.exists(myuppercasefunctionfile))

    # Found with chunk named `fun`
    my_noroxfunctionfile <- file.path(dummypackage, "R", "my_norox.R")
    expect_true(file.exists(my_noroxfunctionfile))
    # Found with chunk named `fun-norox2`
    my_norox2functionfile <- file.path(dummypackage, "R", "my_norox2.R")
    expect_true(file.exists(my_norox2functionfile))
    # Found with chunk named `fun_space`
    my_spacefunctionfile <- file.path(dummypackage, "R", "my_space.R")
    expect_true(file.exists(my_spacefunctionfile))
    my_space2functionfile <- file.path(dummypackage, "R", "my_space2.R")
    expect_true(file.exists(my_space2functionfile))

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
    # _no roxygen at all
    my_norox_lines <- readLines(my_noroxfunctionfile)
    expect_true(all(my_norox_lines == c(
      "#' @noRd", "my_norox <- function(x) {", "  x + 10", "}"
    )))

    # _no roxygen but examples
    my_norox2_lines <- readLines(my_norox2functionfile)
    expect_equal(my_norox2_lines, c(
      "#' @noRd", "#' @examples",
      "#' \\dontrun{", "#' # comment",
      "#' my_norox2(10)", "#' }",
      "#' ",
      "#' \\dontrun{",
      "#' # comment",
      "#' my_norox2(12)", "#' }",
      "my_norox2 <- function(x) {", "  x + 10", "}"
    ))
    # _extra empty line and examples
    my_space_lines <- readLines(my_spacefunctionfile)
    expect_true(all(my_space_lines[6:10] == c(
      "#' @examples", "#' my_space(10)", "#' @export", "",  "my_space <- function(x) {"
    )))
    # _extra empty line and noRd
    my_space2_lines <- readLines(my_space2functionfile)
    expect_true(all(my_space2_lines[6:8] == c(
      "#' @noRd", "",  "my_space2 <- function(x) {"
    )))

    # vignette
    the_vignette <- file.path(dummypackage, "vignettes", "get-started.Rmd")
    expect_true(file.exists(the_vignette))
    vignette_lines <- readLines(the_vignette)
    # No dev chunks in the vignette
    expect_false(any(grepl("```{r dev}", vignette_lines, fixed = TRUE)))
    expect_false(any(grepl("```{r development-1", vignette_lines, fixed = TRUE)))

    # tests
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_median.R")
    ))
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_other_median.R")
    ))
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my-sixth-median_function.R")
    ))
    expect_true(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-myuppercasefunction.R")
    ))
    # no test
    expect_false(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_norox.R")
    ))
    expect_false(file.exists(
      file.path(dummypackage, "tests", "testthat", "test-my_space2.R")
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
  dummypackage.special <- tempfile("dummypackage.special")
  dir.create(dummypackage.special)

  # {fusen} steps
  fill_description(pkg = dummypackage.special, fields = list(Title = "Dummy Package"))
  dev_file <- add_flat_template(pkg = dummypackage.special, overwrite = TRUE, open = FALSE)
  flat_file <- dev_file[grepl("flat_", dev_file)]

  usethis::with_project(dummypackage.special, {

    testfile <- "tests-templates/dev-template-tests-special-char.Rmd"
    file.copy(
      system.file(testfile, package = "fusen"),
      flat_file,
      overwrite = TRUE
    )

    inflate(pkg = dummypackage.special, rmd = flat_file,
            name = "Get started", check = FALSE)

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
dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  inflate(pkg = dummypackage, rmd = flat_file, name = "Get started",
          check = FALSE, document = FALSE)
  desc_lines <- readLines(file.path(dummypackage, "DESCRIPTION"))

  test_that("no attachment run", {
    expect_false("Imports:" %in% desc_lines)
  })

  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)
})


# Tests no errors - no example, no tests ----
usethis::with_project(dummypackage, {
  file.copy(
    system.file("tests-templates/dev-template-no-example-no-tests.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  inflate(pkg = dummypackage, rmd = flat_file,
          name = "Get started", check = FALSE)
  test_that("inflate() output no error", {
    expect_true(file.exists(file.path(dummypackage, "vignettes", "get-started.Rmd")))
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
    flat_file,
    overwrite = TRUE
  )
  test_that("inflate() output message", {
    expect_message(inflate(pkg = dummypackage, rmd = flat_file,
                           name = "Get started", check = FALSE))
  })
  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)
})

# Tests errors - vignette already exists ----
usethis::with_project(dummypackage, {

  inflate(pkg = dummypackage, rmd = flat_file,
          name = "Get started",
          check = FALSE, overwrite = "yes")

  test_that("inflate() output error when second time (not interactive)", {
    expect_error(inflate(pkg = dummypackage, rmd = flat_file,
                         name = "Get started",
                         check = FALSE))
    expect_error(inflate(pkg = dummypackage, rmd = flat_file,
                         name = "Get started",
                         check = FALSE, overwrite = 'no'))
  })

  # No error with overwrite = 'yes'
  inflate(pkg = dummypackage, rmd = flat_file, name = "Get started",
          check = FALSE, overwrite = "yes")

  test_that("inflate() output no error", {
    expect_true(file.exists(file.path(dummypackage, "vignettes", "get-started.Rmd")))
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
    flat_file,
    overwrite = TRUE
  )
  test_that("inflate() output error duplicate functions", {
    expect_error(inflate(pkg = dummypackage, rmd = flat_file,
                         name = "Get started", check = FALSE))
  })
  # Clean R, tests and vignettes
  unlink(file.path(dummypackage, "R"), recursive = TRUE)
  unlink(file.path(dummypackage, "vignettes"), recursive = TRUE)
  unlink(file.path(dummypackage, "tests"), recursive = TRUE)

  # Tests errors - duplicate chunk names
  file.copy(
    system.file("tests-templates/dev-template-stop-duplicate-label.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  test_that("inflate() output error duplicate label names for vignette", {
    expect_error(inflate(pkg = dummypackage, rmd = flat_file, name = "Get started", check = FALSE))
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
  dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
  inflate(pkg = dummypackage, rmd = flat_file,
          name = "Get started", check = FALSE)

  test_that("add_flat_template inflates with .Rproj and no .here", {
    expect_true(file.exists(flat_file))
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
    expect_true(file.exists(file.path(dummypackage, "vignettes", "get-started.Rmd")))
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
  inflate(pkg = dummypackage, rmd = flat_file, name = "# y  _ p n@ \u00E9 ! 1", check = FALSE)
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

usethis::with_project(dummypackage, {
  inflate(pkg = dummypackage, rmd = flat_file, name = "# y  _ p n@ \u00E9 ! 1", check = FALSE)
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
dummypackage <- tempfile("descpackage")
dir.create(dummypackage)
dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  test_that("stop when no DESCRIPTION file", {
    expect_error(inflate(pkg = dummypackage, rmd = flat_file, check = FALSE), "DESCRIPTION file")
  })
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
if (exists("dummypackage.special")) {
  unlink(dummypackage.special, recursive = TRUE)
}

# Deal with noRd, examples and dontrun ----
#' stop() if @noRd but there is an example...
#' Or suggests \dontrun{}, but need to be taken into account in vignette

dummypackage <- tempfile("nordpackage")
dir.create(dummypackage)
dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
flat_file <- dev_file[grepl("flat_", dev_file)]
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))

usethis::with_project(dummypackage, {
  file.copy(
    system.file("tests-templates/dev-template-nord-but-example.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  test_that("Deal with noRd but examples", {
    # No error
    expect_error(inflate(pkg = dummypackage, rmd = flat_file, check = FALSE), regexp = NA)
    # Check error
    skip_on_os(os = c("windows", "solaris"))

    # Could not find function "my_norox2"
    expect_error(rcmdcheck::rcmdcheck(dummypackage, quiet = TRUE,
                                      args = c("--no-manual")))
  })
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Test checks all templates with inflate dots (...) ----
alltemp <- tempfile("all_templates")
dir.create(alltemp)

for (pkgname in c("full", "teaching")) {
  # TODO: "minimal" can inflate ? Empty dev_history does not work.
  # No "additional" with create_fusen
  # {fusen} steps
  path_foosen <- file.path(alltemp, pkgname)
  dev_file <- create_fusen(path_foosen, template = pkgname, open = FALSE)
  flat_file <- dev_file[grepl("flat_", dev_file)]

  usethis::with_project(path_foosen, {

      # Do not check inside check if on CRAN
      skip_on_os(os = c("windows", "solaris"))

      fill_description(pkg = path_foosen, fields = list(Title = "Dummy Package"))
      usethis::use_gpl_license()

      test_that(paste("Check returns OK for template", pkgname), {
        # No redirection of stdout/stderr when non-interactive
        expect_error(
          inflate(pkg = path_foosen, rmd = flat_file, name = "exploration",
                  check = TRUE, quiet = TRUE, args = c("--no-manual"),
                  overwrite = TRUE),
          regexp = NA)

        skip_if_not(interactive())
        # Needs MASS, lattice, Matrix installed
        # quiet and checkdir
        checkdir <- file.path(alltemp, paste0("checkout", pkgname))
        expect_error(
          inflate(pkg = path_foosen, rmd = flat_file, name = "exploration",
                  check = TRUE, check_dir = checkdir, quiet = TRUE,
                  args = c("--no-manual"),
                  overwrite = TRUE),
          regexp = NA)

        # Should not be any errors with templates
        check_lines <- readLines(file.path(checkdir, paste0(basename(path_foosen), ".Rcheck"), "00check.log"))
        expect_equal(check_lines[length(check_lines)], "Status: OK")
        unlink(checkdir, recursive = TRUE)
      })
    # })
  })

} # end of template loop
# Delete dummy package
unlink(alltemp, recursive = TRUE)

# Do not create a second package with {fusen} in the same session, as it will mess up with `setwd()` and {usethis} needs these `setwd()`...
