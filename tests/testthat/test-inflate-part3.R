dummypackage <- tempfile(paste0(sample(letters, 10), collapse = ""))
dir.create(dummypackage)
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
console_output_file <- tempfile(fileext = ".txt")
file.create(console_output_file)

usethis::with_project(dummypackage, {
  # Add licence
  usethis::use_mit_license("John Doe")
  dev_file1 <- add_minimal_flat(
    pkg = dummypackage,
    flat_name = "flat1.Rmd",
    open = FALSE,
    overwrite = TRUE
  )
  capture.output(
    inflate(
      pkg = dummypackage,
      flat_file = dev_file1,
      vignette_name = "Get started",
      check = FALSE,
      open_vignette = FALSE,
      document = TRUE,
      overwrite = "yes",
      codecov = FALSE
    ),
    file = console_output_file,
    type = "message"
  )

  test_that("inflate does not compute codecov if not asked", {
    res <- readLines(console_output_file)

    expect_false(
      any(grepl(
        pattern = "Computing code coverage - it might take some time",
        x = res
      ))
    )

    expect_false(
      any(grepl(
        pattern = paste(
          basename(dummypackage),
          "Coverage:"
        ),
        x = res
      ))
    )
  })

  test_that("inflate fails if a wrong function to compute coverage is used", {
    expect_error(
      inflate(
        pkg = dummypackage,
        flat_file = dev_file1,
        vignette_name = "Get started",
        check = FALSE,
        open_vignette = FALSE,
        document = TRUE,
        overwrite = "yes",
        codecov = TRUE,
        codecov_fun = "wrong"
      ),
      "codecov_fun must be either 'package_coverage' or 'report'"
    )
  })

  capture.output(
    inflate(
      pkg = dummypackage,
      flat_file = dev_file1,
      vignette_name = "Get started",
      check = FALSE,
      open_vignette = FALSE,
      document = TRUE,
      overwrite = "yes",
      codecov = TRUE,
      codecov_fun = "package_coverage"
    ),
    file = console_output_file,
    type = "message"
  )


  test_that("inflate outputs compute codecov if asked and if codecov_fun = package_coverage", {
    res <- readLines(console_output_file)

    expect_true(
      any(grepl(
        pattern = "Computing code coverage - it might take some time",
        x = res
      ))
    )

    expect_true(
      any(grepl(
        pattern = "R/flat1_rmd.R: 0.00%",
        x = res
      ))
    )


    expect_true(
      any(grepl(
        pattern = paste(
          basename(dummypackage),
          "Coverage: 0.00%"
        ),
        x = res
      ))
    )
  })


  capture.output(
    inflate(
      pkg = dummypackage,
      flat_file = dev_file1,
      vignette_name = "Get started",
      check = FALSE,
      open_vignette = FALSE,
      document = TRUE,
      overwrite = "yes",
      codecov = TRUE,
      codecov_fun = "report"
    ),
    file = console_output_file,
    type = "message"
  )


  test_that("inflate does not output compute codecov if asked and if codecov_fun = report", {
    res <- readLines(console_output_file)

    expect_true(
      any(grepl(
        pattern = "Computing code coverage - it might take some time",
        x = res
      ))
    )

    expect_false(
      any(grepl(
        pattern = "R/flat1_rmd.R: 0.00%",
        x = res
      ))
    )


    expect_false(
      any(grepl(
        pattern = paste(
          basename(dummypackage),
          "Coverage: 0.00%"
        ),
        x = res
      ))
    )
  })
})

unlink(console_output_file)
unlink(dummypackage, recursive = TRUE)
