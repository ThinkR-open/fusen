dummypackage <- tempfile("clean")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {

  test_that("load_flat_functions works", {
    expect_true(inherits(load_flat_functions, "function"))

    # Save in temp env for tests
    testenv <- new.env()
    expect_message(
      load_flat_functions(flat_file = flat_file, envir = testenv),
      "sourced in global env")

    expect_true(inherits(testenv$my_median, "function"))
    expect_true(inherits(testenv$my_other_median, "function"))
    expect_true(inherits(testenv$sub_median, "function"))
  })
})

unlink(dummypackage, recursive = TRUE)
