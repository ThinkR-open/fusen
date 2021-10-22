test_that("build_fusen_chunks works properly", {
  res <- build_fusen_chunks("pouet")
  expect_true(
    grepl("pouet", res)
  )
  expect_true(
    grepl("function-pouet", res)
  )
  expect_true(
    grepl("example-pouet", res)
  )
  expect_true(
    grepl("function-pouet", res)
  )
  expect_true(
    grepl("tests-pouet", res)
  )
  expect_true(
    grepl("Title", res)
  )
  expect_true(
    grepl("Description", res)
  )
  expect_true(
    grepl("@return", res)
  )
  expect_true(
    grepl("@export", res)
  )
  res <- build_fusen_chunks("pouet", FALSE)
  expect_true(
    grepl("@noRd", res)
  )
})

# Create a new package, add set of chunks and check
dummydir <- tempfile(pattern = "dummy")
dir.create(dummydir)
path_foosen <- file.path(dummydir, "foosen")

test_that("build_fusen_chunks add lines as expected", {
  withr::with_dir(dummydir, {
    create_fusen(path_foosen, name = "minimal", open = FALSE)
    fill_description(pkg = path_foosen, fields = list(Title = "Dummy Package"))
    path_dev_history <- file.path(path_foosen, "dev", "dev_history.Rmd")
    dev_lines_orig <- readLines(path_dev_history)

    # If interactive in RStudio
    if (
      requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("navigateToFile") &&
      rstudioapi::hasFun("documentSave") #&&
      # rstudioapi::hasFun("documentClose")
    ) {
      print("Test with RStudio")
      # current position
      curr_editor <- rstudioapi::getSourceEditorContext()
      curr_position <- curr_editor$selection[[1L]]$range$start
      # Change file
      rstudioapi::navigateToFile(path_dev_history, line = 33)
      Sys.sleep(1)
      open_editor <- rstudioapi::getSourceEditorContext()
      id <- open_editor$id
      # the_open_path <- rstudioapi::documentPath(id)
      # if (basename(the_open_path) != basename(path_dev_history)) {
      #   stop("Open the file was short to get the correct Id of the opened file")
      # }
        # add chunks
        add_fusen_chunks(function_name = "zaza", export = TRUE)
        rstudioapi::documentSave(id)
        if (rstudioapi::hasFun("documentClose")) {
          rstudioapi::documentClose(id)
        }
        # # Back to current position
        rstudioapi::navigateToFile(curr_editor$path, line = curr_position[1])
    } else {
      print("test without interactive")
      dev_lines <- readLines(path_dev_history)
      dev_lines[33] <- build_fusen_chunks("zaza")
      cat(dev_lines, sep = "\n", file = path_dev_history)
    }

    dev_lines_new <- readLines(path_dev_history)
    expect_equal(length(dev_lines_new), length(dev_lines_orig) + 24)

    unlink(path_foosen, recursive = TRUE)
  })
})
