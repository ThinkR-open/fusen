## Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)

raw_rmd_template <- readLines(system.file("dev-template-full.Rmd", package = "fusen"))
expected_rmd_template_with_fusen_name <- gsub("<my_package_name>", "foosen", raw_rmd_template)

withr::with_dir(dummypackage, {
  test_that("Create fusen", {
    path_foosen <- file.path(dummypackage, "foosen")
    path_dev_history <- file.path(path_foosen, "dev", "dev_history.Rmd")
    create_fusen(path_foosen, name = "full", open = FALSE)

    expect_true(dir.exists(path_foosen))
    expect_true(file.exists(path_dev_history))

    actual_dev_history <- readLines(path_dev_history)
    expect_identical(
      actual_dev_history,
      expected_rmd_template_with_fusen_name
    )
  })
})


## Test overwrite existing dir ----
withr::with_dir(dummypackage, {
  test_that("Overwrite fusen dir is possible with message", {
    path_foosen <- file.path(dummypackage, "foosen")

    # Can not overwrite
    expect_error(
      expect_message(
        create_fusen(path_foosen, name = "full", open = FALSE),
        "Could not create fusen project"
      )
    )

    # Can overwrite with message
    expect_message(
      create_fusen(path_foosen, name = "full", open = FALSE, overwrite = TRUE),
      "Some files may be overwritten"
    )

    # Test with rstudioapi ----
    if (requireNamespace("rstudioapi") && rstudioapi::isAvailable()) {
      expect_true(file.exists(file.path(path_foosen, "foosen.Rproj")))
    } else {
      expect_true(file.exists(file.path(path_foosen, ".here")))
    }
  })
})


## Create in a subdirectory ----
dummysubdir <- tempfile(pattern = "subdir/subdir2/dummy")
test_that("Can create in a subdirectory", {
  expect_error(create_fusen(dummysubdir, name = "full", open = FALSE), regexp = NA)
  expect_true(dir.exists(dummysubdir))
})


## Test gui ----

create_dummygui <- function() {
  dummygui_path <- tempfile(pattern = "dummygui")
  dummygui <- list(
    path = dummygui_path,
    dirname = dirname(dummygui_path),
    basename = basename(dummygui_path)
  )
  return(dummygui)
}

dummygui <- create_dummygui()
withr::with_dir(dummygui$dirname, {
  test_that("Can create in a subdirectory", {
    expect_error(
      create_fusen_gui(dummygui$basename, name = "teaching", with_git = FALSE),
      regexp = NA # expect no errors
    )
    expect_true(dir.exists(dummygui$path))
    expect_true(file.exists(file.path(dummygui$path, "dev", "dev_history.Rmd")))
    expect_false(dir.exists(file.path(dummygui$path, ".git/")))
  })
})
unlink(dummygui$path, recursive = TRUE)


## Test initialise git ----

# In cli
withr::with_dir(dummypackage, {
  test_that("Create a fusen project with git at the cli", {
    path_gigit <- file.path(dummypackage, "gigit")
    path_dev_history <- file.path(path_gigit, "dev", "dev_history.Rmd")

    create_fusen(path_gigit, name = "full", open = FALSE, with_git = TRUE)

    expect_true(dir.exists(path_gigit))
    expect_true(file.exists(path_dev_history))
    # git is initialised
    expect_true(dir.exists(file.path(path_gigit, ".git")))
  })
})

# In Rstudio GUI project wizard
dummygui <- create_dummygui()
withr::with_dir(dummygui$dirname, {
  test_that("Create a fusen project with git using Rstudio GUI", {
    expect_error(
      create_fusen_gui(dummygui$basename, name = "teaching", with_git = TRUE),
      regexp = NA # expect no errors
    )
    expect_true(dir.exists(dummygui$path))
    expect_true(file.exists(file.path(dummygui$path, "dev", "dev_history.Rmd")))
    # git is initialised
    expect_true(dir.exists(file.path(dummygui$path, ".git/")))
  })
})
unlink(dummygui$path, recursive = TRUE)


unlink(dummypackage, recursive = TRUE)
unlink(dummysubdir, recursive = TRUE)