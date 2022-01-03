## Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)

expected_rmd_template_with_fusen_name <- readLines(system.file("flat-template-full.Rmd", package = "fusen")) %>%
  gsub("<my_package_name>", "foosen", .) %>%
  gsub("flat_template[.]Rmd", "flat_full.Rmd", .)

withr::with_dir(dummypackage, {
  test_that("Create fusen", {
    path_foosen <- file.path(dummypackage, "foosen")
    path_dev_history <- suppressMessages(create_fusen(path_foosen, template = "full", open = FALSE))

    expect_true(dir.exists(path_foosen))
    expect_true(all(file.exists(path_dev_history)))

    actual_dev_history <- readLines(path_dev_history[grepl("flat", path_dev_history)])
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
        create_fusen(path_foosen, template = "full", open = FALSE),
        "Could not create fusen project"
      )
    )

    # Can overwrite with message
    expect_message(
      create_fusen(path_foosen, template = "full", open = FALSE, overwrite = TRUE),
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
# clean
unlink(dummypackage, recursive = TRUE)

## Create in a subdirectory ----
dummysubdir <- tempfile(pattern = "subdir/subdir2/dummy")
test_that("Can create in a subdirectory", {
  expect_error(suppressMessages(create_fusen(dummysubdir, template = "full", open = FALSE)), regexp = NA)
  expect_true(dir.exists(dummysubdir))
})
unlink(dummysubdir, recursive = TRUE)

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
    dev_path <- expect_error(
      suppressMessages(
        create_fusen_gui(dummygui$basename, template = "teaching", with_git = FALSE)
      ),
      regexp = NA # expect no errors
    )
    expect_true(dir.exists(dummygui$path))
    expect_true(file.exists(dev_path))
    expect_false(dir.exists(file.path(dummygui$path, ".git/")))
  })
})
unlink(dummygui$path, recursive = TRUE)


## Test initialise git ----
# if git exists
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
git_output <- system(
  command = paste("git init", dummypackage),
  ignore.stdout = TRUE,
  ignore.stderr = TRUE
)
unlink(dummypackage, recursive = TRUE)

if (git_output != 0) {
  # No git installed on machine
  # "Error initializing git repository"
  dummypackage <- tempfile(pattern = "dummy")
  dir.create(dummypackage)
  withr::with_dir(dummypackage, {
    test_that("Create a fusen project with git at the cli", {
      path_gigit <- file.path(dummypackage, "gigit")
      expect_warning(
        create_fusen(path_gigit, template = "full", open = FALSE, with_git = TRUE),
        regexp = "Error initializing git repository")

      expect_true(dir.exists(path_gigit))
      expect_true(all(file.exists(dev_path)))
      # git is not initialized
      expect_false(dir.exists(file.path(path_gigit, ".git")))
    })
  })
  unlink(dummypackage, recursive = TRUE)
} else {
  # "Initialized git repository"

  # In cli, git TRUE
  dummypackage <- tempfile(pattern = "dummy")
  dir.create(dummypackage)
  withr::with_dir(dummypackage, {
    test_that("Create a fusen project with git at the cli", {
      path_gigit <- file.path(dummypackage, "gigit")
      dev_path <- suppressMessages(create_fusen(path_gigit, template = "full", open = FALSE, with_git = TRUE))

      expect_true(dir.exists(path_gigit))
      expect_true(all(file.exists(dev_path)))
      # git is initialized
      expect_true(dir.exists(file.path(path_gigit, ".git")))
    })
  })
  unlink(dummypackage, recursive = TRUE)

  # In cli, git FALSE
  dummypackage <- tempfile(pattern = "dummy")
  dir.create(dummypackage)
  withr::with_dir(dummypackage, {
    test_that("Create a fusen project without git at the cli", {
      path_gigit <- file.path(dummypackage, "gigit")
      dev_path <- suppressMessages(create_fusen(path_gigit, template = "full", open = FALSE, with_git = FALSE))

      expect_true(dir.exists(path_gigit))
      expect_true(all(file.exists(dev_path)))
      # git is not initialized
      expect_false(dir.exists(file.path(path_gigit, ".git")))
    })
  })
  unlink(dummypackage, recursive = TRUE)

  # In Rstudio GUI project wizard, git TRUE
  dummygui <- create_dummygui()
  withr::with_dir(dummygui$dirname, {
    test_that("Create a fusen project with git using Rstudio GUI", {
      dev_file <- expect_error(
        suppressMessages(
          create_fusen_gui(dummygui$basename, template = "teaching", with_git = TRUE)
        ),
        regexp = NA # expect no errors
      )
      expect_true(dir.exists(dummygui$path))
      expect_true(file.exists(dev_file))
      # git is initialized
      expect_true(dir.exists(file.path(dummygui$path, ".git/")))
    })
  })
  unlink(dummygui$path, recursive = TRUE)

  # In Rstudio GUI project wizard, git FALSE
  dummygui <- create_dummygui()
  withr::with_dir(dummygui$dirname, {
    test_that("Create a fusen project without git using Rstudio GUI", {
      dev_file <- expect_error(
        suppressMessages(
          create_fusen_gui(dummygui$basename, template = "teaching", with_git = FALSE)
        ),
        regexp = NA # expect no errors
      )
      expect_true(dir.exists(dummygui$path))
      expect_true(file.exists(dev_file))
      # git is not initialized
      expect_false(dir.exists(file.path(dummygui$path, ".git/")))
    })
  })
  unlink(dummygui$path, recursive = TRUE)
}
