## Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)

expected_rmd_template_with_fusen_name <- readLines(system.file("flat-template-full.Rmd", package = "fusen")) %>%
  gsub("<my_package_name>", "foosen", .) %>%
  gsub("flat_template[.]Rmd", "flat_full.Rmd", .)

withr::with_dir(dummypackage, {
  test_that("Create fusen works", {
    path_foosen <- file.path(dummypackage, "foosen")

    path_dev_history <- suppressMessages(
      create_fusen(path_foosen, template = "full", open = FALSE)
    )

    expect_true(dir.exists(path_foosen))
    expect_true(all(file.exists(path_dev_history)))

    expect_true(file.exists(file.path(path_foosen, ".gitignore")))
    gitignore <- readLines(file.path(path_foosen, ".gitignore"))
    expect_true(all(
      c(".Rproj.user", ".Rhistory", ".RData", ".DS_Store", ".httr-oauth") %in% gitignore
    ))

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

# Test other templates with different flat_name ----
## Create a new project

for (template.to.try in fusen:::create_fusen_choices) {
  # template.to.try <- "full"
  dummypackage <- tempfile(pattern = paste0("create.fusen.", template.to.try))
  dir.create(dummypackage)
  pkgname <- "tempfoosen"
  flat_name <- "hello"

  withr::with_dir(dummypackage, {
    test_that(paste("Create fusen works with template:", template.to.try), {
      if (template.to.try %in% "dev_history") {
        expected_rmd_template_with_fusen_name <- NULL
      } else {
        template.to.try.rmd <- template.to.try
        if (template.to.try == "minimal") {
          template.to.try.rmd <- "minimal_package"
        }

        expect_message(
          expected_rmd_template_with_fusen_name <- readLines(
            system.file(paste0("flat-template-", template.to.try.rmd, ".Rmd"), package = "fusen")
          ) %>%
            gsub("<my_package_name>", pkgname, .) %>%
            gsub("flat_template[.]Rmd", paste0("flat_", flat_name, ".Rmd"), .) %>%
            gsub("my_fun", flat_name, .),
          regexp = NA
        )
      }

      path_foosen <- file.path(dummypackage, pkgname)

      expect_error(
        path_dev_history <- suppressMessages(
          create_fusen(path_foosen, template = template.to.try, flat_name = flat_name, open = FALSE)
        ),
        regexp = NA
      )

      expect_true(dir.exists(path_foosen))
      expect_true(all(file.exists(path_dev_history)))
      if (!is.null(expected_rmd_template_with_fusen_name)) {
        expect_equal(sum(grepl("flat_hello", path_dev_history)), 1)
      } else {
        expect_equal(sum(grepl("flat_hello", path_dev_history)), 0)
      }

      expect_true(file.exists(file.path(path_foosen, ".gitignore")))
      gitignore <- readLines(file.path(path_foosen, ".gitignore"))
      expect_true(all(
        c(".Rproj.user", ".Rhistory", ".RData", ".DS_Store", ".httr-oauth") %in% gitignore
      ))

      if (!is.null(expected_rmd_template_with_fusen_name)) {
        actual_dev_history <- readLines(path_dev_history[grepl("flat", path_dev_history)])
        expect_identical(
          actual_dev_history,
          expected_rmd_template_with_fusen_name
        )
        if (template.to.try %in% c("minimal")) {
          expect_equal(sum(grepl("hello <- function", actual_dev_history)), 1)
        }
      }
    })
  })

  # clean loop
  unlink(dummypackage, recursive = TRUE)
}



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

for (template.to.try in c("full", "minimal", "teaching", "dev_history")) {
  dummygui <- create_dummygui()
  withr::with_dir(dummygui$dirname, {
    test_that(paste("Can create in a project with gui for:", template.to.try), {
      dev_path <- expect_error(
        suppressMessages(
          create_fusen_gui(dummygui$basename, template = template.to.try, with_git = FALSE)
        ),
        regexp = NA # expect no errors
      )
      expect_true(dir.exists(dummygui$path))
      expect_true(all(file.exists(dev_path)))
      expect_false(dir.exists(file.path(dummygui$path, ".git/")))
    })
  })
  unlink(dummygui$path, recursive = TRUE)
}

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
        regexp = "Error initializing git repository"
      )


      expect_true(dir.exists(path_gigit))
      expect_true(all(file.exists(c(
        file.path(path_gigit, "dev", "flat_full.Rmd"),
        file.path(path_gigit, "dev", "0-dev_history.Rmd")
      ))))
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
      expect_true(all(file.exists(c(
        file.path(path_gigit, "dev", "flat_full.Rmd"),
        file.path(path_gigit, "dev", "0-dev_history.Rmd")
      ))))
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
      expect_true(all(file.exists(c(
        file.path(path_gigit, "dev", "flat_full.Rmd"),
        file.path(path_gigit, "dev", "0-dev_history.Rmd")
      ))))
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
      expect_true(all(file.exists(
        file.path(dummygui$path, "dev", "flat_teaching.Rmd")
      )))
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
      expect_true(all(file.exists(
        file.path(dummygui$path, "dev", "flat_teaching.Rmd")
      )))
      # git is not initialized
      expect_false(dir.exists(file.path(dummygui$path, ".git/")))
    })
  })
  unlink(dummygui$path, recursive = TRUE)
}

# Test bad path for package stops soon
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
withr::with_dir(dummypackage, {
  test_that("Create a fusen project with git at the cli", {
    path_bad_name <- file.path(dummypackage, "bad-name")
    expect_error(
      create_fusen(path_bad_name, template = "full", open = FALSE),
      regexp = "Please rename"
    )

    expect_false(dir.exists(path_bad_name))
  })
})
unlink(dummypackage, recursive = TRUE)
