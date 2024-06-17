# Test users full classical process ----
all_templates_second <- fusen:::flat_template_choices[!fusen:::flat_template_choices %in% c("dev_history", "dev")]
test_that("create_fusen and use it as users do", {
  expect_length(all_templates_second, 9)
})

for (template in all_templates_second) {
  # template <- all_templates_second[3]
  dummypackage <- tempfile(pattern = "all.templates.second.flat")
  dir.create(dummypackage)

  path_foosen <- file.path(dummypackage, "foosen.process")

  # Create new fusen project
  paths_dev_history <- suppressMessages(
    create_fusen(path_foosen, template = "minimal", flat_name = "first", open = FALSE)
  )

  orig.proj <- here::here()
  # Now should be inside a project
  usethis::with_project(path_foosen, {
    here:::do_refresh_here(path_foosen)

    # Follow dev_history
    suppressMessages(
      fill_description(
        pkg = here::here(),
        fields = list(Title = "Dummy Package")
      )
    )
    # Define License with use_*_license()
    suppressMessages(usethis::use_mit_license("John Doe"))

    # Inflate first flat file
    suppressMessages(inflate(
      flat_file = "dev/flat_first.Rmd",
      vignette_name = "My First",
      open_vignette = FALSE,
      check = FALSE
    ))

    test_that(paste0("full process -", template, "- first minimal basis ok"), {
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("dev/flat_first.Rmd"))
      expect_true(file.exists("R/first.R"))
      expect_true(file.exists("man/first.Rd"))
      expect_true(file.exists("tests/testthat/test-first.R"))
      expect_true(file.exists("vignettes/my-first.Rmd"))
      expect_true(file.exists("NAMESPACE"))
    })

    # Add a new flat file with new function
    test_that(paste0("full process -", template, "- add and inflate new template"), {
      expect_error(
        add_flat_template(template = template, flat_name = "second", open = FALSE),
        regexp = NA
      )
      expect_true(file.exists("dev/flat_second.Rmd"))

      # Inflate second flat file
      suppressMessages(inflate(
        flat_file = "dev/flat_second.Rmd",
        vignette_name = "My Second",
        open_vignette = FALSE,
        check = FALSE
      ))
      expect_true(file.exists("vignettes/my-second.Rmd"))


      # Depends on template
      if (template %in% c("full")) {
        expect_true(file.exists("R/my_median.R"))
        expect_true(file.exists("man/my_median.Rd"))
        expect_true(file.exists("tests/testthat/test-my_median.R"))
      } else if (template %in% c("teach", "teaching")) {
        expect_true(file.exists("R/add_one.R"))
        expect_true(file.exists("man/add_one.Rd"))
        expect_true(file.exists("tests/testthat/test-add_one.R"))
      } else {
        expect_true(file.exists("R/second.R"))
        expect_true(file.exists("man/second.Rd"))
        expect_true(file.exists("tests/testthat/test-second.R"))
      }
    })

    # inflate_all
    test_that(paste0("full process -", template, "- inflate_all x2"), {
      expect_error(suppressMessages(inflate_all_no_check()), regexp = NA)
      expect_error(suppressMessages(inflate_all_no_check()), regexp = NA)
    })

    here:::do_refresh_here(orig.proj)
  })


  unlink(dummypackage, recursive = TRUE)
} # end of loop on template knit
