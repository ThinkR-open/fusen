# Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
pkg_name <- basename(dummypackage)

test_that("fill_description adds DESCRIPTION", {
  expect_message(
    fill_description(
      pkg = dummypackage,
      fields = list(
        Title = "Build a package from Rmarkdown file",
        Description = "Use Rmarkdown First method to build your package. Start your package with documentation. Everything can be set from a Rmarkdown file in your project.",
        `Authors@R` = c(
          person("Jack", "Doe", email = "jack@email.me", role = c("aut", "cre"), comment = c(ORCID = "0000-0000-0000-0001")),
          person(given = "ThinkR", role = "cph")
        )
      )
    ),
    "Title was modified to 'Title Case'"
  )

  expect_true(file.exists(file.path(dummypackage, "DESCRIPTION")))
  lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  expect_true(lines[1] == paste0("Package: ", pkg_name))
  expect_true(any(grepl("Jack", lines)))
  expect_false(any(grepl("John", lines)))
  expect_false(any(grepl("Sébastien", lines)))

  # Second launch error and no change
  expect_error(
    expect_message(
      fill_description(
        pkg = dummypackage, fields = list(Title = "Second launch")
      )
    )
  )

  lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  expect_true(lines[1] == paste0("Package: ", pkg_name))

  # Title Case changed
  expect_equal(lines[3], "Title: Build A Package From Rmarkdown File")
})

# Fill description message and corrected if malformed, and set overwrite
# _not dot
test_that("no dot description fails", {
  expect_message(
    fill_description(
      pkg = dummypackage,
      fields = list(
        Title = "Build A Package From Rmarkdown file",
        Description = paste(
          "Use Rmd First method to build your package.",
          "Start your package with documentation.",
          "Everything can be set from a Rmarkdown file in your project"
        ),
        `Authors@R` = c(
          person("Jack", "Doe",
            email = "jack@email.me",
            role = c("aut", "cre"), comment = c(ORCID = "0000-0000-0000-0001")
          ),
          person(given = "ThinkR", role = "cph")
        )
      ), overwrite = TRUE
    ),
    "A dot was added."
  )

  lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  # Description with dot
  expect_equal(
    lines[4],
    paste(
      "Description: Use Rmd First method to build your package.",
      "Start your package with documentation.",
      "Everything can be set from a Rmarkdown file in your project."
    )
  )
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Verify curly bracket in title works
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)

test_that("curly bracket in title and description works", {
  # Works with {} in text although not allowed by CRAN
  expect_message(fill_description(
    pkg = dummypackage,
    fields = list(
      Title = "Build a package with {fusen}",
      Description = "Use Rmarkdown First method to build your package with {fusen}.",
      `Authors@R` = c(
        person("Jack", "Doe", email = "jack@email.me", role = c("aut", "cre"), comment = c(ORCID = "0000-0000-0000-0001")),
        person(given = "ThinkR", role = "cph")
      )
    )
  ), "Title Case")
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Test capwords ----
test_that("capwords works", {
  expect_equal(capwords("using AIC for model selection"), "Using AIC For Model Selection")
})
