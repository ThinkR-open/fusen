# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

fill_description(
  pkg = dummypackage,
  fields = list(
    Title = "Build A Package From Rmd file",
    Description = "Use Rmd First method to build your package. Start your package with documentation. Everything can be set from a Rmd file in your project.",
    `Authors@R` = c(
      person("Sebastien", "Rochette", email = "sebastien@thinkr.fr", role = c("aut", "cre"), comment = c(ORCID = "0000-0002-1565-9313")),
      person(given = "ThinkR", role = "cph")
    )
  )
)

test_that("fill_description adds DESCRIPTION", {
  expect_true(file.exists(file.path(dummypackage, "DESCRIPTION")))
  lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  expect_true(lines[1] == "Package: dummypackage")

  # Second launch error and no change
  expect_error(fill_description(
    pkg = dummypackage, fields = list(Title = "Second launch")
  ))
  lines <- readLines(file.path(dummypackage, "DESCRIPTION"))
  expect_true(lines[1] == "Package: dummypackage")
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
