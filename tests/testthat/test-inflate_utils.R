print("==== inflate_utils ====")

# group_code groups columns ----
df <- tibble::tibble(
  id = c(1, 2, 3),
  the_group = c("A", "A", "B"),
  the_code = list(
    c("text 1.1", "text 1.2"), c("text 2.1", "text 2.2"), c("text 3.1", "text 3.2"))
)

test_that("group_code groups columns", {
  df_grouped <- group_code(df, group_col = "the_group", code_col = "the_code")
  expect_equal(nrow(df_grouped), 2)
  expect_equal(df_grouped[["id"]], c(1, 3))
  expect_equal(df_grouped[["the_group"]], c("A", "B"))
  expect_equal(df_grouped[["the_code"]],
               list(
                 c("text 1.1", "text 1.2", "", "text 2.1", "text 2.2"),
                 c("text 3.1", "text 3.2")))
})

# Test is_pkg_proj ----
# Create a new project
dummypackage <- tempfile("isrproj.pkg")
dir.create(dummypackage)


fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  test_that("is_pkg_proj works when no rproj", {
    expect_equal(is_pkg_proj(dummypackage), NA)
  })

  file.remove(file.path(dummypackage, ".here"))
  cat("", file = file.path(dummypackage, 'dummy.Rproj'))

  test_that("is_pkg_proj works when rproj no pkg", {
    expect_equal(is_pkg_proj(dummypackage), FALSE)
  })

  usethis::with_project(dummypackage, {
    inflate(pkg = dummypackage, flat_file = flat_file,
            vignette_name = "Get started", check = FALSE,
            open_vignette = FALSE)
    # See RStudio restart needed
  })

  test_that("is_pkg_proj works when rproj no pkg", {
    expect_equal(is_pkg_proj(dummypackage), FALSE)
  })

  cat("BuildType: Package", file = file.path(dummypackage, 'dummy.Rproj'))

  test_that("is_pkg_proj works when rproj is pkg", {
    expect_equal(is_pkg_proj(dummypackage), TRUE)
  })
})

# asciify_name ----
test_that(
  "Diacritics are properly escaped in vignette file name", {
    vignette_name <- asciify_name(
      "\u00c0 l'or\u00e9e de l'\u00e2pre f\u00f4ret c\u00e9l\u00e8ste"
    )
    expect_identical(vignette_name, "a-l-oree-de-l-apre-foret-celeste")

    vignette_name <- asciify_name(
      "# y  _ p n@ \u00E9 ! 1"
    )
    expect_identical(vignette_name, "y-p-n-e-1")
  })

# create_vignette_head ----
yaml_options <- structure(list(
  title = "dev_history.Rmd for working package",
  author = "S\\u00e9bastien Rochette", date = "`r Sys.Date()`",
  output = "html_document", editor_options = list(chunk_output_type = "console")
), class = "rmd_yaml_list")



test_that("create_vignette_head works", {
  # Full with authors
  output <- create_vignette_head(
    pkg = "mypkg", vignette_name = "the_name",
    yaml_options)

  expect_true(grepl("mypkg", output))
  expect_true(grepl("the_name", output))
  expect_true(grepl('author: "S\\u00e9bastien Rochette"', output, fixed = TRUE))
  expect_true(grepl('date: "`r Sys.Date()`"', output, fixed = TRUE))
  expect_true(grepl("vignette: >", output))

  # Only not extra yaml
  yaml_options <- yaml_options[c("title", "output", "editor_options")]
  output <- create_vignette_head(
    pkg = "mypkg", vignette_name = "the_name",
    yaml_options)

  expect_true(grepl("mypkg", output))
  expect_true(grepl("the_name", output))
  expect_false(grepl('author: "S\\u00e9bastien Rochette"', output, fixed = TRUE))
  expect_false(grepl('date: "`r Sys.Date()`"', output, fixed = TRUE))
  expect_true(grepl("vignette: >", output))

  # No yaml options
  output <- create_vignette_head(
    pkg = "mypkg", vignette_name = "the_name",
    yaml_options = NULL)

  expect_true(grepl("mypkg", output))
  expect_true(grepl("the_name", output))
  expect_false(grepl('author: "S\\u00e9bastien Rochette"', output, fixed = TRUE))
  expect_false(grepl('date: "`r Sys.Date()`"', output, fixed = TRUE))
  expect_true(grepl("vignette: >", output))

})

# Test create_vignette_head ----

# Create a new project
dummypackage <- tempfile("vignette.head")
dir.create(dummypackage)

# {fusen} steps

test_that("create_vignette_head works", {
  expect_error(fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
               , regexp = NA)
  usethis::with_project(dummypackage, {
    head <- create_vignette_head(pkg = dummypackage, vignette_name = "My Super Vignette")
    expect_true(grepl('title: "My Super Vignette"', head))
    expect_true(grepl('  %\\VignetteIndexEntry{My Super Vignette}', head, fixed = TRUE))
    expect_true(grepl(paste0('library(', basename(dummypackage) ,')'), head, fixed = TRUE))
  })
})
unlink(dummypackage, recursive = TRUE)
