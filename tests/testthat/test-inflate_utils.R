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

# {fusen} steps
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
