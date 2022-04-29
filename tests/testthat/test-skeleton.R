test_that("rmardown skeleton exists", {
  skip_if_not(interactive())
  withr::with_tempdir({
    expect_error(
      rmarkdown::draft(
        file = "flat_skeleton.Rmd",
        template = "additional",
        package = "fusen", edit = FALSE
      ),
      regexp = NA
    )
  })
})
