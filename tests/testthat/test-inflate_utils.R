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
