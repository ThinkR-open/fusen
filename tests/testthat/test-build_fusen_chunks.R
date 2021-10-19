test_that("build_fusen_chunks works properly", {
  res <- build_fusen_chunks("pouet")
  expect_true(
    grepl("pouet", res)  
  )
  expect_true(
    grepl("function-pouet", res)  
  )
  expect_true(
    grepl("example-pouet", res)  
  )
  expect_true(
    grepl("function-pouet", res)  
  )
  expect_true(
    grepl("tests-pouet", res)  
  )
  expect_true(
    grepl("Title", res)  
  )
  expect_true(
    grepl("Description", res)  
  )
  expect_true(
    grepl("@return", res)  
  )
  expect_true(
    grepl("@export", res)  
  )
  res <- build_fusen_chunks("pouet", FALSE)
  expect_true(
    grepl("@noRd", res)  
  )
})
