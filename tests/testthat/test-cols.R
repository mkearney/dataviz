test_that("col2hex", {
  blue <- col2hex("blue")
  expect_identical(blue, "#0000FF")
})
