# Tests

test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("floral"), floral_pal)
  expect_error(define_palette("notapalette"))
})

