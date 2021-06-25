# Tests
# Define Palette Tests
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("dna"), nucleotide_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("rna"), nucleotide_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("free"), free_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("ocean"), ocean_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("forest"), forest_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("fire"), fire_pal)
  expect_error(define_palette("notapalette"))
})
test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("floral"), floral_pal)
  expect_error(define_palette("notapalette"))
})


