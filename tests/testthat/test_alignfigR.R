# Tests

read_alignment("Data/protein.fasta") -> tibble_fasta

# Read alignment tests

test_that("test that read_alignment() returns a dataframe", {
  expect_true(is.data.frame(tibble_fasta))
})

test_that("test that read_alignment() returns a tibble with the correct number of rows", {
  expect_equal(nrow(tibble_fasta), 263)
})

test_that("test that read_alignment() returns a tibble with the correct number of columns", {
  expect_equal(ncol(tibble_fasta), 39)
})

# Extract subalign tests

test_that("test that extract_subalign() returns a tibble with the correct number of rows", {
  expect_equal(nrow(extract_subalign(tibble_fasta)), 9994)
})

test_that("test that extract_subalign() returns a tibble with the correct number of columns", {
  expect_equal(ncol(extract_subalign(tibble_fasta)), 6)
})

test_that("test that extract_subalign() returns a tibble with the correctly named columns", {
  expect_equal(names(extract_subalign(tibble_fasta)), c("Taxa", "seq", "x1", "x2", "y1","y2"))
})

test_that("test that extract_subalign() returns a tibble with the correct x1 column", {
  expect_equal(sum(extract_subalign(tibble_fasta)$x1 == rep(1:263, 9994/263)), 9994)
})

test_that("test that extract_subalign() returns a tibble with the correct x2 column", {
  expect_equal(sum(extract_subalign(tibble_fasta)$x2 == rep(2:264, 9994/263)), 9994)
})

test_that("test that extract_subalign() returns a tibble with the correct y1 column", {
  expect_equal(sum(extract_subalign(tibble_fasta)$y1 == c(t(replicate(263, 1:(9994/263))))), 9994)
})

test_that("test that extract_subalign() returns a tibble with the correct y2 column", {
  expect_equal(sum(extract_subalign(tibble_fasta)$y2 == c(t(replicate(263, 2:39)))), 9994)
})

test_that("test that the clist in extract_subalign() functions correctly", {
  expect_equal(nrow(extract_subalign(tibble_fasta, clist = 1:10)), 380)
})

test_that("test that the tlist in extract_subalign() functions correctly", {
  expect_equal(sum(extract_subalign(tibble_fasta, tlist = c("C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521"))$Taxa %in% c("C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521")), 789)
})

# Define Palette Tests

test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("dna"), nucleotide_pal)
  expect_equal(define_palette("rna"), nucleotide_pal)
  expect_equal(define_palette("free"), free_pal)
  expect_equal(define_palette("ocean"), ocean_pal)
  expect_equal(define_palette("forest"), forest_pal)
  expect_equal(define_palette("fire"), fire_pal)
  expect_equal(define_palette("floral"), floral_pal)
  expect_error(define_palette("notapalette"))
})





