# Tests

# Prepare the test data...
protein_file <- system.file("extdata", "protein.fasta", package = "alignfigR") 
#tibble_fasta <- read_alignment(protein_file) 
#readr::write_csv(tibble_fasta, "data/protein_tibble_fasta.csv")

# Read alignment tests

test_that("test that read_alignment() returns a tibble", {
  # Is it a tibble?
  expect_s3_class(
    read_alignment(protein_file),
    c("tbl_df", "tbl","data.frame")
  )
})

# TODO: check the right style for prepping data used in tests
tibble_fasta <- read_alignment(protein_file)

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
  expect_equal(ncol(extract_subalign(tibble_fasta)), 7)
})

test_that("test that extract_subalign() returns a tibble with the correctly named columns", {
  expect_equal(
    names(extract_subalign(tibble_fasta)), 
    c("Taxa", "seq", "x1", "x2", "y1","y2", "column")
  )
})

test_that("test that extract_subalign() returns a tibble with the correct x1 column", {
  expect_equal(
    sum(extract_subalign(tibble_fasta)$x1 == rep(1:263, 9994/263)), 
    9994
  )
})

test_that("test that extract_subalign() returns a tibble with the correct x2 column", {
  expect_equal(
    sum(extract_subalign(tibble_fasta)$x2 == rep(2:264, 9994/263)), 
    9994
  )
  
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
  check_array <- c("C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521")
  expect_equal(
    sum(extract_subalign(tibble_fasta, tlist = check_array)$Taxa %in% check_array), 
    789
  )
})

# Define Palette Tests

test_that("test that define_palette() returns the correct palette", {
  expect_equal(define_palette("dna"), nucleotide_pal)
  expect_equal(define_palette("rna"), nucleotide_pal)
  expect_equal(define_palette("basic"), basic_pal)
  expect_equal(define_palette("ocean"), ocean_pal)
  expect_equal(define_palette("forest"), forest_pal)
  expect_equal(define_palette("fire"), fire_pal)
  expect_equal(define_palette("floral"), floral_pal)
  expect_error(define_palette("notapalette"))
})





