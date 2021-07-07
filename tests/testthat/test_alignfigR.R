# Tests

# Prepare the test data...
protein_file <- system.file("extdata", "protein.fasta", package = "alignfigR")
data_longer_file <- system.file("extdata", "data_longer_test", package = "alignfigR")
tibble_fasta <- read_alignment(protein_file) 


# Read alignment tests
test_that("test that read_alignment() returns a tibble", {
  # Is it a tibble?
  expect_s3_class(
    read_alignment(protein_file),
    c("tbl_df", "tbl","data.frame")
  )
})
test_that("test that read_alignment() returns a tibble with the correct number of rows", {
  expect_equal(
    nrow(tibble_fasta), 
    263)
})

test_that("test that read_alignment() returns a tibble with the correct number of columns", {
  expect_equal(
    ncol(tibble_fasta), 
    39)
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

# filter_taxa_and_sites Tests

test_that("test that filter_taxa_and_sites() can select only desired taxa", {
  expect_equal(
    names(
      filter_taxa_and_sites(tibble_fasta, 
                            taxa = c("C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521"))), 
    c("column", "C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521"))
})

test_that("test that filter_taxa_and_sites() can select only desired sites", {
  expect_equal(
    nrow(
      filter_taxa_and_sites(
        tibble_fasta, 
        sites = c(1:150))), 
    150)
})

test_that("test that filter_taxa_and_sites() can exclude only desired taxa", {
  expect_equal(
    names(
      filter_taxa_and_sites(
        tibble_fasta, 
        taxa = c("C9EABACTA301505", "C9CABACTO298505", "C9DABACTP301521"), 
        exclude_taxa = TRUE)), 
    taxa_exclusion_test)
})

test_that("test that filter_taxa_and_sites() can exclude only desired sites", {
  expect_equal(
    nrow(
      filter_taxa_and_sites(
        tibble_fasta, 
        sites = c(1:150), exclude_sites = TRUE)), 
    113)
})

# make_data_longer Tests

test_that("test that make_data_longer() has correct column names", {
  expect_equal(
    names(
      make_data_longer(
        tibble_fasta)), 
    c("Taxa", "seq"))
})

test_that("test that make_data_longer() has correct number of rows", {
  expect_equal(
    nrow(
      make_data_longer(
        tibble_fasta)), 
    9994)
})











