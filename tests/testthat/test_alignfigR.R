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




