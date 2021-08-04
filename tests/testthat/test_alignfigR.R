# Tests

# Read alignment tests
test_that("test that read_alignment() returns a tibble", {
  # Is it a tibble?
  expect_s3_class(
    read_alignment(protein_file),
    c("tbl_df", "tbl","data.frame")
  )
})

test_that("test that read_alignment() can recognize a non-alignment", {
  # Is it a tibble?
  expect_error(
    read_alignment(incorrect_protein_file)
  )
})

test_that("test that read_alignment() returns a tibble with the correct columns names", {
  expect_equal(
    names(
    read_alignment(protein_file)),
  tibble_fasta_names
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
  expect_equal(names(define_palette(color_palette = "custom", uniques = c("A", "C", "G", "T"), custom_colors = c("dodgerblue", "cornsilk1", "red", "black"))), c("A", "C", "G", "T"))
  expect_equal(define_palette("purpyr"), nucleotide_ry_pal)
  expect_equal(define_palette("nucleotide", type = "Nucleotide"), nucleotide_pal)
  expect_error(define_palette("nucleotide", type = "Character"))
  expect_equal(define_palette("default", type = "Protein"), protein_default_pal)
  expect_equal(define_palette("default", type = "Nucleotide"), nucleotide_default_pal)
  expect_equal(define_palette("default", type = "Character"), character_default_pal)
  expect_error(define_palette("default", type = "sfgrf"))
  expect_equal(define_palette("ocean"), protein_ocean_pal)
  expect_equal(define_palette("forest"), protein_forest_pal)
  expect_equal(define_palette("fire"), protein_fire_pal)
  expect_equal(define_palette("floral"), protein_floral_pal)
  expect_equal(define_palette("clustal", type = "Protein"), protein_clustal_pal)
  expect_equal(define_palette("clustal", type = "Nucleotide"), nucleotide_clustal_pal)
  expect_error(define_palette("clustal", type = "Character"))
  expect_equal(define_palette("zappo"), protein_zappo_pal)
  expect_equal(define_palette("taylor"), protein_taylor_pal)
  expect_equal(define_palette("hydrophobicity"), protein_hydrophobicity_pal)
  expect_equal(define_palette("helixpro"), protein_helix_propensity_pal)
  expect_equal(define_palette("strandpro"), protein_strand_propensity_pal)
  expect_equal(define_palette("turnpro"), protein_turn_propensity_pal)
  expect_equal(define_palette("buried index"), protein_buried_index_pal)
  expect_error(define_palette("notapalette"))
})

# filter_taxa_and_sites Tests

test_that("test that filter_taxa_and_sites() returns a tibble", {
  expect_s3_class(
    filter_taxa_and_sites(
      tibble_fasta),
    c("tbl_df","tbl", "data.frame"))
})

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
    sum(filter_test$column == c(151:263)),
    113)
})

# make_data_longer Tests

test_that("test that make_data_longer() returns a tibble", {
  expect_s3_class(
      make_data_longer(
        tibble_fasta),
  c("tbl_df","tbl", "data.frame"))
})

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

# create_geom_rect_alignment Test

test_that("test that create_geom_rect() returns a tibble", {
  expect_s3_class(
    create_geom_rect_alignment(
      data_longer_test),
    c("tbl_df","tbl", "data.frame"))
})

test_that("test that create_geom_rect_alignment() has correct number of rows", {
  expect_equal(
    nrow(
    create_geom_rect_alignment(data_longer_test)),
    9994)
})

test_that("test that create_geom_rect_alignment() has correct number of cols", {
  expect_equal(
    ncol(
      create_geom_rect_alignment(data_longer_test)),
    6)
})

test_that("test that create_geom_rect_alignment() has correct column names", {
  expect_equal(
    names(
      create_geom_rect_alignment(data_longer_test)),
    create_geom_rect_alignment_names)
})

# Plot Alignment
test_that("plot_alignment() returns ggplot object",{
  expect_s3_class(plot_alignment(tibble_fasta, color_palette = "forest"),
            "ggplot")
})

test_that("plot_alignment() returns ggplot object if taxon labels = TRUE",{
  expect_s3_class(plot_alignment(tibble_fasta, color_palette = "forest", taxon_labels = TRUE),
                  "ggplot")
})

# prep_site_frequencies()
test_that("prep_site_frequencies() returns tibble object",{
  expect_s3_class(prep_site_frequencies(data_longer_test),
                  c("tbl_df","tbl","data.frame"))
})

# plot_frequencies
test_that("plot_frequencies() returns ggplot object",{
  expect_s3_class(plot_frequencies(prep_site_frequencies_test),
                  "ggplot")
})

# plot_site_frequencies
test_that("plot_site_frequencies() returns ggplot object",{
  expect_s3_class(plot_site_frequencies(tibble_fasta, color_palette = "clustal"),
                  "ggplot")
})

# determine_type
test_that("determine_type() allows the user to select the correct data_type",{
  expect_true(user_determine_protein_test == "Protein")
  expect_true(user_determine_character_test == "Character")
  expect_true(user_determine_nuc_test == "Nucleotide")
  expect_error(determine_type(tibble_fasta, data_type = "shfkeht"))
  expect_true(determine_type(tibble_fasta, data_type = "") == "Protein")
  expect_true(determine_type(nucleotide_fasta, data_type = "") == "Nucleotide")
  expect_true(determine_type(character_fasta, data_type = "") == "Character")
})





