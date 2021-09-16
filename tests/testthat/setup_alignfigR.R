# Prepare the test data...
protein_file <- system.file("extdata", "protein.fasta", package = "alignfigR")
nucleotide_file <- system.file("extdata", "nucleotide.fasta", package = "alignfigR") 
character_file <- system.file("extdata", "character.fasta", package = "alignfigR") 
incorrect_protein_file <- file.path("data", "incorrect_protein_tibble_fasta.csv")

data_longer_file <- file.path("data", "data_longer_test.csv")

prep_geom_rect_alignment_file <- file.path("data", "create_geom_rect_alignment_test.csv")

readr::read_csv(data_longer_file) -> data_longer_test
readr::read_csv(prep_geom_rect_alignment_file) -> prep_geom_rect_alignment_test
readr::read_csv(incorrect_protein_file) -> incorrect_protein_test
tibble_fasta <- read_alignment(protein_file)
nucleotide_fasta <- read_alignment(nucleotide_file)
character_fasta <- read_alignment(character_file)
filter_taxa_and_sites(tibble_fasta,sites = c(1:150), exclude_sites = TRUE) -> filter_test
prep_site_frequencies(data_longer_test) -> prep_site_frequencies_test
determine_type(tibble_fasta, data_type = "Protein") -> user_determine_protein_test

determine_type(tibble_fasta, data_type = "Character") -> user_determine_character_test

determine_type(tibble_fasta, data_type = "Nucleotide") -> user_determine_nuc_test



