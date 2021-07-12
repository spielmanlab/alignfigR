# Prepare the test data...
protein_file <- system.file("extdata", "protein.fasta", package = "alignfigR")

data_longer_file <- file.path("data", "data_longer_test.csv")

create_geom_rect_alignment_file <- file.path("data", "create_geom_rect_alignment_test.csv")

readr::read_csv(data_longer_file) -> data_longer_test
readr::read_csv(create_geom_rect_alignment_file) -> create_geom_rect_alignment_test
tibble_fasta <- read_alignment(protein_file)
