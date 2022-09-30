library(here)
library(tidyverse)


raw_table <- read_csv(here("test_data/12S_V5F1_raw_primerTree_output.csv"))

forward_primer <- "TAGAACAGGCTCCTCTAG"
reverse_primer<- "TTAGATACCCCACTATGC"
mismatch <- 3
minimum_length <- 5

maximum_length <- 500
accessionTaxa <- "~/Projects/rCRUX_deployed/accessionTaxa.sql"

## Does filter results work here

filtered_table <- filter_primer_hits(raw_table,
                                     forward_primer, reverse_primer,
                                     mismatch, minimum_length,
                                     maximum_length)

## Yes

## Does get taxonomizr work

taxonomized_table <- get_taxonomizr_from_accession(filtered_table,
                                                   accessionTaxa)


