#' A wrapper function for blast_datatable that reads a data.frame from a path
#'
#' Reads a data.frame from path,
#' then passes it to blast_datatable
#' then writes the output.
#'
#' @param path a path to a csv from get_blast_seeds
#' @param dir a directory, passed to blast_datatable for it to save to
#' @param db_dir a directory containing a blast-formatted database
#' @param expand logical, determines whether to expand too_many_Ns and not_in
#'        db into real tables
#' @param accession_taxa_path a path to the accessionTaxa sql created by
#'        taxonomizr
#' @return NULL
#' @export
rcrux_blast <- function(path, dir, db_dir, accession_taxa_path, expand = TRUE,
                        ...) {
    blast_seeds <- read.csv(path)
    output_table <- blast_datatable(blast_seeds, dir, db_dir,
                    accession_taxa_path, ...)
    # Write output_table to dir/rcrux_blast_output/summary.csv
    # Read condensed vectors and expand them
    return(NULL)
}