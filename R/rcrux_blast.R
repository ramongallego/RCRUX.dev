#' A wrapper function for blast_datatable that reads a data.frame from a path
#'
#' Reads a data.frame from path,
#' then passes it to blast_datatable
#' then writes the output.
#'
#' @param seeds_path a path to a csv from get_blast_seeds
#' @param db_dir a directory containing a blast-formatted database
#' @param accession_taxa_path a path to the accessionTaxa sql created by
#'        taxonomizr
#' @param working_dir a directory in which to save partial and complete output
#' @param expand_vectors logical, determines whether to expand too_many_Ns
#'        and not_in db into real tables
#' @return NULL
#' @export
rcrux_blast <- function(seeds_path, db_dir, accession_taxa_path, working_dir,
                        expand_vectors = TRUE, ...) {
    output_dir <- paste(working_dir, "rcrux_blast_output", sep = "/")
    save_dir <- paste(working_dir, ".rcrux_blast_save", sep = "/")
    dir.create(save_dir)
    dir.create(output_dir)
    blast_seeds <- read.csv(seeds_path)
    output_table <- blast_datatable(blast_seeds, save_dir, db_dir,
                    accession_taxa_path, ...)
    # Write output_table to dir/rcrux_blast_output/summary.csv
    summary_csv_path <- paste(output_dir, "summary.csv", sep = "/")
    write.csv(output_table, file = summary_csv_path)
    # Read condensed vectors and expand them
    if (expand_vectors) {
        too_many_ns_path <- paste(save_dir, "too_many_ns.txt", sep = "/")
        too_many_ns_indices <- as.numeric(readLines(too_many_ns_path))
        too_many_ns <- blast_seeds[too_many_ns_indices, ]
        too_many_ns_csv_path <- paste(output_dir, "too_many_ns.csv", sep = "/")
        write.csv(too_many_ns, file = too_many_ns_csv_path)

        not_in_db_path <- paste(save_dir, "not_in_db.txt", sep = "/")
        not_in_db_indices <- as.numeric(readLines(not_in_db_path))
        not_in_db <- blast_seeds[not_in_db_indices, ]
        not_in_db_csv_path <- paste(output_dir, "not_in_db.csv", sep = "/")
        write.csv(not_in_db, file = not_in_db_csv_path)
    }
    return(NULL)
}