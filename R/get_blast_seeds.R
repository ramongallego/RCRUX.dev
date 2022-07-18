#' Function to query NCBI's primer_blast tool, append the taxonomy, and
#' output the results as .csvs
#'
#' @param forward_primer passed to primer_search, which turns it into a list of
#'        each primer it could be based on its degenerate primers, then passes
#'        each one in turn to NCBI
#' @param reverse_primer passed to primer_search, which turns it into a list of
#'        each primer it could be based on its degenerate primers, then passes
#'        each one in turn to NCBI
#' @param file_out_dir the parent directory to place the data in.
#'        get_blast_seeds does not attempt to create a new directory, so
#'        file_out_dir should already exist
#' @param Metabarcode_name used to name the subdirectory and the files. If a
#'        directory named Metabarcode_name does not exist in file_out_dir, a
#'        new directory will be created. get_blast_seeds appends
#'        Metabarcode_name to the beginning of each of the four files it
#'        generates.
#' @param accessionTaxa the path to sql created by taxonomizr
#' @param organism a vector of character vectors. Each character vector is
#'        passed in turn to primer_search, which passes them to NCBI.
#'        get_blast_seeds aggregates all of the results into a single file.
#' @param minimum_length parse_primer_hits returns a table with a product_length
#'        column. get_blast_seeds removes each row that has a value less than
#'        minimum_length in the product_length column.
#' @param maximum_length parse_primer_hits returns a table with a
#'        product_length column. get_blast_seeds removes each row that has a
#'        value greater than maximum_length in the product_length column
#' @param num_permutations passed to primer_search, which passes it to NCBI
#' @param primer_specificity_database passed to primer_search, which passes it
#'        to NCBI
#' @param hitsize passed to primer_search, which passes it to NCBI
#' @param evalue passed to primer_search, which passes it to NCBI
#' @param word_size passed to primer_search, which passes it to NCBI
#' @param MAX_TARGET_PER_TEMPLATE passed to primer_search, which passes it to
#'        NCBI
#' @param NUM_TARGETS_WITH_PRIMERS passed to primer_search, which passes it to
#'        NCBI
#' @param ... additional arguments passed to primer_search, which passes it to
#'        NCBI
#' @return a tibble containing the same information as the csv it generates
#' @export
get_blast_seeds <- function(forward_primer, reverse_primer,
                            file_out_dir, Metabarcode_name,
                            accessionTaxa,
                            organism, mismatch = 3,
                            minimum_length = 5, maximum_length = 500,
                            primer_specificity_database = "nt", ...,
                            return_table = TRUE) {

    # Start by making the directory and checking for the sql and whatnot.
    dir.create(file.path(paste0(file_out_dir, Metabarcode_name)))
    if (!file.exists(accessionTaxa)) {
      stop("accessionTaxa does not exist")
    }
    out <- paste0(file_out_dir, Metabarcode_name, "/")

    # Aggregate the primer_search return values
    # Then parse_primer_hits all of them
    results_table <- iterative_primer_search(forward_primer, reverse_primer,
                                            organism,
                                            primer_specificity_database, ...)
    # Throw an error if there are no results
    if (nrow(results_table) < 1) {
      stop("Primer search returned no hits.")
    }

    taxonomized_results_table <-
      get_taxonomizr_from_accession(results_table, accessionTaxa)
    filtered_results_table <- filter_primer_hits(taxonomized_results_table,
                                                forward_primer, reverse_primer,
                                                mismatch, minimum_length,
                                                maximum_length)

    # save output
    save_output_as_csv(taxonomized_results_table,
                        "_primerTree_output_with_taxonomy", out,
                        Metabarcode_name)
    save_output_as_csv(filtered_results_table, "_raw_primerTree_output", out,
                        Metabarcode_name)

    #return if you're supposed to
    if (return_table) {
      return(taxonomized_results_table)
    }
    else {
      return(NULL)
    }
}