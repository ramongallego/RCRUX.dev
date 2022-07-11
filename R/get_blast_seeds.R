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