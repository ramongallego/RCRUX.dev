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