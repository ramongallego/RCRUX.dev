#' @param forward the forward primer
#' @param reverse the reverse primer
#' @param organism a character vector containing an id or name parseable by NCBI
#'        as an organism. If it is a vector with multiple entries, each entry
#'        will be queried separately.
#' @param which NCBI database to search. If it is a vector with multiple
#'        entries, each entry will be queried separately.
#' @return a data.table summarizing the results of several primer_searches
#' @export
iterative_primer_search <- function(forward, reverse, organism, database, ...) {
    output <- NULL
    # Use for loops to iterate over all the vector options
    # Note that for loops do *not* struggle with non-vectors
    # (They just treat the single thing as a single thing)
    # Use tibble::add_row() because I *think* it shallow copies and is therefore
    # a little faster?
    # Add a database column as you go
    # Possibly a column for other options?
    # wrap each search in a try
    # wrap each parse in a try
    # remove duplicate rows
    output <- dplyr::distinct(output)
    return(output)
}