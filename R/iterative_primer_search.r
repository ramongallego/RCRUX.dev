#' Call primer_search with several parameters and aggregate the results
#'
#' This function acts like primer_search and parse_primer hits all in one. Its
#' parameters are very similar to primer_search, but it takes vectors for
#' organism and for database and performs a primer search for each combination.
#' It downgrades errors from primer_search and parse_primer_hits into warnings.
#' This is useful when searching for a large number of different combinations,
#' allowing the function to output successful results.
#'
#' @param forward the forward primer
#' @param reverse the reverse primer
#' @param organisms a character vector containing an id or name parseable by
#'        NCBI as an organism. If it is a vector with multiple entries, each
#'        entry will be queried separately.
#' @param which NCBI databases to search. If it is a vector with multiple
#'        entries, each entry will be queried separately.
#' @return a data.table summarizing the results of several primer_searches
#' @export
iterative_primer_search <- function(forward, reverse, organisms,
                                    databases = "nt", ...) {
    output <- NA
    # Use for loops to iterate over all the vector options
    for (org in organisms) {
        for (db in databases) {
            response <- try(
                primerTree::primer_search(forward, reverse, organism = org,
                    primer_specificity_database = db, ...),
                silent = TRUE
            )
            if (class(response) == "try-error") {
                # To do: include useful metadata and messages
                msg <- conditionMessage(attr(response, "condition"))
                warning(msg)
            }
            else {
                # Splice the parse onto the output
                for (r in response) {
                    parsed <- try(
                        primerTree::parse_primer_hits(r),
                        silent = TRUE
                    )
                    if (class(parsed) == "try-error") {
                        # To do: include useful metadata and messages
                        msg <- conditionMessage(attr(response, "condition"))
                        warning(msg)
                        message("This occurred while processing organism ", org,
                                " and database ", db, ".")
                    }
                    else if (!is.data.frame(parsed)) {
                        warning("parse_primer_hits returned an object that is
                                not a dataframe. It will be ignored.")
                        message("This occurred while processing organism ", org,
                                " and database ", db, ".")
                    }
                    else {
                        # turn it into a data.table
                        # because I think that makes this faster?
                        data.table::setDT(parsed)
                        parsed <- dplyr::mutate(parsed, database = db)

                        # NA is the case
                        # where it has not really been initialized
                        # Why not initialize it as an empty data.table?
                        # This is a hedge against parse_primer_hits
                        # changing the output format
                        # So this doesn't really work because is.na is done on a
                        # Whole vector rather than on the object as a whole
                        if (is.na(output)) {
                            output <- parsed
                        }
                        else {
                            output <- tibble::add_row(output, parsed)
                        }
                    }
                }
            }
        }
    }

    # remove duplicate rows
    output <- dplyr::distinct(output)
    return(output)
}