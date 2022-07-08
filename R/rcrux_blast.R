#' A wrapper function for blast_datatable that reads a data.frame from a path
#'
#' Reads a data.frame from path,
#' then passes it to blast_datatable
#' then writes the output.
#'
#' @param path a path to a csv from get_blast_seeds
#' @param dir a directory, passed to blast_datatable for it to save to
#' @param db_dir a directory containing a blast-formatted database
#' @param expand: logical, determines whether to expand too_many_Ns and not_in
#'        db into real tables
#' @return NULL
#' @export
rcrux_blast <- function(path, dir, expand = TRUE, ...) {
    blast_seeds <- read.csv(path)
    blast_datatable(blast_seeds, dir, ...)
    # Write output
    return(NULL)
}