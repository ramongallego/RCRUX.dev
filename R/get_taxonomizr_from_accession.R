#' Attach taxonomy data to an input table
#'
#' @param input a data.frame
#' @param accessionTaxa_path the path to an accessionTaxa sql
#' @return the data.frame with taxonomy data
#' @export
get_taxonomizer_from_accession <- function(input, accessionTaxa_path) {
    input_taxids <- taxonomizr::accessionToTaxa(input$accession,
                                            accessionTaxa_path)

    input_taxonomy <- taxonomizr::getTaxonomy(input_taxids, accessionTaxa_path,
                                desiredTaxa = c("species", "superkingdom",
                                "kingdom", "phylum", "subphylum", "superclass",
                                "class", "subclass", "order", "family",
                                "subfamily", "genus", "infraorder", "subcohort",
                                "superorder", "superfamily", "tribe",
                                "subspecies", "subgenus", "species group",
                                "parvorder", "varietas"))

    # Use mutate to add colummns because it already (correctly) assumed that
    # that stuff would be in the right order and mutate uses memory well.
    output <- NULL
    return(output)
}