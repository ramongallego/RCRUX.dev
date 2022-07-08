#' Attach taxonomy data to an input table
#'
#' @param input a data.frame
#' @param accessionTaxa_path the path to an accessionTaxa sql
#' @return the data.frame with taxonomy data
#' @export
get_taxonomizer_from_accession <- function(input, accessionTaxa_path) {
    input_taxid <- taxonomizr::accessionToTaxa(input$accession,
                                            accessionTaxa_path)

    input_taxonomy <- taxonomizr::getTaxonomy(input_taxid, accessionTaxa_path,
                                desiredTaxa = c("species","superkingdom",
                                "kingdom", "phylum", "subphylum", "superclass",
                                "class", "subclass", "order", "family",
                                "subfamily", "genus", "infraorder", "subcohort",
                                "superorder", "superfamily", "tribe",
                                "subspecies", "subgenus", "species group",
                                "parvorder", "varietas"))

    input_taxonomy <- cbind("accession" = input$accession,
                            "taxID" = input_taxid, input_taxonomy)
    input_taxonomy <- tibble::as_tibble(input_taxonomy)
    # Join the blast output and taxonomy tibbles
    return(dplyr::full_join(input, input_taxonomy, by = "accession"))
}