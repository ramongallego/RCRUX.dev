#' Extracts query parameters from a row and calls blastdbcmd
#'
#' Given a row from a blast_seed formatted data.frame, extracts arguments
#' for blastdbcmd, then returns the output of the blastdbcmd call.
#' 
#' @param row a row from get_blast_seeds
#' @param db_dir a directory with a blast-formatted database
#' @param ncbi_bin if not null use it as the parent directory for blastn
#' @return a fasta-formatted string
#' @export
run_blastdbcmd <- function(query_row, db_dir, ncbi_bin = NULL) {
    # Extract arguments
    accession <- query_row$accession
    db <- query_row$database
    if (db ==  "refseq_representative_genomes") {
        if (query_row$superkingdom == "Eukaryota") {
            db <- "ref_euk_rep_genomes"
        }
        else {
            db <- "ref_prok_rep_genomes"
        }
    }
    db_path <- paste(db_dir, db, sep = "/")
    db_type <- "nucl"
    forward <- query_row$forward_stop
    reverse <- query_row$reverse_stop
    if (forward < reverse) {
        forward <- forward + 1
        reverse <- reverse - 1
    }
    else {
        temp <- forward
        forward <- reverse + 1
        reverse <- temp - 1
    }

    # System call
    if (is.null(ncbi_bin)) {
        fasta <- system2("blastdbcmd", args = c("-db", db_path,
                                                "-dbtype", db_type,
                                                "-entry", accession,
                                                "-range",
                                                paste0(forward, "-", reverse)),
                                                stdout = TRUE)
        return(fasta)
    }
    else {
        blastdbcmd <- paste(ncbi_bin, "blastdbcmd", sep = "/")
        fasta <- system2(blastdbcmd, args = c("-db", db_path,
                                                "-dbtype", db_type,
                                                "-entry", accession,
                                                "-range",
                                                paste0(forward, "-", reverse)),
                                                stdout = TRUE)
        return(fasta)
    }
}