#' Take a datatable and return the results of BLASTing it
#'
#' Given a datatable with the column names of the datatable returned by
#' RCRUX.dev::get_blast_seeds(), use blastdbcmd to convert entries into
#' fasta files, then uses blastn to query ncbi databases for those
#' sequences. It compiles the results of blastn into a data.frame that it
#' returns.
#' Additionally, it saves its state as text files in a specified directory with
#' each iteration.
#'
#' @param blast_seeds a data.frame formatted like the output from
#'        get_blast_seeds_multi_taxa_or_db
#' @param save_dir a directory in which to create text files representing the
#'        current state
#' @param db_dir a directory with a blast-formatted database
#' @param accession_taxa_path a path to an sql created by
#'        taxonomizr::prepareDatabase()
#' @param sample_size the number of entries to accumulate into a fasta before
#'        calling blastn
#' @param wildcards a character vector representing the number of wildcards to
#'        discard
#' @return A data.frame representing the output of blastn
#' @export
blast_datatable <- function(blast_seeds, save_dir, db_dir, accession_taxa_path,
                            sample_size = 1000, wildcards = "NNNN") {
    output_table <- NULL

    # Default values for tracker variables
    num_rounds <- 0
    too_many_ns <- c()
    not_in_db <- c()
    unsampled_indices <- c(seq_len(nrow(blast_seeds)))

    # Pick up where it left off
    # This needs to add rcrux_blast_data onto the path
    if (file.exists(paste(save_dir, "unsampled_indices.txt", sep = "/"))) {
        rounds_path <- paste(save_dir, "num_rounds.txt", sep = "/")
        num_rounds <- as.numeric(readLines(con = rounds_path))

        ns_path <- paste(save_dir, "too_many_Ns.txt", sep = "/")
        too_many_ns <- as.numeric(readLines(con = ns_path))

        not_in_db_path <- paste(save_dir, "not_in_db.txt", sep = "/")
        not_in_db <- as.numeric(readLines(con = not_in_db_path))
    }

    while (length(unsampled_indices) > 0) {
        # sample some of them, removing them from the vector
        sample_indices <- smart_sample(unsampled_indices, sample_size)
        unsampled_indices <-
            unsampled_indices[!(unsampled_indices %in% sample_indices)]

        # run blastdbcmd on each
        # sort results into appropriate buckets
        aggregate_fasta <- NULL
        for (index in sample_indices) {
            fasta <- run_blastdbcmd(blast_seeds[index, ], db_dir)
            # Maybe in these cases we can just append directly to output?

            # So this is somewhat atrocious. Why do we do it this way?
            # Well, in cases where the command has a non-0 exit status,
            # system2 sometimes (always?) returns a character vector of length 0
            # This causes an error because there are no characters to check, so
            # the if has nothing to operate on. This kludgey `or` fixes that.
            if (length(fasta) == 0 || nchar(fasta) == 0) {
                not_in_db <- append(not_in_db, index)
            }
            else if (length(grep(wildcards, fasta)) > 0) {
                too_many_ns <- append(too_many_ns, index)
            }
            else {
                aggregate_fasta <- append(aggregate_fasta, fasta)
            }
        }

        # run blastn and aggregate results
        # This is a temporary fix for the fact that blasting things in multiple
        # dbs at once is nonsensical
        kludge <- paste(db_dir, "nt", sep = "/")
        blastn_output <- run_blastn(aggregate_fasta, kludge)
        if (is.null(output_table)) {
            output_table <- blastn_output
        }
        else {
            output_table <- tibble::add_row(output_table, blastn_output)
        }

        # save the state of the blast
        num_rounds <- num_rounds + 1
        save_state(save_dir, output_table, unsampled_indices, too_many_ns,
            not_in_db, num_rounds)
    }

    # If we get a taxid from blastn can we just use that?
    output_table_taxonomy <-
        get_taxonomizr_from_accession(output_table, accession_taxa_path)
    return(output_table_taxonomy)
}