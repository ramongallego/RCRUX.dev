#' Helper function to save the state of a running blast search
#'
#' @param dir the directory to save in
#' @param output_table the outputs generated so far
#' @param unsampled_indices a vector of indices not yet sampled
#' @param too_many_ns a vector of indices that result
#'        in a fasta with too many Ns
#' @param not_in_db a vector of indices not found in the local
#'        database
#' @return NULL
#' @export
save_state <- function(dir, output_table, unsampled_indices, too_many_ns,
                        not_in_db) {
    save_dir <- paste0(dir, "rcrux_blast_data/")
    if (!dir.exists(save_dir)) {
        dir.create(save_dir)
    }
    utils::write.table(output_table, file = paste0(save_dir,
                        "output_table.txt"), row.names = FALSE, sep = ",")
    write(unsampled_indices, file = paste0(save_dir, "unsampled_indices.txt"))
    write(too_many_ns, file = paste0(save_dir, "too_many_Ns.txt"))
    write(not_in_db, file = paste0(save_dir, "not_in_db.txt"))
}