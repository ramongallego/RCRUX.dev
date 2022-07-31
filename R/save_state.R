#' Helper function to save the state of a running blast search
#'
#' @param dir the directory to save in
#' @param output_table the outputs generated so far
#' @param unsampled_indices a vector of indices not yet sampled
#' @param too_many_ns a vector of indices that result
#'        in a fasta with too many Ns
#' @param not_in_db a vector of indices not found in the local
#'        database
#' @param num_rounds the number of rounds so far
#' @return NULL
#' @export
save_state <- function(save_dir, output_table, unsampled_indices, too_many_ns,
                        not_in_db, num_rounds) {
    if (!dir.exists(save_dir)) {
        dir.create(save_dir)
    }
    write.table(output_table, file = paste(save_dir,
                        "output_table.txt", sep = "/"), row.names = FALSE, sep = ",")
    write(unsampled_indices, file = paste(save_dir, "unsampled_indices.txt", sep = "/"), sep = "/")
    write(too_many_ns, file = paste(save_dir, "too_many_ns.txt", sep = "/"), sep = "/")
    write(not_in_db, file = paste(save_dir, "not_in_db.txt", sep = "/"))
    write(not_in_db, file = paste(save_dir, "num_rounds.txt", sep = "/"))
}