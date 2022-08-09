# Hate the name
# The idea here is to return a vector of fasta-strings
# From blastdbcmd
generate_fastas <- function(X, database) {
    dbcmd <- function(accession, forward, reverse) {
        # So we're gonna call blastdbcmd much like we do in run_blastdbcmd
    }
    apply(X, 1, dbcmd)
}