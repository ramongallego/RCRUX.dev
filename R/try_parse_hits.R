#' Function to attempt parse_primer_hits and fail less loudly
#'
#' Equivalent to parse_primer_hits() if the argument is a legal
#' argument. If the argument is illegal, returns FALSE, which
#' the program can use to respond appropriately.
#' 
#' @param response A response object to parse
#' @return If parse_primer_hits() doesn't throw an error, the results of
#'         parse_primer_hits(). If it does throw an error, FALSE
#' @export

# I'm not sure I trust the old documentation or that this function is necessary
try_parse_hits <- function(response) {
  tryCatch(parse_primer_hits(response),
           error = function(e) {
             return(e)
           },
           finally = {})
}