#' Advances split names by one split_length
#'
#' Used to adjust names of splits inside \code{swim_parse_omega} to account for
#' 50 split not being correctly captured
#'
#' @importFrom stringr str_extract
#'
#' @param x a string to rename, from columns output by \code{splits_parse}
#' @param split_len distance for each split
#' @return returns string iterated up by split_length
#'
#' @seealso \code{splits_rename_omega} runs inside \code{\link{swim_parse_omega}} on the
#'   output of \code{\link{splits_parse}}

splits_rename_omega <- function(x, split_len = split_length_omega){

    x <- x %>%
    stringr::str_extract("\\d{2,4}") %>%
      as.numeric() + split_len

    x %>%
      paste0("Split_", .)

}

