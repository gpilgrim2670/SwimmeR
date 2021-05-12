#' Helper for reading interleaving prelims and finals results
#'
#' Interleaves times or places based on row number ranges.
#'
#' @param entries a data frame containing columns for minimum and maximum row
#'   number (usually `Row_Min` and `Row_Max`).  Times or places will be
#'   interleaved into this data frame.
#' @param results a data frame containing times (or places) in column 1 (or
#'   other values to be interleaved) and row numbers in column 2 (usually
#'   `Row_Numb`).
#' @param type either "individual" or "relay"
#' @return a modified version of `entries` with values from `results`
#'   interleaved on the basis of row number
#'
#' @seealso \code{interleave_results} is a helper function used in
#'   \code{\link{hy3_parse}}

interleave_results <-
  function(entries,
           results,
           type = c("individual", "relay")) {

    if (type == "individual") {
      # Adding in results
      i <- cut(results$Row_Numb, t(entries[, 6:7]))
      levels(i)[c(FALSE, TRUE)] <- NA
      entries[[names(results[1])]][i] <- results[[1]]
    } else if (type == "relay") {
      # relays
      i <- cut(results$Row_Numb, t(entries[, 5:6]))
      levels(i)[c(FALSE, TRUE)] <- NA
      entries[[names(results[1])]][i] <- results[[1]]
    }
    return(entries)
  }
