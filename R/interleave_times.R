#' Helper for reading interleaving prelims and finals times
#'
#' Interleaves times based on row number ranges.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#' @param results a dataframe containing columns for minimum and maximum row number (usually `Row_Min` and `Row_Max`).  Times will be interleaved into this dataframe.
#' @param times a dataframe containing times in column 1 (or other values to be interleaved) and row numbers in column 2 (usually `Row_Numb`).
#' @param type either "individual" or "relay"
#' @return a modified version of `results` with values from `times` interleaved on the basis of row number
#'
#' @examples \dontrun{
#'  }
#' \dontrun{
#'  }
#' @seealso \code{interleave_times} is a helper function used in \code{\link{parse_hy3}}
#'
#'

interleave_times <-
  function(results,
           times,
           type = c("individual", "relay")) {

    # results <- entry
    # times <- finals
    # type <- "individual"

    if (type == "individual") {
      # Adding in prelims times
      i <- cut(times$Row_Numb, t(results[, 6:7]))
      levels(i)[c(FALSE, TRUE)] = NA
      results[[names(times[1])]][i] <- times[[1]]
    } else if (type == "relay") {
      # relays
      i <- cut(times$Row_Numb, t(results[, 5:6]))
      levels(i)[c(FALSE, TRUE)] <- NA
      results[[names(times[1])]][i] <- times[[1]]
    }
    return(results)
  }


### can also be done with data.table
# entry <- data.table::setDT(entry)[prelims, Prelims_Time := Prelims_Time, on = .(Row_Min < Row_Numb, Row_Max > Row_Numb)]
