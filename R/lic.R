`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a


#' Parse NONMEM License Information
#'
#' Runs a short NONMEM test via [test_nm()] and parses license-related
#' information from the NONMEM console output. Returns the registered user,
#' expiration/current dates, whether the license is expired, and the days
#' remaining (if available).
#'
#' The `nm_starter` is either a `bat` file on Windows (e.g. `nm74/run/nmfe74.bat`)
#' or a shell script on Linux (e.g. `nm751/run/nmfe75`). The NONMEM run is
#' executed only to capture console output that carries the license banner.
#'
#' @param nm_starter The script of the NONMEM executable. See details above.
#'
#' @return A list with components:
#' * `registered_to` (`character(1)`): Name/email the license is registered to, or `NA`.
#' * `expiration_date` (`character(1)`): License expiration date as printed by NONMEM, or `NA`.
#' * `current_date` (`character(1)`): Current date as printed by NONMEM, or `NA`.
#' * `expired` (`logical(1)`): `TRUE` if NONMEM reports the license expired, otherwise `FALSE`.
#' * `days_left` (`integer(1)`): Days until expiration if reported by NONMEM, otherwise `NA_integer_`.
#' * `error` (`character(1)`): Error message if the check failed; otherwise `NA`.
#'
#' @examples
#' \dontrun{
#' nm_lic_info("/opt/nonmem/nm751/run/nmfe75")
#' }
#'
#' @export
nm_lic_info <- function(nm_starter) {
  checkmate::assert_string(nm_starter, min.chars = 1)

  make_result <- function(
    registered_to = NA_character_,
    expiration_date = NA_character_,
    current_date = NA_character_,
    expired = TRUE,
    days_left = NA_integer_,
    error = NA_character_
  ) {
    out <- list(
      registered_to = registered_to,
      expiration_date = expiration_date,
      current_date = current_date,
      expired = expired,
      days_left = days_left,
      error = error
    )
    out
  }

  res <- test_nm(nm_starter)
  if (!isTRUE(res$success)) {
    return(make_result(error = res$msg))
  }

  msg <- res$msg %||% ""
  if (grepl("ERROR reading license file", msg, fixed = TRUE)) {
    return(make_result(error = "Undefined error reading license file"))
  }

  lines <- unlist(strsplit(msg, "\\r?\\n", useBytes = TRUE))
  trim <- function(z) sub("^\\s+|\\s+$", "", z, perl = TRUE)

  get_after_colon <- function(prefix) {
    i <- grep(paste0("^", prefix), lines, perl = TRUE)
    if (length(i) == 0) {
      return(NA_character_)
    }
    val <- sub(paste0("^", prefix, "\\s*:?\\s*"), "", lines[i[1]], perl = TRUE)
    trim(val)
  }

  registered_to <- get_after_colon("License Registered to:")
  expiration_date <- get_after_colon("Expiration Date:")
  current_date <- get_after_colon("Current Date:")

  expired <- any(grepl("NONMEM LICENSE HAS EXPIRED", lines, fixed = TRUE))

  days_left <- {
    i <- grep("^Days until program expires", lines, perl = TRUE)
    if (length(i) == 0) {
      NA_integer_
    } else {
      num <- sub(".*?([0-9]+).*", "\\1", lines[i[1]], perl = TRUE)
      if (grepl("^[0-9]+$", num)) as.integer(num) else NA_integer_
    }
  }

  make_result(
    registered_to = if (nzchar(registered_to)) registered_to else NA_character_,
    expiration_date = if (nzchar(expiration_date)) {
      expiration_date
    } else {
      NA_character_
    },
    current_date = if (nzchar(current_date)) current_date else NA_character_,
    expired = isTRUE(expired),
    days_left = days_left
  )
}
