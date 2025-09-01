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
    days_left = NA_character_,
    error = NA_character_
  ) {
    list(
      registered_to = registered_to,
      expiration_date = expiration_date,
      current_date = current_date,
      expired = expired,
      days_left = days_left,
      error = error
    )
  }

  res <- test_nm(nm_starter)
  if (!isTRUE(res$success)) {
    return(make_result(error = res$msg))
  }

  msg <- res$msg

  if (grepl("ERROR reading license file", msg, fixed = TRUE)) {
    return(make_result(error = "Undefined error reading licence file"))
  }

  get_or_na <- function(pattern, x) {
    if (grepl(pattern, x, perl = TRUE)) {
      trimws(sub(pattern, "\\1", x, perl = TRUE))
    } else {
      NA_character_
    }
  }

  registered_to <- get_or_na(".*License Registered to:\\s*([^\\r\\n]+).*", msg)
  expiration_date <- get_or_na(".*Expiration Date:\\s*([0-9A-Z ]+).*", msg)
  current_date <- get_or_na(".*Current Date:\\s*([0-9A-Z ]+).*", msg)

  expired <- grepl("NONMEM LICENSE HAS EXPIRED", msg, fixed = TRUE)

  days_left <- NA_integer_
  if (!expired && grepl("Days until program expires", msg, fixed = TRUE)) {
    if (
      grepl(".*Days until program expires\\s*:\\s*([0-9]+).*", msg, perl = TRUE)
    ) {
      days_left <- as.integer(sub(
        ".*Days until program expires\\s*:\\s*([0-9]+).*",
        "\\1",
        msg,
        perl = TRUE
      ))
    }
  }

  make_result(
    registered_to = registered_to,
    expiration_date = expiration_date,
    current_date = current_date,
    expired = expired,
    days_left = days_left
  )
}
