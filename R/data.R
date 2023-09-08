#' Reads a NONMEM sdtab file
#'
#' Currently only one TABLE entry is supported!
#' The function tries to find if the table separator is `,` or whitespace.
#'
#' @param sdtab_file A valid path to the sdtab file.
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'\dontrun{
#' data <- read_sdtab("path/modelstdtab")
#'}
read_sdtab <- function(sdtab_file) {
  checkmate::assert_file_exists(sdtab_file, access = "r")

  ## Check which type of separator we have in our tables
  header_line <- scan(
    file = sdtab_file, nlines = 1, skip = 1,
    what = "character", sep = "\n", quiet = TRUE
  )

  sep <- ""
  if (length(grep(",", header_line)) != 0) {
    sep <- ","
  }

  data <- read.table(sdtab_file,
    sep = sep, skip = 1,
    header = TRUE, na.strings = ".", check.names = FALSE
  )
  data
}
