#' Reads information from a NONMEM model file
#'
#' @param mod_file Valid path to a NONMEM model file. The file must be readable.
#'
#' The function searches for `$DATA` and `$TABLE` entries in the model file.
#'
#' @return A list with the following fields:
#'  * `mod_file` the name of the NONMEM model file (full path)
#'  * `data_file` the name of the data stream input file or `NA` if not found.
#'  * `sdtab_file` the name of the output `sdtab` file or `NA` if not found.
#' @export
#'
#' @examples
#' \dontrun{
#' infos <- mod_info("path/my_nm_model.mod")
#' }
mod_info <- function(mod_file) {
  checkmate::assert_file_exists(mod_file, access = "r")
  lines <- readr::read_lines(mod_file, progress = FALSE)

  result <- list(
    mod_file = mod_file,
    data_file = NA_character_,
    sdtab_file = NA_character_
  )

  # data file
  data_line <- lines[which(grepl("$DATA", lines, fixed = TRUE))]
  if (length(data_line) >= 1) {
    data_line <- data_line[1]
    result$data_file <- stringr::str_extract(data_line, "(?<=\\$DATA\\s)[^\\s]+")
  }

  # sdtab
  table_lines <- lines[which(grepl("$TABLE", lines, fixed = TRUE))]
  if (length(table_lines) >= 1) {
    result$sdtab_file <- stringr::str_extract(table_lines, "(?<=FILE=)[^\\s]+")
  }

  result
}
