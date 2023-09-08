

#' Runs a NONMEM session via system call
#'
#' The NONMEM run will be executed within the folder of the model file.
#' The `nm_starter` is either a `bat` file on windows (e.g. `nm74/run/nmfe74.bat`) or a shell
#' script on linux (e.g. `nm751/run/nmfe75`).
#'
#' @param mod_file A valid path to the NONMEM model file.
#' @param nm_starter The script ot the NONMEM executable. See details for more infos.
#' @param out_file_name A name for the NONMEM output file.
#' @param timeout Timeout in seconds. If 0, no timeout is set.
#' @param debug If `TRUE`, `stdout` output will be displayed.
#' @param check_data_file If `TRUE` the data stream file name will be parsed from the model file
#' and it is checked if this files exists in the folder of the model.
#'
#' @return `TRUE` on success and `FALSE` on a failed execution.
#' @export
#'
#' @examples
#'\dontrun{
#' run_ok <- nm_run("path/my_nm_model.mod", "/opt/nonmem/nm751/run/nmfe75")
#'}
nm_run <- function(mod_file,
                   nm_starter,
                   out_file_name = "nm_output",
                   timeout = 0,
                   debug = FALSE,
                   check_data_file = TRUE) {
  checkmate::assert_file_exists(mod_file, access = "r")
  checkmate::assert_file_exists(nm_starter, access = "x")
  checkmate::assert_string(out_file_name, min.chars = 1, max.chars = 20)
  if (!.is_ascii_only(out_file_name)) {
    stop("`out_file_name` must only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  if (!.is_ascii_only(mod_file_path)) {
    stop("`mod_file_path` can only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  run_folder <- dirname(mod_file_path)
  infos <- mod_info(mod_file)
  if (check_data_file) {
    if (is.na(infos$data_file)) {
      warning("Could not parse data stream filename from model file", call. = FALSE)
    } else {
      if (!file.exists(file.path(run_folder, infos$data_file))) {
        stop(glue::glue("Data stream file `{infos$data_file}` does not exist in folder `{run_folder}`."),
          call. = FALSE
        )
      }
    }
  }

  run_folder_norm <- run_folder |>
    normalizePath()
  mod_file <- basename(mod_file_path)
  rundir_arg <- glue::glue("-rundir=\"{run_folder_norm}\"")

  call <- glue::glue("{nm_starter} {mod_file} {out_file_name} {rundir_arg}")
  res <- system(call, ignore.stdout = !debug, timeout = timeout)
  return(res == 0)
}

