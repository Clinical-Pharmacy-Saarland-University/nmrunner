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
#' @return A list with the following items:
#' * `success` `TRUE` or `FALSE` that indicate if the execution was sucessful (this does not
#' indicate that the run was successful).
#' * `exec_time` The execution time in seconds.
#' * `mod_file` The full path to the NONMEM model file that was executed.
#' * `sdtab_file` The full path to the NONMEM output `sdtab` file (or `NA` if it could not be parsed
#' from the `mod` file).
#' * `data_file` The full path to the NONMEM data stream file (or `NA` if it could not be parsed
#' from the `mod` file).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_ok <- nm_run("path/my_nm_model.mod", "/opt/nonmem/nm751/run/nmfe75")
#' }
nm_run <- function(mod_file,
                   nm_starter,
                   out_file_name = "nm_output",
                   timeout = 0,
                   debug = FALSE,
                   check_data_file = TRUE) {
  start_time <- proc.time()["elapsed"]

  checkmate::assert_file_exists(mod_file, access = "r")
  checkmate::assert_file_exists(nm_starter, access = "x")
  checkmate::assert_string(out_file_name, min.chars = 1, max.chars = 20)
  checkmate::assert_integerish(timeout, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_flag(debug)
  checkmate::assert_flag(check_data_file)

  if (!.is_ascii_only(out_file_name)) {
    stop("`out_file_name` must only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  if (!.is_ascii_only(mod_file)) {
    stop("`mod_file_path` can only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  run_folder <- dirname(mod_file)
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

  result <- list(
    success = FALSE,
    exec_time = NA,
    mod_file = mod_file,
    sdtab_file = file.path(run_folder, infos$sdtab_file),
    data_file = file.path(run_folder, infos$data_file)
  )

  run_folder_norm <- run_folder |>
    normalizePath()
  mod_file <- basename(mod_file)
  rundir_arg <- glue::glue("-rundir=\"{run_folder_norm}\"")
  call <- glue::glue("{nm_starter} {mod_file} {out_file_name} {rundir_arg}")
  sys_res <- system(call, ignore.stdout = !debug, timeout = timeout) |> suppressWarnings()

  # remove this anoying trash files if possible
  trash_files <- c('condor.set', 'condorarguments.set', 'condoropenmpiscript.set', 'trash.tmp')
  try(file.remove(trash_files) |> suppressWarnings(), silent = TRUE)

  result$success <- sys_res == 0
  end_time <- proc.time()["elapsed"]
  result$exec_time <- end_time - start_time

  return(result)
}
