.get_mod_file_path <- function() {
  system.file("extdata", "test.mod", package = "nmrunner")
}


#' Test NONMEM Execution Capability
#'
#' This function tests the system's ability to execute NONMEM models by attempting to
#' start a NONMEM run via an ingternal test model. It checks
#' file paths, prepares a test environment, and captures any execution errors. The
#' function is useful for validating the setup of NONMEM in an R environment works.
#'
#' The NONMEM run will be executed within the folder of the model file.
#' The `nm_starter` is either a `bat` file on windows (e.g. `nm74/run/nmfe74.bat`) or a shell
#' script on linux (e.g. `nm751/run/nmfe75`).
#'
#' @param nm_starter The script of the NONMEM executable. See details for more infos.
#' @param @param debug If `TRUE`, `stdout` output will be displayed. The output will be captured and
#' print via `cat`.
#'
#' @return A list containing the following components:
#'         - `success`: Logical, TRUE if NONMEM run was successful, FALSE otherwise.
#'         - `msg`: Character string, contains NONMEM console output from the test run or error information.
#'
#' @examples
#' \dontrun{
#' run_infos <- test_nm("/opt/nonmem/nm751/run/nmfe75")
#' }
#'
#' @export
test_nm <- function(nm_starter, debug = FALSE) {
  checkmate::assert_string(nm_starter, min.chars = 1)
  checkmate::assert_flag(debug)

  if (!file.exists(nm_starter)) {
    return(list(success = FALSE, msg = paste("Could not find NONMEM starter file:", nm_starter)))
  }

  test_mod_path <- .get_mod_file_path()
  if (!file.exists(test_mod_path)) {
    return(list(success = FALSE, msg = paste("Model file does not exist at path:", test_mod_path)))
  }

  test_df <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    AMT = c(1, NA, 1, NA),
    DV = c(1, 0.5, 2, 1)
  )

  run_folder_info <- try(create_rnd_run_folder(test_mod_path, "./", data_frame = test_df),
    silent = TRUE
  )

  if (inherits(run_folder_info, "try-error")) {
    return(list(
      success = FALSE,
      msg = paste(
        "Could not create test run folder with error:",
        attr(run_folder_info, "condition")$message
      )
    ))
  }

  on.exit(
    {
      try_remove <- purrr::insistently(remove_run_folder,
                                       purrr::rate_delay(pause = 0.5, max_times = 10))
      try_remove(run_folder_info$path)
    }
  )

  results <- nm_run(run_folder_info$mod_file, nm_starter, debug = debug)
  return(list(success = results$success, msg = results$console_output))
}
