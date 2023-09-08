.population_run_folder <- function(run_folder,
                                   mod_file,
                                   data_file = NULL,
                                   data_frame = NULL,
                                   sep = " ",
                                   na_char = ".",
                                   header = TRUE) {
  if (is.null(data_file) && is.null(data_frame)) {
    stop("`data_file` or `data_frame` must be not `NULL`", call. = FALSE)
  }
  checkmate::assert_string(sep)
  checkmate::assert_string(na_char)
  checkmate::assert_flag(header)


  info_mod <- mod_info(mod_file)
  # copy mod file
  dest_modfile <- file.path(run_folder, basename(mod_file))
  ok <- file.copy(mod_file, dest_modfile)
  if (!ok) {
    stop(glue::glue("Could not copy mod file to run directory `{run_folder}`"), call. = FALSE)
  }

  # copy or create data file
  dest_data_file <- info_mod$data_file
  if (is.na(dest_data_file)) {
    if (is.null(data_file)) {
      stop("You have to define the `data_file`. Could not parse `data_file` name from mod file.")
    }
    dest_data_file <- basename(data_file)
  }
  dest_data_file <- file.path(run_folder, dest_data_file)

  if (is.null(data_frame)) {
    checkmate::assert_file_exists(data_file, access = "r")
    ok <- file.copy(data_file, dest_data_file)
    if (!ok) {
      stop(glue::glue("Could not copy data file to run directory `{run_folder}`"), call. = FALSE)
    }
  } else {
    checkmate::assert_data_frame(data_frame)
    tryCatch(.write_data_stream(data_frame, dest_data_file, sep = " ", na_char = ".", header = TRUE),
      error = function(e) {
        stop(glue::glue("Could not create data file in run directory `{run_folder}`"), call. = FALSE)
      }
    )
  }

  sdtab_path <- ifelse(is.na(info_mod$sdtab_file),
    NA_character_, file.path(run_folder, info_mod$sdtab_file)
  )

  result <- list(
    path = run_folder,
    sdtab_file = sdtab_path,
    data_file = dest_data_file,
    mod_file = dest_modfile
  )

  return(result)
}


#' Creates a run folder and copies/creates model file and data stream file
#'
#' This function can be used to create new directories and run new models or existing models
#' with new data streams. The `run_folder` is created and model file and data stream files or
#' data_frames will be copied/created in this folder.
#'
#' Data streams can either be provided as files (`data_file`) or
#' `data.frames` (`data_frame`).
#'
#' The data stream file will be copied or serialized via the following rules:
#' * The file name of the data stream is fetched via [mod_info()].
#' * If [mod_info()] could not parse the file, the base file name of `data_file` will be used.
#' * If `data_file` and not `data.frame` is provided, `data_file` will to copied to the `run_folder`.
#' * If `data_frame` is provided, the data.frame is serialized to the destination.
#'
#' @param mod_file A valid path to an existing NONMEM model file.
#' @param run_folder A valid folder that exists or should be created (recursively) that
#' should act as a NONMEM run folder.
#' @param data_file A path or name of a NONMEM data stream file or `NULL`. See details for more
#' information.
#' @param data_frame A NONMEM data stream data.frame or `NULL`. See details for more
#' information.
#' @param create_folder If `run_folder` should be created (recursively). If `FALSE`, the function
#' assumes that this folder already exists.
#' @param sep If a `data_frame` should de serialized to a NONMEME data stream file: Column separator
#' of the file (defailt: whitepsace).
#' @param na_char If a `data_frame` should de serialized to a NONMEME data stream file: Character
#' for missing values (default: `.`).
#' @param header If a `data_frame` should de serialized to a NONMEME data stream file: `TRUE` if
#' column names should be present in the data stream file, else `FALSE`.
#'
#' @return A list with the following items:
#' * `path` The full path to the run folder
#' * `sdtab_file` The full path to the `sdtab` file (or `NA` if it could not be paraed from the
#' mod file).
#' * `data_file` The full path to the data stream file.
#' * `mod_file`  The full path to the NONMEM model file.
#'
#' @export
#' @seealso [create_rnd_run_folder()], [remove_run_folder()]
#'
#' @examples
#' \dontrun{
#' rf_info <- create_run_folder("mymod.mod", "/users/nmusers/myrunfolder/", data_frame = data)
#' rf_info <- create_run_folder("mymod.mod", "/users/nmusers/myrunfolder/", data_file = "test.csv")
#' }
#'
create_run_folder <- function(mod_file,
                              run_folder,
                              data_file = NULL,
                              data_frame = NULL,
                              create_folder = TRUE,
                              sep = " ",
                              na_char = ".",
                              header = TRUE) {
  checkmate::assert_file_exists(mod_file, access = "r")
  checkmate::assert_flag(create_folder)
  if (!.is_ascii_only(run_folder)) {
    stop("`run_folder` must only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  if (!create_folder) {
    checkmate::assert_directory_exists(run_folder, access = "rw")
  } else {
    fs::dir_create(run_folder, recurse = TRUE)
  }

  .population_run_folder(run_folder, mod_file, data_file, data_frame, sep, na_char, header)
}



#' Creates a random run folder and copies/creates model file and data stream file
#'
#' This function can be used to create new directories and run new models or existing models
#' with new data streams. In contrast to [create_run_folder()], tnis function
#' creates a randomly named folder inside `base_run_folder` to execute models. This function can
#' be useful in APIs or apps that need temporary run folders.
#'
#' Data streams can either be provided as files (`data_file`) or
#' `data.frames` (`data_frame`).
#'
#' The data stream file will be copied or serialized via the following rules:
#' * The file name of the data stream is fetched via [mod_info()].
#' * If [mod_info()] could not parse the file, the base file name of `data_file` will be used.
#' * If `data_file` and not `data.frame` is provided, `data_file` will to copied to the `run_folder`.
#' * If `data_frame` is provided, the data.frame is serialized to the destination.
#'
#' @param mod_file A valid path to an existing NONMEM model file.
#' @param base_run_folder A valid and existing base folder for NONMEM runs. In this folder a
#' random run folder will be created with a folder name of `nchar_folder` characters.
#' @param data_file A path or name of a NONMEM data stream file or `NULL`. See details for more
#' information.
#' @param data_frame A NONMEM data stream data.frame or `NULL`. See details for more
#' information.
#' @param nchar_folder The number of characters for the randomly named folder (must be 3-20).
#' @param sep If a `data_frame` should de serialized to a NONMEME data stream file: Column separator
#' of the file (defailt: whitepsace).
#' @param na_char If a `data_frame` should de serialized to a NONMEME data stream file: Character
#' for missing values (default: `.`).
#' @param header If a `data_frame` should de serialized to a NONMEME data stream file: `TRUE` if
#' column names should be present in the data stream file, else `FALSE`.
#'
#' @return A list with the following items:
#' * `path` The full path to the run folder
#' * `sdtab_file` The full path to the `sdtab` file (or `NA` if it could not be paraed from the
#' mod file).
#' * `data_file` The full path to the data stream file.
#' * `mod_file`  The full path to the NONMEM model file.
#'
#' @export
#' @seealso [create_run_folder()], [remove_run_folder()]
#'
#' @examples
#' \dontrun{
#' rf_info <- create_rnd_run_folder("mymod.mod", "/users/nmusers/myrunfolder/", data_frame = data)
#' rf_info <- create_rnd_run_folder("mymod.mod", "/users/nmusers/myrunfolder/", data_file = "test.csv")
#' }
#'
create_rnd_run_folder <- function(mod_file,
                                  base_run_folder,
                                  data_file = NULL,
                                  data_frame = NULL,
                                  nchar_folder = 10,
                                  sep = " ",
                                  na_char = ".",
                                  header = TRUE) {
  checkmate::assert_file_exists(mod_file, access = "r")
  checkmate::assert_directory_exists(base_run_folder, access = "rw")
  checkmate::assert_integerish(nchar_folder, lower = 3, upper = 20, len = 1, any.missing = FALSE)
  if (!.is_ascii_only(base_run_folder)) {
    stop("`base_run_folder` must only contain ASCII characters (NONMEM limitation).", call. = FALSE)
  }

  wd_folder <- NULL
  for (i in 1:5) {
    wd_folder <- file.path(base_run_folder, .rand_folder_name(nchar_folder))
    if (dir.exists(wd_folder)) {
      wd_folder <- NULL
      next
    }
    if (dir.create(wd_folder, showWarnings = FALSE)) {
      break
    }

    wd_folder <- NULL
  }

  if (is.null(wd_folder)) {
    stop("Could not create run folder", call. = FALSE)
  }

  tryCatch(
    .population_run_folder(
      wd_folder, mod_file,
      data_file, data_frame, sep, na_char, header
    ),
    error = function(e) {
      try(remove_run_folder(wd_folder), silent = TRUE)
      stop(e$message, call. = FALSE)
    }
  )
}


#' Removes a folder, all files in the folder and all its subfolders
#'
#' @param folder Directory that should be removed
#'
#' @return The deleted paths (invisibly).
#'
#' @export
#' @seealso [create_run_folder()], [create_rnd_run_folder()]
#'
#' @examples
#' \dontrun{
#' remove_run_folder("myrunfolder")
#' }
remove_run_folder <- function(folder) {
  checkmate::assert_directory_exists(folder, access = "rw")
  fs::dir_delete(folder)
}
