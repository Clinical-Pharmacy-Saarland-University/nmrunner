

create_run_folder <- function(mod_file, run_base, data_file = NULL, data_frame = NULL) {

  wd_folder <- NULL
  for (i in 1:10) {
    wd_folder <- file.path(run_dir, rand_folder_name())
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


  source_mod_file <- file.path(base_dir, mod_file)
  source_data_file <- file.path(base_dir, data_file)

  sink_mod_file <- file.path(wd_folder, mod_file)
  sink_data_file <- file.path(wd_folder, data_file)

  ok <- file.copy(
    c(source_mod_file, source_data_file),
    c(sink_mod_file, sink_data_file)
  )

  if (!all(ok)) {
    stop("Could not copy files to run folder", call. = FALSE)
    try(dir_delete(wd_folder))
  }

  return(wd_folder)
}
