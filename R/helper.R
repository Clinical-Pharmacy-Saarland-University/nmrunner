
.is_ascii_only <- function(path_string) {
  return(grepl("^[\x01-\x7F]+$", path_string))
}

.rand_folder_name <- function(length = 10) {
  random_string <- paste0(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = "")
  return(random_string)
}
