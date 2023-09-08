<!-- START_BADGES -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active/) [![Package version](https://img.shields.io/badge/GitHub-0.0.0.9000-orange.svg)](https://github.com/Clinical-Pharmacy-Saarland-University/nmrunner/) [![minimal R version](https://img.shields.io/badge/R%3E%3D-4.1.0-blue.svg)](https://cran.r-project.org/)
---
<!-- END_BADGES -->

# nmrunner: Start NONMEM runs via R
`nmrunner` facilitates the initiation of NONMEM runs directly from `R` and subsequently retrieves the results. Designed for compatibility with both `Windows` and `Linux`, it offers a suite of auxiliary functions tailored for integration into `APIs` or interactive platforms like `Shiny` apps. Emphasizing simplicity, the package is intentionally minimalistic.

## Status
The package is currently **under development** and API and functionality is **not stable**.

## Install
```r
devtools::install_github("Clinical-Pharmacy-Saarland-University/nmrunner")
```
You need an active and valid `NONMEM` licence installed on your machine.

## Usage

### Run a NONMEM model in a folder
```r
library(nmrunner)

run_info <- nm_run("path/my_nm_model.mod", "/opt/nonmem/nm751/run/nmfe75", timeout = 360)

cat("Runtime [sec]:", run_info$exec_time)
sim_tab <- read_sdtab(run_info$sdtab_file)
head(sim_tab)
```

### Parse sdtab and data stream file names from mod files
```r
library(nmrunner)

infos <- mod_info("path/my_nm_model.mod")
print(infos$data_file)
print(infos$sdtab_file)

```

### Create a run folder and execute a model
```r
library(nmrunner)

nm_starter <- "C:/nm74/run/nmfe74.bat"
mod_file <- "C:/Temp/NM_R/0935pro_supp_org.mod"
data_file <- "C:/Temp/NM_R/Test.csv"
run_folder <- "C:/Temp/NM_TEST"

folder_info <- create_run_folder(mod_file, run_folder, data_file = data_file)
run_info <- nm_run(folder_info$mod_file, nm_starter)

print(paste("Run ok: ", run_info$success))
print(paste("Runtime [sec]: ", run_info$exec_time))
res_df <- read_sdtab(run_info$sdtab_file)
```

### Use in API/App with a randomly created run folders async
```r
library(nmrunner)
library(purrr)
library(dplyr)
library(future)

nm_starter <- "C:/nm74/run/nmfe74.bat"
mod_file <- "C:/Temp/NM_R/0935pro_supp_org.mod"
data_file <- "C:/Temp/NM_R/Test.csv"
run_base <- "C:/Temp/NM_TEST"

# just use some data for demo
df_1 <- read.csv(data_file)
df_2 <- read.csv(data_file) |> mutate(DV = DV * 1.2)

# run a model with different data streams provided via data.frames
# this creates randomly named folders for execution and deletes them after reading the
# sdtab file 
run_model <- function(data_frame) {
  run_folder <- create_rnd_run_folder(mod_file, run_base, data_frame = data_frame)
  on.exit({
    # if nm_run times out, try 10 times to remove the the folder
    # removing can fail if NONMEM cannot be terminated and the folder resource is locked
    try_remove <- insistently(remove_run_folder, rate_delay(pause = 0.5, max_times = 10))
    try_remove(run_folder$path)
  })
  
  # use a timeout of 360 seconds for a run
  run_info <- nm_run(run_folder$mod_file, nm_starter, timeout = 360)
  if (!run_info$success) {
    stop("Run failed", call. = FALSE)
  }
  
  print(paste("Runtime [sec]: ", run_info$exec_time))
  read_sdtab(run_info$sdtab_file)
}

# 2 runs async - do not forget to seed (this is necessary due to random gen of folders)
plan(multisession, workers = 3)
fut_1 <- future(run_model(df_1), seed = TRUE)
fut_2 <- future(run_model(df_2), seed = TRUE)

df_1 <- value(fut_1)
df_2 <- value(fut_2)
```