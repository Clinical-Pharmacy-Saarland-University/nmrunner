<!-- START_BADGES -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active/) [![Package version](https://img.shields.io/badge/GitHub-0.0.0.9000-orange.svg)](https://github.com/Clinical-Pharmacy-Saarland-University/nmrunner/) [![minimal R version](https://img.shields.io/badge/R%3E%3D-4.1.0-blue.svg)](https://cran.r-project.org/)
---
<!-- END_BADGES -->

# nmrunner: Start NONMEM runs via R
`nmrunner` facilitates the initiation of NONMEM runs directly from `R` and subsequently retrieves the results. Designed for compatibility with both `Windows` and `Linux`, it offers a suite of auxiliary functions tailored for integration into `APIs` or interactive platforms like `Shiny` apps. Emphasizing simplicity, the package is intentionally minimalistic.

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

cat("Runtime [sec]:", run_info$exec_time})
sim_tab <- read_sdtab(run_info$sdtab_file)
head(sim_tab)
```

### Parse sdtab and data stream file names
```r
library(nmrunner)