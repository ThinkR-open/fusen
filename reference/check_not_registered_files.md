# Show in a table files that are not already registered in the yaml config file

If user start their package without 'fusen' or with version \< 0.4, they
need to create the config file, with already existing functions.

## Usage

``` r
check_not_registered_files(
  path = ".",
  config_file,
  guess = TRUE,
  to_csv = TRUE,
  open = FALSE
)
```

## Arguments

- path:

  Path to package to check for not registered files

- config_file:

  Path to the configuration file

- guess:

  Logical. Guess if the file was inflated by a specific flat file

- to_csv:

  Logical. Whether to store along the config file, the outputs in a csv
  for the user to clean it manually

- open:

  Logical. Whether to open the csv of unregistered files.

## Value

Path to csv file if `to_csv` is TRUE.
[`dput()`](https://rdrr.io/r/base/dput.html) of the dataframe otherwise.

## See also

[`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
for automatically registering all files already present in the project,
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
to inflate every flat files according to the configuration file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run this on the current package in development
out_csv <- check_not_registered_files()
file.edit(out_csv)
} # }

# Or you can try on the reproducible example
dummypackage <- tempfile("clean")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]
# Inflate once
usethis::with_project(dummypackage, {
  suppressMessages(
    inflate(
      pkg = dummypackage,
      flat_file = flat_file,
      vignette_name = "Get started",
      check = FALSE,
      open_vignette = FALSE
    )
  )

  # Add a not registered file to the package
  cat("# test R file\n", file = file.path(dummypackage, "R", "to_keep.R"))

  # Use the function to check the list of files
  out_csv <- check_not_registered_files(dummypackage)
  out_csv
  # Read the csv to see what is going on
  content_csv <- read.csv(out_csv, stringsAsFactors = FALSE)
  content_csv
  # Keep it all or delete some files, and then register all remaining
  out_config <- register_all_to_config()
  out_config
  # Open the out_config file to see what's going on
  yaml::read_yaml(out_config)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/clean1b6851112c9c".
#> ── config file for dev/flat_full.Rmd ───────────────────────────────────────────
#> ℹ Some files in your package are not registered in the configuration file stored in dev/config_fusen.yaml: R/to_keep.R
#> fusen uses a configuration file to store the structure of your package and help you clean it when needed.
#> You will find a list of unregistered files there: csv_file that you can open with `file.edit('dev/config_not_registered.csv')`
#> 
#> Delete unregistered files that you do not need anymore. Then run `fusen::register_all_to_config()`.
#> After that, this message should not appear in your next `fusen::check_not_registered_files()` or `fusen::inflate_all()` calls.
#> For more information, read `vignette('register-files-in-config (`vignette(fusen::register-files-in-config)`)', package = 'fusen')`
#> cli-213-514
#> ✔ R: R/to_keep.R was added to the config file
#> ── Delete dev/config_not_registered.csv ────────────────────────────────────────
#> ✔ Setting active project to "<no active project>".
#> $flat_full.Rmd
#> $flat_full.Rmd$path
#> [1] "dev/flat_full.Rmd"
#> 
#> $flat_full.Rmd$state
#> [1] "active"
#> 
#> $flat_full.Rmd$R
#> [1] "R/my_median.R"       "R/my_other_median.R"
#> 
#> $flat_full.Rmd$tests
#> [1] "tests/testthat/test-my_median.R"      
#> [2] "tests/testthat/test-my_other_median.R"
#> 
#> $flat_full.Rmd$vignettes
#> [1] "vignettes/get-started.Rmd"
#> 
#> $flat_full.Rmd$inflate
#> $flat_full.Rmd$inflate$flat_file
#> [1] "dev/flat_full.Rmd"
#> 
#> $flat_full.Rmd$inflate$vignette_name
#> [1] "Get started"
#> 
#> $flat_full.Rmd$inflate$open_vignette
#> [1] FALSE
#> 
#> $flat_full.Rmd$inflate$check
#> [1] FALSE
#> 
#> $flat_full.Rmd$inflate$document
#> [1] TRUE
#> 
#> $flat_full.Rmd$inflate$overwrite
#> [1] "ask"
#> 
#> $flat_full.Rmd$inflate$clean
#> [1] "ask"
#> 
#> $flat_full.Rmd$inflate$codecov
#> [1] FALSE
#> 
#> 
#> 
#> $keep
#> $keep$path
#> [1] "keep"
#> 
#> $keep$state
#> [1] "active"
#> 
#> $keep$R
#> [1] "R/to_keep.R"
#> 
#> $keep$tests
#> list()
#> 
#> $keep$vignettes
#> list()
#> 
#> 
unlink(dummypackage, recursive = TRUE)
```
