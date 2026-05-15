# Include all existing package files in the config file

Helps transition from non-fusen packages or made with earlier version

## Usage

``` r
register_all_to_config(pkg = ".", config_file)
```

## Arguments

- pkg:

  Path to the package from which to add file to configuration file

- config_file:

  Path to the configuration file

## Value

Invisible path to 'fusen' configuration file

## See also

[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
for the list of files not already associated with a flat file in the
config file,

## Examples

``` r
if (FALSE) { # \dontrun{
# Usually run this one inside the current project
# Note: running this will write "dev/config_fusen.yaml" in your working directory
register_all_to_config()
} # }

# Or you can try on the reproducible example
dummypackage <- tempfile("register")
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
  out_path <- register_all_to_config(dummypackage)

  # Look at the output
  yaml::read_yaml(out_path)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/register1b687a265974".
#> ── config file for dev/flat_full.Rmd ───────────────────────────────────────────
#> ✔ There are no unregistered files.
#> There is no file to register or everything was already registered
#> ✔ Setting active project to "/tmp/Rtmp005QYy/dummy.package1b688202d64".
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
```
