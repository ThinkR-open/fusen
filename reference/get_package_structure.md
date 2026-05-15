# Get structure and information of a 'fusen' built package for developers

Get structure and information of a 'fusen' built package for developers

Draw a tree of the package structure in the console

## Usage

``` r
get_package_structure(config_file, emoji = TRUE, silent = FALSE)

draw_package_structure(structure_list, silent = FALSE)
```

## Arguments

- config_file:

  Path to a source configuration file to get the structure from

- emoji:

  Add emojis to the output

- silent:

  Whether to print messages

- structure_list:

  A list of information about the package as issued from
  `[get_package_structure()]`

## Value

A list of information about the package

## Examples

``` r
if (FALSE) { # \dontrun{
# This only works inside a 'fusen' built package
pkg_structure <- get_package_structure()
draw_package_structure(pkg_structure)
} # }

# Example with a dummy package
dummypackage <- tempfile("drawpkg.structure")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(
  add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # Add an extra R file with internal function
  # to list in "keep"
  dir.create("R")
  cat("extra_fun <- function() {1}\n", file = "R/my_extra_fun.R")

  # Works with classical package
  pkg_structure <- get_package_structure()
  draw_package_structure(pkg_structure)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/drawpkg.structure1b686edbfed2".
#> ── No NAMESPACE file found there:  /tmp/Rtmp005QYy/drawpkg.structure1b686edbfed2
#> ── keep ────────────────────────────────────────────────────────────────────────
#> 
#> - keep
#>   - path
#>       + keep
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/my_extra_fun.R
#>       + 🙈 extra_fun
#>   - tests
#>   - vignettes
#> ✔ Setting active project to "<no active project>".

usethis::with_project(dummypackage, {
  # Works with 'fusen' package
  suppressMessages(
    inflate(
      pkg = dummypackage,
      flat_file = flat_file,
      vignette_name = "Get started",
      check = FALSE,
      open_vignette = FALSE
    )
  )

  pkg_structure <- get_package_structure()
  draw_package_structure(pkg_structure)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/drawpkg.structure1b686edbfed2".
#> ── config file for dev/flat_full.Rmd ───────────────────────────────────────────
#> ── Reading NAMESPACE file ──────────────────────────────────────────────────────
#> ── flat_full.Rmd ───────────────────────────────────────────────────────────────
#> ── keep ────────────────────────────────────────────────────────────────────────
#> 
#> - flat_full.Rmd
#>   - flat_title
#>       + flat_full.Rmd for working package
#>   - path
#>       + dev/flat_full.Rmd
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/my_median.R
#>       + 👀 my_median
#>     - R/my_other_median.R
#>       + 👀 my_other_median
#>       + 🙈 sub_median
#>   - tests
#>       + tests/testthat/test-my_median.R
#>       + tests/testthat/test-my_other_median.R
#>   - vignettes
#>       + vignettes/get-started.Rmd
#> - keep
#>   - path
#>       + keep
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/my_extra_fun.R
#>       + 🙈 extra_fun
#>   - tests
#>   - vignettes
#> ✔ Setting active project to "<no active project>".
```
