# Inflate Rmd to package

Inflate Rmd to package

## Usage

``` r
inflate(
  pkg = ".",
  flat_file,
  vignette_name = "Get started",
  open_vignette = TRUE,
  check = TRUE,
  document = TRUE,
  overwrite = "ask",
  clean = "ask",
  update_params = TRUE,
  codecov = FALSE,
  ...
)
```

## Arguments

- pkg:

  Path to package

- flat_file:

  Path to Rmarkdown file to inflate

- vignette_name:

  Character. Title of the resulting vignette. Use `NA` if you do not
  want to create a vignette.

- open_vignette:

  Logical. Whether to open vignette file at the end of the process

- check:

  Logical. Whether to check package after Rmd inflating

- document:

  Logical. Whether to document your package using
  [`att_amend_desc`](https://thinkr-open.github.io/attachment/reference/att_amend_desc.html)

- overwrite:

  Logical (TRUE, FALSE) or character ("ask", "yes", "no). Whether to
  overwrite vignette and functions if already exists.

- clean:

  Logical (TRUE, FALSE) or character ("ask", "yes", "no) Whether to
  delete files that are not anymore created by the current flat file.
  Typically, if you have deleted or renamed a function in the flat file.
  Default to "ask".

- update_params:

  Logical. Whether to update the inflate parameters in the configuration
  file.

- codecov:

  Logical. Whether to compute code coverage with
  [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md).

- ...:

  Arguments passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
  For example, you can do `inflate(check = TRUE, quiet = TRUE)`, where
  `quiet` is passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

## Value

Package structure. Return path to current package.

## See also

[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
to inflate every flat files according to the configuration file.

## Examples

``` r
# Create a new project
dummypackage <- tempfile("dummy.package")
dir.create(dummypackage)

# {fusen} steps
dev_file <- add_flat_template(template = "full", pkg = dummypackage, overwrite = TRUE)
#> Created file .here in /tmp/Rtmp005QYy/dummy.package1b688202d64 . Please start a new R session in the new project directory.
flat_file <- dev_file[grepl("flat", dev_file)]
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
inflate(
  pkg = dummypackage,
  flat_file = flat_file,
  vignette_name = "Exploration of my Data",
  check = FALSE
)
#> ✔ Setting active project to "/tmp/Rtmp005QYy/dummy.package1b688202d64".
#> ℹ Loading dummy.package1b688202d64
#> Writing NAMESPACE
#> ✔ Adding knitr to Suggests field in DESCRIPTION.
#> ☐ Use `requireNamespace("knitr", quietly = TRUE)` to test if knitr is
#>   installed.
#> ☐ Then directly refer to functions with `knitr::fun()`.
#> ✔ Adding "inst/doc" to .gitignore.
#> ── config file for dev/flat_full.Rmd ───────────────────────────────────────────
#> ✔ R: R/my_median.R was added to the config file
#> ✔ R: R/my_other_median.R was added to the config file
#> ✔ tests: tests/testthat/test-my_median.R was added to the config file
#> ✔ tests: tests/testthat/test-my_other_median.R was added to the config file
#> ✔ vignettes: vignettes/exploration-of-my-data.Rmd was added to the config file
#> Saving attachment parameters to yaml config file
#> Updating dummy.package1b688202d64 documentation
#> ℹ Loading dummy.package1b688202d64
#> Writing NAMESPACE
#> Writing my_median.Rd
#> Writing my_other_median.Rd
#> Writing sub_median.Rd
#> ℹ Loading dummy.package1b688202d64
#> [+] 3 package(s) added: stats, rmarkdown, testthat.

# Explore directory of the package
# browseURL(dummypackage)

# Try pkgdown build
# usethis::use_pkgdown()
# pkgdown::build_site(dummypackage)
# Delete dummy package
unlink(dummypackage, recursive = TRUE)
```
