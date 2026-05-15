# Inflate all your flat files

Inflate all the flat files stored in "dev/" and starting with "flat\_"

## Usage

``` r
inflate_all(
  pkg = ".",
  document = TRUE,
  check = TRUE,
  open_vignette = FALSE,
  overwrite = TRUE,
  check_unregistered = TRUE,
  codecov = FALSE,
  stylers,
  ...
)

inflate_all_no_check(
  pkg = ".",
  document = TRUE,
  open_vignette = FALSE,
  overwrite = TRUE,
  check_unregistered = TRUE,
  codecov = FALSE,
  stylers,
  ...
)
```

## Arguments

- pkg:

  Path to package

- document:

  Logical. Whether to document your package using
  [`att_amend_desc`](https://thinkr-open.github.io/attachment/reference/att_amend_desc.html)

- check:

  Logical. Whether to check package after Rmd inflating

- open_vignette:

  Logical. Whether to open vignette file at the end of the process

- overwrite:

  Logical (TRUE, FALSE) or character ("ask", "yes", "no). Whether to
  overwrite vignette and functions if already exists.

- check_unregistered:

  Logical. Whether to help detect unregistered files. Typically files
  not created from a flat file and added manually in the repository.

- codecov:

  Logical. Whether to compute code coverage with
  [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md).

- stylers:

  Function to be run at the end of the process, like
  [`styler::style_pkg`](https://styler.r-lib.org/reference/style_pkg.html)
  or `lintr::lint_package` or a lambda function combining functions
  like: `function() {styler::style_pkg(); lintr::lint_package()}`. For a
  unique function, use the format without parenthesis `()` at the end of
  the command.

- ...:

  Arguments passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
  For example, you can do `inflate(check = TRUE, quiet = TRUE)`, where
  `quiet` is passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

## Value

side effect. Inflates all your flat files that can be inflated.

## Details

This requires to
[`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
all flat files individually at least once, so that their specific
inflate configurations are stored.

This also requires to register all R, tests and vignettes files of your
package, even if not created with an inflate. Run `inflate_all()` once
and read the messages. The first time, you will probably need to run
[`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
if your package is not new.

For more information, read the
[`vignette("inflate-all-your-flat-files", package = "fusen")`](https://thinkr-open.github.io/fusen/articles/inflate-all-your-flat-files.md)

## See also

[`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
for the options of a single file inflate,
[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
for the list of files not already associated with a flat file in the
config file,
[`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
for automatically registering all files already present in the project
before the first `inflate_all()`

## Examples

``` r
if (FALSE) { # \dontrun{
# Usually, in the current package run inflate_all() directly
# These functions change the current user workspace
inflate_all()
# Or inflate_all_no_check() to prevent checks to run
inflate_all_no_check()
# Or inflate with the styler you want
inflate_all(stylers = styler::style_pkg)
} # }

# You can also inflate_all flats of another package as follows
# Example with a dummy package with a flat file
dummypackage <- tempfile("inflateall.otherpkg")
dir.create(dummypackage)
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
flat_files <- add_minimal_package(
  pkg = dummypackage,
  overwrite = TRUE,
  open = FALSE
)
#> Created file .here in /tmp/Rtmp005QYy/inflateall.otherpkg1b6866f1c631 . Please start a new R session in the new project directory.
flat_file <- flat_files[grep("flat", basename(flat_files))]
# Inflate the flat file once
usethis::with_project(dummypackage, {
  # if you are starting from a brand new package, inflate_all() will crash
  # it's because of the absence of a fusen config file
  #
  # inflate_all() # will crash

  # Add licence
  usethis::use_mit_license("John Doe")

  # you need to inflate manually your flat file first
  inflate(
    pkg = dummypackage,
    flat_file = flat_file,
    vignette_name = "Get started",
    check = FALSE,
    open_vignette = FALSE,
    document = TRUE,
    overwrite = "yes"
  )

  # your config file has been created
  config_yml_ref <-
    yaml::read_yaml(getOption("fusen.config_file", default = "dev/config_fusen.yaml"))
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/inflateall.otherpkg1b6866f1c631".
#> ✔ Adding "MIT + file LICENSE" to License.
#> ✔ Writing LICENSE.
#> ✔ Writing LICENSE.md.
#> ✔ Adding "^LICENSE\\.md$" to .Rbuildignore.
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> Writing NAMESPACE
#> ✔ Adding knitr to Suggests field in DESCRIPTION.
#> ☐ Use `requireNamespace("knitr", quietly = TRUE)` to test if knitr is
#>   installed.
#> ☐ Then directly refer to functions with `knitr::fun()`.
#> ✔ Adding "inst/doc" to .gitignore.
#> ── config file for dev/flat_minimal.Rmd ────────────────────────────────────────
#> ✔ R: R/my_fun.R was added to the config file
#> ✔ tests: tests/testthat/test-my_fun.R was added to the config file
#> ✔ vignettes: vignettes/get-started.Rmd was added to the config file
#> Saving attachment parameters to yaml config file
#> Updating inflateall.otherpkg1b6866f1c631 documentation
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> Writing NAMESPACE
#> Writing my_fun.Rd
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> [+] 2 package(s) added: rmarkdown, testthat.
#> ✔ Setting active project to "/tmp/Rtmp005QYy/dummy.package1b688202d64".

# Next time, you can run inflate_all() directly
usethis::with_project(dummypackage, {
  # now you can run inflate_all()
  inflate_all(check = FALSE, document = TRUE)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/inflateall.otherpkg1b6866f1c631".
#> ✔ The flat file flat_minimal.Rmd is going to be inflated
#> ── config file for dev/flat_minimal.Rmd ────────────────────────────────────────
#> ── check not registered files ──────────────────────────────────────────────────
#> ✔ There are no unregistered files.
#> Documentation parameters were restored from attachment config file.
#> Updating inflateall.otherpkg1b6866f1c631 documentation
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> Writing NAMESPACE
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> ✔ Setting active project to "/tmp/Rtmp005QYy/dummy.package1b688202d64".

# If you wish, the code coverage can be computed
usethis::with_project(dummypackage, {
  # now you can run inflate_all()
  inflate_all(check = FALSE, document = TRUE, codecov = TRUE)
})
#> ✔ Setting active project to "/tmp/Rtmp005QYy/inflateall.otherpkg1b6866f1c631".
#> ✔ The flat file flat_minimal.Rmd is going to be inflated
#> ── config file for dev/flat_minimal.Rmd ────────────────────────────────────────
#> ── check not registered files ──────────────────────────────────────────────────
#> ✔ There are no unregistered files.
#> Documentation parameters were restored from attachment config file.
#> Updating inflateall.otherpkg1b6866f1c631 documentation
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> Writing NAMESPACE
#> ℹ Loading inflateall.otherpkg1b6866f1c631
#> ℹ Computing code coverage - it might take some time
#> inflateall.otherpkg1b6866f1c631 Coverage: 0.00%
#> R/my_fun.R: 0.00%
#> ✔ Setting active project to "/tmp/Rtmp005QYy/dummy.package1b688202d64".

# Clean the temporary directory
unlink(dummypackage, recursive = TRUE)
```
