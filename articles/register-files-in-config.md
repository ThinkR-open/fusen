# Register files in config

``` r

library(fusen)
```

## {fusen} registers all files of your package

From {fusen} \>=0.5, a file named “dev/config_fusen.yaml” (by default)
registers all files created from a specific “flat” file as soon as you
inflate it. This helps you keep track on the link between flat files and
all R, tests and vignettes files created in your package.

This configuration file also register all files not created from a flat
file. This helps you keep your package clean.

To know if you have some special steps to realize with this version of
{fusen}, run:

``` r

fusen::inflate_all_no_check()
```

Messages of this function will guide you through the transition if you
used a too old version of {fusen}.

Chances are that you will need to:

1.  Inflate each flat file of your project one by one (at least once)
    with this version of {fusen}
2.  Run
    [`fusen::check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
3.  Check the content of “dev/config_not_registered.csv” if there are
    unregistered files.
4.  Clean up your package of useless unregistered files if required.
5.  Register all remaining unregistered files with
    [`fusen::register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)

Try again
[`fusen::inflate_all_no_check()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
or
[`fusen::inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
and that should be good now.  
*Read
[`vignette("inflate-all-your-flat-files", package = "fusen")`](https://thinkr-open.github.io/fusen/articles/inflate-all-your-flat-files.md)
to know more about
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md).*

## What is the configuration file?

The configuration file is stored as “dev/config_fusen.yaml”.  
This lists all flat files that were inflated, along with the lists of
all files created during inflate and the parameters used to inflate.  
This also list all other files, not created with the flat files, under a
section named “keep”.

For instance, a part of the configuration file of package {fusen} looks
like:

``` yaml
flat_addins.Rmd:
  path: dev/flat_addins.Rmd
  state: active
  R: R/build_fusen_chunks.R
  tests: tests/testthat/test-build_fusen_chunks.R
  vignettes: []
  inflate:
    flat_file: dev/flat_addins.Rmd
    vignette_name: .na
    open_vignette: true
    check: false
    document: true
    overwrite: ask
keep:
  path: keep
  R:
  - R/addins.R
  - R/create_fusen_rsproject.R
  - R/fusen-package.R
  - R/globals.R
  - R/inflate-utils.R
  - R/load_flat_functions.R
  - R/utils-pipe.R
  tests:
  - tests/testthat/test-create_fusen_rsproject.R
  - tests/testthat/test-inflate_qmd.R
  - tests/testthat/test-inflate_utils.R
  - tests/testthat/test-load_flat_functions.R
  - tests/testthat/test-skeleton.R
  vignettes: vignettes/tips-and-tricks.Rmd
```

## What about unregistered files?

Imagine you inflated some function named `my_fun()`, and then you decide
to change the name of the function to `my_new_fun()`. After inflate,
there will be two files “R/myfun.R” and “R/my_new_fun.R” in your
package. However, “R/myfun.R” should be deleted because this function
should not exist anymore.

If you are unsure about which files should still be there or not, you
can run
[`fusen::check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md).
In this case, “R/myfun.R” will appear in the csv file created in
“dev/config_not_registered.csv”, with a message like “Possibly
deprecated file”.

You now have the possibility to detect deprecated files of your
project.  
Run
[`fusen::check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
as often as you need if you want to clean your project. *Note that this
function is included in
[`fusen::inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
and
[`fusen::inflate_all_no_check()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md),
which let you the opportunity to clean your project regularly.*

To be able to detect deprecated files, all legitimate files need to be
registered. Even the ones that you created manually. In this case, they
will be listed under the `"keep"` section. {fusen} knows that it should
not bother you with these files.

## Deprecate flat files after inflate

In the example configuration file above, you can note that the
“flat_addins.Rmd” section has a parameter `state: active` which means
that
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
will indeed inflate this file.

If your practice of {fusen} is to inflate it once, and then continue
development in the R, test and vignette files directly, then you may
want to properly deprecated the flat file.

In this case, we recommend to store your flat file in a sub-directory
named “dev/dev_history/” as this will be the default behavior in a
future release. Then, you can change your flat file to
`state: deprecated` in the configuration file, and change both `path:`
section of the file too.

## Detect all files that are not registered in configuration file

[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
shows files that are not already registered in the yaml config file.  
*Note that
[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
will try to guess the source flat template, if you used {fusen} \>= 0.4
before.*

The csv file “dev/config_not_registered.csv” created by
[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
lists all unregistered files. It contains three columns:

- type: The type of file among R, test or vignette
- path: The path where to find the unregistered file
- origin: If the file was created from a former flat file, the origin
  may be detected.
  - If the flat file detected already appears in the configuration file,
    the unregistered file is probably a deprecated file that needs to be
    deleted. But only you know.
  - If the flat file detected is not already in the configuration file,
    the section for this flat file will be created during the next step
    with
    [`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md).
  - If no source was detected, this probably means that you created it
    manually.

Decide which files you want to delete. Then run
[`fusen::register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
to register all remaining files in the “keep” section.

Have a look at the configuration file at the end of the process to check
that everything is set properly. Maybe some flat files detected should
not be in `state: active` but in `state: deprecated`. You’re the only
one who knows.

``` r

#' \dontrun{
# Run this on the current package in development
out_csv <- check_not_registered_files()
file.edit(out_csv)
#' }
```

``` r

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
unlink(dummypackage, recursive = TRUE)
```

## How `register_all_to_config()` works

If all files of the current state are legitimate, then, you can register
everything in the config file.

Run
[`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
in your current package to create the “dev/config_fusen.yaml” that
registers all your files. You will note a “keep” section, which lists
all files for which the source was not guessed.

This function deletes the “config_not_registered.csv” created with
[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md).

``` r

#' \dontrun{
# Usually run this one inside the current project
# Note: running this will write "dev/config_fusen.yaml" in your working directory
register_all_to_config()
#' }
```

``` r

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
```
