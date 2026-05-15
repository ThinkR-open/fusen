# Inflate all your flat files

``` r

library(fusen)
```

## Inflate all your flat files at once

The
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
function allows the user to inflate all the active flat files available
in the “dev/” directory at once. This requires to inflate each new flat
file individually at least once in order to register the inflate
parameters.

Prior to be able to inflate all the flat files, some checks are
performed, to assess whether the flat files are available to be used by
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md).

### Store configuration of each flat file once

[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
requires the existence of a complete {fusen} configuration file:
“dev/config_fusen.yaml”. This file exists only from versions of {fusen}
upper than v0.5.0:

- If you are working in a project initiated with a version of {fusen}
  prior to v0.5.0, please manually run
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  for each of the flat files you want to work with, in order to create
  the appropriate “dev/config_fusen.yaml” file.

- If you are already familiar with “dev/config_fusen.yaml” but were
  working with a dev version of {fusen} prior to v0.5.1, your
  “dev/config_fusen.yaml” file does not contain inflate-related
  parameters. You will have to inflate again you flat files in order to
  complete the configuration file with the proper
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  parameters.

### Prevent some flat files to be inflated

When a flat file is listed in the configuration file, it will be
inflated each time you run
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md).  
If you do not want this flat file to be inflated anymore, you can
deprecate it. Open the configuration file in “dev/config_fusen.yaml”.
Find the section concerning the flat file. Change the `state: active` to
`state: deprecated`. It will not be inflated during the following calls
to
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md).

You may have a flat file that is a work in progress and is not inflated
yet. This will not affect other flat files to be inflated. In this case
you may see the following message. You can ignore it.

    The flat file {flat} is not going to be inflated.
    It was detected in your flats directory but it is absent from the config file.
    Please inflate() it manually when you are ready, so that it is accounted the next time.

### Wrapper around `inflate_all()`

There is a wrapper named
[`inflate_all_no_check()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
that will prevent running
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
This is a short version of `inflate_all(check = FALSE)`.

### Register all other files to help clean your project

Note also that all files stored in R, tests and vignettes directories
are checked to detect those not created from a flat file. They will need
to be registered in the config file too, in order to help you keep your
package clean of deprecated files.
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
thus runs
[`check_not_registered_files()`](https://thinkr-open.github.io/fusen/reference/check_not_registered_files.md)
behind the scene and informs about the procedure. Read
[`vignette('register-files-in-config', package = 'fusen')`](https://thinkr-open.github.io/fusen/articles/register-files-in-config.md)
to get more information.

### Compute the code coverage

If you want to compute the code coverage of your package, you can run
`inflate_all(codecov = TRUE)`. It will run
[`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md)
at the end of the process.

### Complete the process with your own code style

You can run your preferred styling functions just before the check of
the package in
[`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md).
For instance, if you want {styler} package to clean your code during the
inflate process, you can run `inflate_all(stylers = styler::style_pkg)`.
If you also would like to clean the “dev/” directory, you can run
`inflate_all(stylers = function() {styler::style_pkg(); styler::style_dir("dev")})`.

``` r

#' \dontrun{
# Usually, in the current package run inflate_all() directly
# These functions change the current user workspace
inflate_all()
# Or inflate_all_no_check() to prevent checks to run
inflate_all_no_check()
# Or inflate with the styler you want
inflate_all(stylers = styler::style_pkg)
#' }

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

# Next time, you can run inflate_all() directly
usethis::with_project(dummypackage, {
  # now you can run inflate_all()
  inflate_all(check = FALSE, document = TRUE)
})

# If you wish, the code coverage can be computed
usethis::with_project(dummypackage, {
  # now you can run inflate_all()
  inflate_all(check = FALSE, document = TRUE, codecov = TRUE)
})

# Clean the temporary directory
unlink(dummypackage, recursive = TRUE)
```
