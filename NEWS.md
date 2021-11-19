# fusen 0.2.4.9000

## Major changes

* `add_flat_template()` superseeds `add_dev_history()` with a more advanced use
* Add Rstudio Addin to insert chunks for new function (@ColinFay)
* Deal with `\dontrun{}` in example chunks
* Allow short names for chunks: dev, fun, ex, test
* `create_fusen()` to create a {fusen} project from command line or with RStudio new project (@ALanguillaume)

## Minor changes

* Fix filename to inflate in templates with new calls of `add_dev_history()` (@Cervangirard)
* Default vignette name is now "Get started" creating "vignettes/get-started.Rmd"
* All open files are saved when using `inflate()` where {rstudioapi} works

# fusen 0.2.4

* Update vignette and tests

# fusen 0.2.3

* Update unit tests
* Show check outputs in console
* Ask before overwriting everything
* Check Description Title and description fields
* Check if folder name is correct package name

# fusen 0.2.2

* Protect tests from older Pandoc versions

# fusen 0.2.1

* Fix documentation issues for CRAN
* Add templates for "dev_history.Rmd" file
* Add more informative messages to users
* New vignette to explain how to maintain a {fusen} package

# fusen 0.2.0

* Prepare for CRAN

# fusen 0.1.1

## features

* Allow non-clean vignette name
* Allow different "dev_history" templates: "full", "minimal" and "additional"

## documentation

* Add vignette to explain how to maintain a package with {fusen}
* Add vignette to explain how to deal with data
* Added a `NEWS.md` file to track changes to the package.

## tests

* Add tests for corner cases in Rmd templates

# fusen 0.1.0

* First release
