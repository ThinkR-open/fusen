# Changelog

## fusen 0.7.2

CRAN release: 2025-08-29

### Minor changes

- Fixed compatibility issue with upcoming `usethis` v3.2.0. Thanks to
  [@jennybc](https://github.com/jennybc) for the report and
  investigation.

## fusen 0.7.1

CRAN release: 2025-01-26

### Bug fix

- Skip some tests on MacOS on CRAN to avoid errors

## fusen 0.7.0

CRAN release: 2025-01-14

### New features

- Adding
  [`sepuku()`](https://thinkr-open.github.io/fusen/reference/sepuku.md),
  allowing to remove all traces of a fusen-related files and tags in the
  package ([\#257](https://github.com/Thinkr-open/fusen/issues/257)
  [\#258](https://github.com/Thinkr-open/fusen/issues/258)
  [\#259](https://github.com/Thinkr-open/fusen/issues/259))
  [@ymansiaux](https://github.com/ymansiaux)
- `inflate` and
  [`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
  proposes to compute the package code coverage using
  [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md)
  using the `codecov` parameter (default is FALSE)
- RStudio Addin “Fusen : Add fusen chunks” as well as
  [`fusen::add_fusen_chunks()`](https://thinkr-open.github.io/fusen/reference/add_fusen_chunks.md)
  add a development chunk in complement to the “function”, “example” and
  “test” chunks already provided
  ([\#255](https://github.com/Thinkr-open/fusen/issues/255))
  [@ymansiaux](https://github.com/ymansiaux)

### Minor changes

- Add `# nolint: line_length_linter` in created files to avoid line
  length linter warnings
  ([\#278](https://github.com/Thinkr-open/fusen/issues/278))
  [@LBeaulaton](https://github.com/LBeaulaton)

### Bug fixes

- Fix [`grep()`](https://rdrr.io/r/base/grep.html) calls in unit tests
  ([\#271](https://github.com/Thinkr-open/fusen/issues/271))
  [@statnmap](https://github.com/statnmap)

## fusen 0.6.0

CRAN release: 2024-05-14

### Breaking changes

- {fusen} now relies on {lightparser} instead of {parsermd} to parse
  flat file. This allows to avoid installation problems with {parsermd},
  which is not updated anymore. As {lightparser} is lighter, this may
  have unattended effects on specific flat file cases. Please report any
  issue you may encounter.
  ([\#233](https://github.com/Thinkr-open/fusen/issues/233))
- `inflate_all*()` does not use parameter `clean` anymore. Use
  `check_unregistered` instead to check if all files are registered in
  the configuration file.

### New features

- [`draw_package_structure()`](https://thinkr-open.github.io/fusen/reference/get_package_structure.md)
  along with
  [`get_package_structure()`](https://thinkr-open.github.io/fusen/reference/get_package_structure.md)
  allows to draw the package structure with all functions created in
  each R file, and whether they are exported
  ([\#189](https://github.com/Thinkr-open/fusen/issues/189))
- [`rename_flat_file()`](https://thinkr-open.github.io/fusen/reference/rename_flat_file.md)
  allows to rename a flat file, and deals with config and inflated files
- [`deprecate_flat_file()`](https://thinkr-open.github.io/fusen/reference/deprecate_flat_file.md)
  helps properly deprecate a flat file, modifies the config file and
  cleans the previously inflated files
- [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  detects functions renamed or removed and allow to clean the package
  repository ([\#24](https://github.com/Thinkr-open/fusen/issues/24))
- Allow `organisation` in
  [`init_share_on_github()`](https://thinkr-open.github.io/fusen/reference/init_share_on_github.md)
  to send to a GitHub organisation
- Fix
  [`load_flat_functions()`](https://thinkr-open.github.io/fusen/reference/load_flat_functions.md)
  to work with VSCode

## fusen 0.5.2

CRAN release: 2023-08-17

### New features

- Allow a styler function with parameter `stylers` in `inflate_all*()`
  (e.g `inflate_all(stylers = styler::style_pkg)`)

### Minor changes

- Fix use of
  [`packageVersion()`](https://rdrr.io/r/utils/packageDescription.html)
  with character
- Allow “.” for current package when adding flat file without
  DESCRIPTION ([\#224](https://github.com/Thinkr-open/fusen/issues/224))

## fusen 0.5.1

CRAN release: 2023-06-29

### New features

#### Inflate all active flat file

- [`inflate_all()`](https://thinkr-open.github.io/fusen/reference/inflate_all.md)
  uses the configuration file to inflate all your flat files at once.
  `document` and `check` options are thus only run once for all flat
  files. ([\#204](https://github.com/Thinkr-open/fusen/issues/204),
  [@ymansiaux](https://github.com/ymansiaux))
- This requires to run
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  at least once for each flat file.
- This also requires to register all other files, that were present in
  the package before this version of ‘fusen’ with
  [`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)

#### List all files created with an `inflate()` in a config file with parameters

- [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  creates a configuration file “dev/config_fusen.yaml” to register all
  files created while inflating the corresponding flat file, along with
  inflate parameters
  ([\#198](https://github.com/Thinkr-open/fusen/issues/198),
  [@ymansiaux](https://github.com/ymansiaux))

### Breaking changes

- Arguments `rmd` and `name` in function
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  now lead to errors (Deprecated since v0.3.0).
- [`add_dev_history()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  was deprecated since v0.3.0 in favor of
  [`add_flat_template()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md).
  Now
  [`add_dev_history()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  only adds a “dev_history.Rmd” file in the “dev/” directory.
- `add_flat_template(template = "minimal")` no longer exists to avoid
  confusion between minimal package or minimal flat file. Indeed, now
  there are `add_flat_template(template = "minimal_package")` (also
  [`add_minimal_package()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md))
  or `add_flat_template(template = "minimal_flat")` (also
  [`add_minimal_flat()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)).
  The latter doing exactly the same as
  [`add_additional()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  ([\#187](https://github.com/Thinkr-open/fusen/issues/187))
- [`create_fusen()`](https://thinkr-open.github.io/fusen/reference/create_fusen.md)
  still uses `minimal` as `minimal_package`

### Bug fixes

- Fix using line break after function name in flat files
  ([\#142](https://github.com/Thinkr-open/fusen/issues/142),
  [@FlorenceMounier](https://github.com/FlorenceMounier))
- If project directory is renamed by “my.package (Copy)”,
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  still works, even if this name is not a proper package name. What is
  important is that DESCRIPTION Package name is correct.

### Major changes

- [`create_fusen()`](https://thinkr-open.github.io/fusen/reference/create_fusen.md)
  and the RStudio gui interface now accept `flat_file` parameter to name
  the first flat file as well as the first function (when using
  ‘minimal’ template).
- The tips and tricks vignette shortly presents how to combine {fusen}
  and {golem} ([\#187](https://github.com/Thinkr-open/fusen/issues/187))
- Incorrect function names issued from addins or
  [`add_flat_template()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  are cleaned before being included in the flat file to follow
  underscore rule.

### Minor changes

- Update CONTRIBUTING to speak about flat file in {fusen} itself
- replace the maintainer’s name from
  [`fill_description()`](https://thinkr-open.github.io/fusen/reference/fill_description.md)
  in examples, templates and tests
  ([\#155](https://github.com/Thinkr-open/fusen/issues/155),
  [@FlorenceMounier](https://github.com/FlorenceMounier))
- [`create_fusen()`](https://thinkr-open.github.io/fusen/reference/create_fusen.md)
  vaccinates created git project
  ([\#171](https://github.com/Thinkr-open/fusen/issues/171))
- Examples under function roxygen documentation are cleaned from extra
  spaces after empty `#'` to avoid git diff against code linters /
  stylers.

## fusen 0.5.0

### New features

#### Publish your package website on GitHub

- Publish your {fusen} project on a GitHub website with one command:
  [`init_share_on_github()`](https://thinkr-open.github.io/fusen/reference/init_share_on_github.md)

#### List all files created with an `inflate()` in a config file

- [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  creates a “dev/config_fusen.yaml” file to register all files created
  while inflating the corresponding flat file (First steps in
  [\#24](https://github.com/Thinkr-open/fusen/issues/24))
- Migrate from a non-fusen package or a previous version of ‘fusen’ with
  [`register_all_to_config()`](https://thinkr-open.github.io/fusen/reference/register_all_to_config.md)
- Create or update the config file from a data.frame with
  `df_to_config()` to list legitimate scripts (even if not associated
  with a flat file)

#### Others

- Allow multiple examples for the same function
  ([\#149](https://github.com/Thinkr-open/fusen/issues/149))

### Bug fixes

- Fix for when using word “`function(`” in documentation
  ([\#174](https://github.com/Thinkr-open/fusen/issues/174),
  [@FlorenceMounier](https://github.com/FlorenceMounier))

## fusen 0.4.2

CRAN release: 2023-06-07

### Bug

- Replacement of unexported usethis functions
  ([\#205](https://github.com/Thinkr-open/fusen/issues/205),
  [@ateucher](https://github.com/ateucher))

## fusen 0.4.1

CRAN release: 2022-09-29

### New features

- Load all `function` chunks of the flat file currently opened with
  [`load_flat_functions()`](https://thinkr-open.github.io/fusen/reference/load_flat_functions.md)
  (Like a `load_all()` for a flat file)
- Allow to
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  a Quarto “.qmd” flat file
  ([\#160](https://github.com/Thinkr-open/fusen/issues/160))

### Minor

- Fix HTML5 doc

## fusen 0.4.0

CRAN release: 2022-04-30

### New features

- [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  the current opened flat file if `flat_file` is empty
  ([\#138](https://github.com/Thinkr-open/fusen/issues/138))
- Add rmarkdown template for additional flat file for RStudio
- Add wrappers around
  [`add_flat_template()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  for lazy devs:
  [`add_additional()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md),
  [`add_full()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md),
  `add_minimal()`
- Show “flat_template” origin of files generated by {fusen} (@
  ALanguillaume)
- Allow `inflate(vignette_name = c("Super title" = "01-Super Slug"))`
  for nice Title different from vignette Entry
  ([\#87](https://github.com/Thinkr-open/fusen/issues/87))
- Get the author and date from flat file to the vignette
  ([\#129](https://github.com/Thinkr-open/fusen/issues/129))

### Bug fixes

- Read DESCRIPTION file for package name when available
  ([\#144](https://github.com/Thinkr-open/fusen/issues/144)
  [@VincentGuyader](https://github.com/VincentGuyader))
- Read `nyc_squirrels` with encoding to avoid encoding problems with
  `use_data()`
- Allow flat files with `tests` only
- Extract yaml metadata from flat file, like author or date to include
  in the inflated vignette
- Simplify “flat_teaching” with a unique simple function
- Fix `asciify_name()` to account for diacritics (@ ALanguillaume)
- Improve template ‘full’ for internal data use
- Fix tests when git does not exists

## fusen 0.3.0

CRAN release: 2021-12-16

### Breaking changes

- [`add_flat_template()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  superseeds
  [`add_dev_history()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  with more advanced possibilities
- [`add_dev_history()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  is deprecated
- Vignette name to create is now set with
  `inflate(vignette_name = "Get started")` instead of `name`
- Flat name to inflate is now set with
  `inflate(flat_file = "dev/flat_full.Rmd")` instead of `rmd`

### Major changes

- Check included now uses
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  instead of `rcmdcheck()`
- Avoid creating vignette with `inflate(vignette_name = NA)`
- Decide whether or not to open vignette when inflate with
  `inflate(open_vignette = FALSE)`
- Improve documentation included in flat templates to reflect changes in
  using dev_history file
- Add RStudio Addin to insert a new flat template
- Add RStudio Addin to insert chunks for new function
  ([@ColinFay](https://github.com/ColinFay))
- Deal with `\dontrun{}` in example chunks
- Allow short names for chunks: dev, fun, ex, test
- [`create_fusen()`](https://thinkr-open.github.io/fusen/reference/create_fusen.md)
  to create a {fusen} project from command line or with RStudio new
  project ([@ALanguillaume](https://github.com/ALanguillaume))
- Add “do not edit by hand” in files generated

#### Grouping functions under the same file

- Group functions in same R file and test file if under same (level 1 +
  level 2) titles in the Rmd
- Group functions in same R file and test file if they have the same
  `@rdname` roxygen tag
- Group functions in same R file and test file if they have the same
  `@filename` roxygen tag (only recognized by ‘fusen’)
- Group functions in same R file and test file if the function chunk get
  chunk option `{r function-my_func, filename = "my_filename"}`

### Minor changes

- [`add_flat_template()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  uses the `flat_name` to pre-fill the template with the first function
  name.
- Fix .onLoad functions file creation
- Allow `R6Class()` in `function` chunks
- Fix inflate function chunks with data or package documentation only
- Fix inflate with empty functions chunks
- Fix filename to inflate in templates with new calls of
  [`add_dev_history()`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
  ([@Cervangirard](https://github.com/Cervangirard))
- Default vignette name is now “Get started” creating
  “vignettes/get-started.Rmd”
- All open files are saved when using
  [`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
  where {RStudioapi} works
- Ask to restart RStudio after first inflate

## fusen 0.2.4

CRAN release: 2021-08-04

- Update vignette and tests

## fusen 0.2.3

CRAN release: 2021-07-26

- Update unit tests
- Show check outputs in console
- Ask before overwriting everything
- Check Description Title and description fields
- Check if folder name is correct package name

## fusen 0.2.2

CRAN release: 2021-07-09

- Protect tests from older Pandoc versions

## fusen 0.2.1

CRAN release: 2021-07-08

- Fix documentation issues for CRAN
- Add templates for “dev_history.Rmd” file
- Add more informative messages to users
- New vignette to explain how to maintain a {fusen} package

## fusen 0.2.0

- Prepare for CRAN

## fusen 0.1.1

### features

- Allow non-clean vignette name
- Allow different “dev_history” templates: “full”, “minimal” and
  “additional”

### documentation

- Add vignette to explain how to maintain a package with {fusen}
- Add vignette to explain how to deal with data
- Added a `NEWS.md` file to track changes to the package.

### tests

- Add tests for corner cases in Rmd templates

## fusen 0.1.0

- First release
