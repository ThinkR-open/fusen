# fusen 0.7.1

## Bug fix

- Skip some tests on MacOS on CRAN to avoid errors

# fusen 0.7.0

## New features

- Adding `sepuku()`, allowing to remove all traces of a fusen-related files and tags in the package (#257 #258 #259) @ymansiaux
- `inflate` and `inflate_all()` proposes to compute the package code coverage using `covr::package_coverage()` using the `codecov` parameter (default is FALSE)
- RStudio Addin "Fusen : Add fusen chunks" as well as `fusen::add_fusen_chunks()` add a development chunk in complement to the "function", "example" and "test" chunks already provided (#255) @ymansiaux

## Minor changes

- Add `# nolint: line_length_linter` in created files to avoid line length linter warnings (#278) @LBeaulaton

## Bug fixes

- Fix `grep()` calls in unit tests (#271) @statnmap

# fusen 0.6.0

## Breaking changes

- {fusen} now relies on {lightparser} instead of {parsermd} to parse flat file. This allows to avoid installation problems with {parsermd}, which is not updated anymore. As {lightparser} is lighter, this may have unattended effects on specific flat file cases. Please report any issue you may encounter. (#233)
- `inflate_all*()` does not use parameter `clean` anymore. Use `check_unregistered` instead to check if all files are registered in the configuration file.

## New features

- `draw_package_structure()` along with `get_package_structure()` allows to draw the package structure with
  all functions created in each R file, and whether they are exported (#189)
- `rename_flat_file()` allows to rename a flat file, and deals with config and inflated files
- `deprecate_flat_file()` helps properly deprecate a flat file, modifies the config file
  and cleans the previously inflated files
- `inflate()` detects functions renamed or removed and allow to clean the package repository (#24)
- Allow `organisation` in `init_share_on_github()` to send to a GitHub organisation
- Fix `load_flat_functions()` to work with VSCode

# fusen 0.5.2

## New features

- Allow a styler function with parameter `stylers` in `inflate_all*()` (e.g `inflate_all(stylers = styler::style_pkg)`)

## Minor changes

- Fix use of `packageVersion()` with character
- Allow "." for current package when adding flat file without DESCRIPTION (#224)

# fusen 0.5.1

## New features

### Inflate all active flat file

- `inflate_all()` uses the configuration file to inflate all your flat files at once. `document` and `check` options are thus only run once for all flat files. (#204, @ymansiaux)
- This requires to run `inflate()` at least once for each flat file.
- This also requires to register all other files, that were present in the package before this version of 'fusen' with `register_all_to_config()`

### List all files created with an `inflate()` in a config file with parameters

- `inflate()` creates a configuration file "dev/config_fusen.yaml" to register all files created while inflating the corresponding flat file, along with inflate parameters (#198, @ymansiaux)

## Breaking changes

- Arguments `rmd` and `name` in function `inflate()` now lead to errors (Deprecated since v0.3.0).
- `add_dev_history()` was deprecated since v0.3.0 in favor of `add_flat_template()`. Now `add_dev_history()` only adds a "dev_history.Rmd" file in the "dev/" directory.
- `add_flat_template(template = "minimal")` no longer exists to avoid confusion between minimal package or minimal flat file. Indeed, now there are `add_flat_template(template = "minimal_package")` (also `add_minimal_package()`) or `add_flat_template(template = "minimal_flat")` (also `add_minimal_flat()`). The latter doing exactly the same as `add_additional()` (#187)
- `create_fusen()` still uses `minimal` as `minimal_package`

## Bug fixes

- Fix using line break after function name in flat files (#142, @FlorenceMounier)
- If project directory is renamed by "my.package (Copy)", `inflate()` still works, even if this name is not a proper package name. What is important is that DESCRIPTION Package name is correct.

## Major changes

- `create_fusen()` and the RStudio gui interface now accept `flat_file` parameter to name the first flat file as well as the first function (when using 'minimal' template).
- The tips and tricks vignette shortly presents how to combine {fusen} and {golem} (#187)
- Incorrect function names issued from addins or `add_flat_template()` are cleaned before being included in the flat file to follow underscore rule.

## Minor changes

- Update CONTRIBUTING to speak about flat file in {fusen} itself
- replace the maintainer's name from `fill_description()` in examples, templates and tests (#155, @FlorenceMounier)
- `create_fusen()` vaccinates created git project (#171)
- Examples under function roxygen documentation are cleaned from extra spaces after empty `#'` to avoid git diff against code linters / stylers.

# fusen 0.5.0

## New features

### Publish your package website on GitHub

- Publish your {fusen} project on a GitHub website with one command: `init_share_on_github()`

### List all files created with an `inflate()` in a config file

- `inflate()` creates a "dev/config_fusen.yaml" file to register all files created while inflating the corresponding flat file (First steps in #24)
- Migrate from a non-fusen package or a previous version of 'fusen' with `register_all_to_config()`
- Create or update the config file from a data.frame with `df_to_config()` to list legitimate scripts (even if not associated with a flat file)

### Others

- Allow multiple examples for the same function (#149)

## Bug fixes

- Fix for when using word "`function(`" in documentation (#174, @FlorenceMounier)

# fusen 0.4.2

## Bug

- Replacement of unexported usethis functions (#205, @ateucher)

# fusen 0.4.1

## New features

- Load all `function` chunks of the flat file currently opened with `load_flat_functions()` (Like a `load_all()` for a flat file)
- Allow to `inflate()` a Quarto ".qmd" flat file (#160)

## Minor

- Fix HTML5 doc

# fusen 0.4.0

## New features

- `inflate()` the current opened flat file if `flat_file` is empty (#138)
- Add rmarkdown template for additional flat file for RStudio
- Add wrappers around `add_flat_template()` for lazy devs: `add_additional()`, `add_full()`, `add_minimal()`
- Show "flat_template" origin of files generated by {fusen} (@
  ALanguillaume)
- Allow `inflate(vignette_name = c("Super title" = "01-Super Slug"))` for nice Title different from vignette Entry (#87)
- Get the author and date from flat file to the vignette (#129)

## Bug fixes

- Read DESCRIPTION file for package name when available (#144 @VincentGuyader)
- Read `nyc_squirrels` with encoding to avoid encoding problems with `use_data()`
- Allow flat files with `tests` only
- Extract yaml metadata from flat file, like author or date to include in the inflated vignette
- Simplify "flat_teaching" with a unique simple function
- Fix `asciify_name()` to account for diacritics (@
  ALanguillaume)
- Improve template 'full' for internal data use
- Fix tests when git does not exists

# fusen 0.3.0

## Breaking changes

- `add_flat_template()` superseeds `add_dev_history()` with more advanced possibilities
- `add_dev_history()` is deprecated
- Vignette name to create is now set with `inflate(vignette_name = "Get started")` instead of `name`
- Flat name to inflate is now set with `inflate(flat_file = "dev/flat_full.Rmd")` instead of `rmd`

## Major changes

- Check included now uses `devtools::check()` instead of `rcmdcheck()`
- Avoid creating vignette with `inflate(vignette_name = NA)`
- Decide whether or not to open vignette when inflate with `inflate(open_vignette = FALSE)`
- Improve documentation included in flat templates to reflect changes in using dev_history file
- Add RStudio Addin to insert a new flat template
- Add RStudio Addin to insert chunks for new function (@ColinFay)
- Deal with `\dontrun{}` in example chunks
- Allow short names for chunks: dev, fun, ex, test
- `create_fusen()` to create a {fusen} project from command line or with RStudio new project (@ALanguillaume)
- Add "do not edit by hand" in files generated

### Grouping functions under the same file

- Group functions in same R file and test file if under same (level 1 + level 2) titles in the Rmd
- Group functions in same R file and test file if they have the same `@rdname` roxygen tag
- Group functions in same R file and test file if they have the same `@filename` roxygen tag (only recognized by 'fusen')
- Group functions in same R file and test file if the function chunk get chunk option `{r function-my_func, filename = "my_filename"}`

## Minor changes

- `add_flat_template()` uses the `flat_name` to pre-fill the template with the first function name.
- Fix .onLoad functions file creation
- Allow `R6Class()` in `function` chunks
- Fix inflate function chunks with data or package documentation only
- Fix inflate with empty functions chunks
- Fix filename to inflate in templates with new calls of `add_dev_history()` (@Cervangirard)
- Default vignette name is now "Get started" creating "vignettes/get-started.Rmd"
- All open files are saved when using `inflate()` where {RStudioapi} works
- Ask to restart RStudio after first inflate

# fusen 0.2.4

- Update vignette and tests

# fusen 0.2.3

- Update unit tests
- Show check outputs in console
- Ask before overwriting everything
- Check Description Title and description fields
- Check if folder name is correct package name

# fusen 0.2.2

- Protect tests from older Pandoc versions

# fusen 0.2.1

- Fix documentation issues for CRAN
- Add templates for "dev_history.Rmd" file
- Add more informative messages to users
- New vignette to explain how to maintain a {fusen} package

# fusen 0.2.0

- Prepare for CRAN

# fusen 0.1.1

## features

- Allow non-clean vignette name
- Allow different "dev_history" templates: "full", "minimal" and "additional"

## documentation

- Add vignette to explain how to maintain a package with {fusen}
- Add vignette to explain how to deal with data
- Added a `NEWS.md` file to track changes to the package.

## tests

- Add tests for corner cases in Rmd templates

# fusen 0.1.0

- First release
