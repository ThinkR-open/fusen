# Add flat Rmd file that drives package development

Add flat Rmd file that drives package development

## Usage

``` r
add_additional(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "additional",
  overwrite = FALSE,
  open = TRUE
)

add_minimal_flat(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "minimal",
  overwrite = FALSE,
  open = TRUE
)

add_minimal_package(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "minimal",
  overwrite = FALSE,
  open = TRUE
)

add_full(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "full",
  overwrite = FALSE,
  open = TRUE
)

add_dev_history(pkg = ".", dev_dir = "dev", overwrite = FALSE, open = TRUE)

add_flat_template(
  template = c("full", "minimal_package", "minimal_flat", "additional", "teaching",
    "dev_history"),
  pkg = ".",
  dev_dir = "dev",
  flat_name = NULL,
  overwrite = FALSE,
  open = TRUE
)
```

## Arguments

- pkg:

  Path where to save file

- dev_dir:

  Name of directory for development Rmarkdown files. Default to "dev".

- flat_name:

  Name of the file to write in dev. Use the name of the main function of
  your template to get chunks pre-filled with this function name.

- overwrite:

  Whether to overwrite existing flat Rmd template file with same name

- open:

  Logical. Whether to open file after creation

- template:

  Name of the template to use. See details.

## Value

Create flat Rmd file(s) template(s) and return its (their) path

## Details

Choose `template` among the different templates available:

- "full": The full template with a reproducible package that can
  directly be inflated. It comes along with the "dev_history" template.
  Default.

- "minimal_package": Minimal template to start a new package when you
  already know 'fusen', along with the "dev_history" template. Note that
  this is called "minimal" in
  [`create_fusen()`](https://thinkr-open.github.io/fusen/reference/create_fusen.md).

- "minimal_flat" or "additional": Template for a new minimal flat file
  only.

- "teaching": Template with a reproducible package, simpler than "full",
  but everything to teach the minimal structure of a package.

- "dev_history": Template with functions commonly used during package
  development. This does not contain chunks to write your own functions.

Abbreviated names can also be used for the different templates: "add"
for additional, "minflat" for minimal_flat, "minpkg" for minimal_package
"teach" for teaching, "dev" for "dev_history".

`add_additional()`, `add_minimal_flat()`, `add_dev_history()`,
`add_minimal_package()`, `add_full()` are wrapper around
`add_flat_template("additional")`, ...

## Examples

``` r
# Create a new project
dummypackage <- tempfile("dummy.package.flat")
dir.create(dummypackage)

# Add
add_flat_template(template = "teaching", pkg = dummypackage)
#> Created file .here in /tmp/Rtmp005QYy/dummy.package.flat1b6869540466 . Please start a new R session in the new project directory.
#> [1] "/tmp/Rtmp005QYy/dummy.package.flat1b6869540466/dev/flat_teaching.Rmd"
# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# For classical use in your package
if (FALSE) { # \dontrun{
# first time ever using 'fusen'
add_flat_template("full")

# first time in your new package
add_flat_template("minimal_package")

# add new flat file for new functions
add_flat_template("add")
add_additional()
add_minimal_flat()

# add only the dev_history file in an existing package
add_dev_history()

# add new flat template for teaching (a reduced full template)
add_flat_template("teaching")
} # }
```
