# Rename a flat file

Rename a flat file

## Usage

``` r
rename_flat_file(flat_file, new_name)
```

## Arguments

- flat_file:

  Path to the flat file to rename

- new_name:

  New name for the flat file

## Value

Used for side effect. Flat file renamed, config file updated, inflated
files modified when needed.

## Examples

``` r
if (FALSE) { # \dontrun{
# These functions change the current user workspace
dev_file <- suppressMessages(
  add_flat_template(
    template = "add",
    pkg = ".",
    overwrite = TRUE,
    open = FALSE
  )
)
rename_flat_file(
  flat_file = "dev/flat_additional.Rmd",
  new_name = "flat_new.Rmd"
)
} # }
```
