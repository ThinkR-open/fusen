# Deprecate a flat file

It is not inflated again during `[inflate_all()]` as it is identified as
deprecated in the config file. Previously generated files get "do not
edit by hand" message removed. The flat file is moved to
"dev/flat_history".

## Usage

``` r
deprecate_flat_file(flat_file)
```

## Arguments

- flat_file:

  Path to the flat file to deprecate

## Value

Used for side effect. Move flat file to "dev/flat_history", update
config file, and remove "do not edit by hand" message.

## Examples

``` r
if (FALSE) { # \dontrun{
# These functions change the current user workspace
dev_file <- suppressMessages(
  add_flat_template(
    template = "add",
    pkg = dummypackage,
    overwrite = TRUE,
    open = FALSE
  )
)
deprecate_flat_file(flat_file = "dev/flat_additional.Rmd")
} # }
```
