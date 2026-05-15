# Add 'fusen' chunks

Create 'fusen' chunks inside your Rmd

## Usage

``` r
add_fusen_chunks(
  function_name = NULL,
  export = getOption("fusen.export.functions")
)
```

## Arguments

- function_name:

  Name of the function to create. If NULL (the default), the user will
  be prompted to enter it.

- export:

  Should the function be exported? Default is
  `getOption("fusen.export.functions")`. If NULL, the user will be
  prompted to enter it.

## Value

A list with the context and the content, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
add_fusen_chunks("this", export = TRUE)
} # }
```
