# Get all functions created in a R file

Get all functions created in a R file

## Usage

``` r
get_all_created_funs(file)
```

## Arguments

- file:

  A R file

## Value

A character vector of function names

## Examples

``` r
file_path <- tempfile(fileext = ".R")
cat(
  "my_fun <- function() {1}",
  "my_fun2 <- function() {2}",
  sep = "\n",
  file = file_path
)
get_all_created_funs(file_path)
#> [1] "my_fun"  "my_fun2"
```
