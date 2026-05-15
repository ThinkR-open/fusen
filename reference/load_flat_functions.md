# Load the code of all 'function' chunk in a flat file

Load the code of all 'function' chunk in a flat file

## Usage

``` r
load_flat_functions(flat_file, envir = globalenv())
```

## Arguments

- flat_file:

  Path to the flat to load functions from

- envir:

  the [`environment`](https://rdrr.io/r/base/environment.html) in which
  `expr` is to be evaluated. May also be `NULL`, a list, a data frame, a
  pairlist or an integer as specified to
  [`sys.call`](https://rdrr.io/r/base/sys.parent.html).

## Value

Path to flat file loaded. Used for side effect: Load functions in the
global environment.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use this command directly in the console
fusen::load_flat_functions()

# Or choose a flat file to load functions from
load_flat_functions(flat_file = "dev/flat_full.Rmd")
load_flat_functions(flat_file = "dev/flat_clean_fusen_files.Rmd")
} # }
```
