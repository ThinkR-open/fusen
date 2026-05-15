# Fill DESCRIPTION file of the package

Fill DESCRIPTION file of the package

## Usage

``` r
fill_description(pkg = ".", fields, overwrite = FALSE)
```

## Arguments

- pkg:

  Path to package

- fields:

  A named list of fields to add to DESCRIPTION, potentially overriding
  default values. See
  [`use_description`](https://usethis.r-lib.org/reference/use_description.html)
  for how you can set personalized defaults using package options

- overwrite:

  Whether to overwrite existing DESCRIPTION

## Value

Fill DESCRIPTION file with fields. Return path to file.

## Examples

``` r
# Create a new project
dummypackage <- tempfile("dummy.package.desc")
dir.create(dummypackage)

fill_description(
  pkg = dummypackage,
  fields = list(
    Title = "Build A Package From Rmarkdown file",
    Description = paste(
      "Use Rmd First method to build your package.",
      "Start your package with documentation.",
      "Everything can be set from a Rmarkdown file in your project."
    ),
    `Authors@R` = c(
      person(
        "John",
        "Doe",
        email = "john@email.me",
        role = c("aut", "cre"),
        comment = c(ORCID = "0000-0000-0000-0000")
      )
    )
  )
)
#> Warning: Invalid ORCID iD: ‘0000-0000-0000-0000’.
#> ! Title was modified to 'Title Case'.

# Delete dummy package
unlink(dummypackage, recursive = TRUE)
```
