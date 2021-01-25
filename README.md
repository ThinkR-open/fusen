
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fusen

<!-- badges: start -->

[![R build
status](https://github.com/ThinkR-open/fusen/workflows/R-CMD-check/badge.svg)](https://github.com/ThinkR-open/fusen/actions)
[![Codecov test
coverage](https://codecov.io/gh/ThinkR-open/fusen/branch/master/graph/badge.svg)](https://codecov.io/gh/ThinkR-open/fusen?branch=master)
<!-- badges: end -->

{fusen} inflates a Rmarkdown file to magically create a package.

> If you know how to create a Rmarkdown file, then you know how to build
> a package.

## Installation

You can install the development version of {fusen} from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("fusen")
```

## How to use it

  - Create a new directory / new project
  - Add the Rmarkdown template
  - Follow the `"dev_history.Rmd"` template to write your documentation
    and build your functions and test your examples.
      - Chunk named `function` gets the code of a function
      - Chunk named `example` gets the code for examples of using the
        function. This will be used for function `@examples` and will be
        kept for the vignette
      - Chunk named `tests` gets the code for unit testing
      - Chunk named `development` gets the code for development
        purposes, usually only used once like {usethis} functions
  - Inflate the template to transform it as a package with functions,
    unit tests and the current Rmd transformed as a vignette. And check.
  - Build your {pkgdown} site to verify everything is at the right place

*Note that the `"dev_history.Rmd"` template is indeed a working
example.*  
*Note also that {fusen} was itself created from the `"dev_history.Rmd"`
template available in its GitHub repository.*

``` r
# Create a new project
tmpdir <- tempdir()
dummypackage <- file.path(tmpdir, "dummypackage")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE)
# You may need to execute inflate() in the console directly
inflate(pkg = dummypackage, rmd = dev_file, name = "exploration")
# Build {pkgdown} to test it
pkgdown::build_site(pkg = dummypackage)
# > See references and articles
```

## Who is {fusen} for?

When you write a Rmarkdown file (or a vignette), you create a
documentation for your analysis (or package). Inside, you write some
functions, you test your functions with examples and you maybe write
some unit tests to verify the outputs. This is even more true if you
follow this guide : [‘Rmd first’: When development starts with
documentation](https://rtask.thinkr.fr/blog/rmd-first-when-development-starts-with-documentation/)
After that, you need to move your functions in the correct place. Let
{fusen} do that for you.

{fusen} is first addressed to people who never wrote a package before
but know how to write a Rmarkdown file. Understanding package
infrastructure and correctly settling it can be frightening. This
package may help them do the first step.

{fusen} is also addressed to more advanced developers who are fed up
with switching between R files, tests files, vignettes. In particular,
when changing arguments of a function, we need to change examples, unit
tests in multiple places. Here, you can do it in one place. No risk to
forget one.

## Why is this package named {fusen} ?

A fusen is an origami. It is a page of paper that you fold in a specific
way so that at the end, you can magically inflate it to let a nice box
appear.

<img src="img/fusen-origami.jpg" width="25%" />

Similarly, the {fusen} package uses one page of RMarkdown, that you fill
in a specific way so that at the end, you can magically `inflate()` it
to let a nice package appear.

## Aknowledgments

  - Thanks to Deemah who asked me to go further ‘Rmd first’ after my
    presentation at use’R 2019 in Toulouse: [‘The “Rmd first” method:
    when projects start with
    documentation’](https://github.com/statnmap/prez/blob/master/2019-07_useR_Toulouse.pdf)
    (Video on Youtube: <https://youtu.be/cB1BCxFbhtk>).
  - Thanks to @rundel and its package {parsermd} who helped me get back
    in this project with ease : <https://github.com/rundel/parsermd>

## Code of Conduct

Please note that the dummypackage project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
