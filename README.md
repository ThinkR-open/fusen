
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![fusen status
badge](https://thinkr-open.r-universe.dev/badges/fusen)](https://thinkr-open.r-universe.dev)
[![CRAN
status](https://www.r-pkg.org/badges/version/fusen)](https://CRAN.R-project.org/package=fusen)
[![R-CMD-check](https://github.com/ThinkR-open/fusen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThinkR-open/fusen/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ThinkR-open/fusen/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ThinkR-open/fusen)
[![](https://cranlogs.r-pkg.org/badges/fusen)](https://cran.r-project.org/package=fusen)
<!-- badges: end -->

# fusen <img src="man/figures/logo.png" align="right" alt="" width="120" />

{fusen} inflates a Rmarkdown file to magically create a package.

> If you know how to create a Rmarkdown file, then you know how to build
> a package.

<img src="man/figures/fusen_inflate_functions.png" width="75%" style="display: block; margin: auto;" />

*Fill the flat Rmd (or qmd) template with everything in one place and
{fusen} will inflate the identified parts in the correct package files
and directories.*

> The {fusen} Rmarkdown template encourages users to fill their
> documentation and tests at the same time of writing their functions
> code. Thanks to the R package structure used by {fusen}, you can build
> a robust workflow or R package. {fusen} simplifies and reduces the
> number of steps towards a full R package.  
> After that, your {pkgdown} documentation website is one command away
> to be shared with all your users.

This {fusen} package is a real-world example of {fusen} use as it is
itself created from the flat templates available in `"dev/"` folder in
its GitHub repository. You can have a look at its architecture in the
[Readme of the “dev”
directory](https://github.com/ThinkR-open/fusen/blob/main/dev/README.md).

## Installation

You can install the released CRAN version:

``` r
install.packages("fusen")
```

> *Full documentation for the CRAN version is here:
> <https://thinkr-open.github.io/fusen/>*

You can install the development version of {fusen} from r-universe or
GitHub:

``` r
# From r-universe.dev (No need for GITHUB_PAT)
install.packages("fusen", repos = c("https://thinkr-open.r-universe.dev", "https://cloud.r-project.org"))

# Or with {pak} using GitHub API - Need for GITHUB_PAT
# install.packages("pak")
pak::pak("ThinkR-open/fusen")
```

> *Full documentation for the development version is here:
> <https://thinkr-open.github.io/fusen/dev/>*

## You are one Rmd away from building a package!

*{fusen} is all about correctly separating and naming chunks.*

- Create a new directory / new project with
  - RStudio template: File \> New Project \> New directory \> Package
    using {fusen}  
    <img src="man/figures/fusen_rstudio_project.png" width="50%" />
- Choose the template
  - Choose the template `teaching` the first time to see how {fusen}
    works,
  - Choose the template `full` the second time to answer most of your
    questions  
    <img src="man/figures/create_fusen_rstudio.png" width="50%" />
  - *Or command line:
    `create_fusen("path/to/new/project", template = "teaching")`*
- Open the “dev/flat_teaching.Rmd” to start setting up the package
- In this flat Rmd template, run the first chunks named `description`
  asking to describe your package and license it
  - They look like these lines of code:

``` r
fill_description(fields = list(Title = "My Awesome Package"))
usethis::use_mit_license("John Doe")
```

- Write your analysis and functionalities following the Rmd template
  - You probably develop them with a few examples and tests
  - *For the first time, you can let the code as is, this is already the
    content for a working package*
- Run the following code to **transform the flat Rmd as an inflated
  package**
  - This will open the vignette created

``` r
fusen::inflate(
  flat_file = "dev/flat_teaching.Rmd",
  vignette_name = "Get started",
  check = TRUE
)
```

- Share it on a website on GitHub

``` r
fusen::init_share_on_github()
```

**That’s it! You built a package! A documented and tested package!**  
**You even have a website for it!**

Let’s test it now:

- Install your package locally

``` r
remotes::install_local()
```

- Restart your R session to clean environment
  - You can restart your RStudio session to let appear the “Build” tab
    panel
- Test functions of your package

``` r
my.package::my_median(1:12)
```

## Description of the Rmd template

As I said earlier, this is all about using the correct split and name
for your chunks.

- Follow the `"dev/flat_template.Rmd"` template to write your
  documentation and build your functions and test your examples.
  - Chunk named `function` gets the code of a function
  - Chunk named `example` gets the code for examples of using the
    function. This will be used for function `@examples` and will be
    kept for the vignette.
    - As chunk names should be unique in the future vignette, you can
      add function names like `example-myfunction`,
      `example-myotherfunction`, …
  - Chunk named `tests` gets the code for unit testing
  - Chunk named `development` gets the code for development purposes,
    usually only used once like {usethis} functions
- **Inflate** the flat Rmd template to transform it as a package with
  functions, unit tests and the current Rmd transformed as a vignette.
  And check.

*Note that the `"flat*.Rmd"` files created with templates `full` and
`teaching` are indeed working examples that can directly be inflated.*

> You can also have a look at
> [{squirrels.fusen}](https://github.com/statnmap/squirrels.fusen) that
> has been built to present the method. Follow the commits:
> <https://github.com/statnmap/squirrels.fusen/commits/main>

## How to maintain a {fusen}? Can I use {fusen} with old-way packages?

There is a dedicated vignette to answer this:
<https://thinkr-open.github.io/fusen/articles/Maintain-packages-with-fusen.html>

- **Option 1**: After a first inflate of your flat file, you can
  continue developing in the “flat_template.Rmd” file, and then inflate
  it using `fusen::inflate_all()`
- **Option 2**: After you’re done with inflating, you can decide to
  deprecate your flat file with `fusen::deprecate_flat_file()` and
  develop in the package files directly, as for any other R package.
  ‘fusen’ has no impact on the structure of a classical package once
  inflated. You can even use `sepuku()` to remove all traces of ‘fusen’.

> Advice: Use git as soon as possible, this will avoid losing your work
> if you made some modifications in the wrong place

> Advice: Show the package structure with
> `fusen::draw_package_structure()` in a “dev/Readme.md” file to help
> developers

## Who is {fusen} for?

If you mind about documentation for your users and tests for your
maintainers, {fusen} is for you. Start writing documentation and tests
while you conceive your functionalities, so that there are directly
available for your users and maintainers.

Indeed, when you write a Rmarkdown file (or a vignette), you create a
documentation for your analysis (or package). When you write some
functions, you check your functions with examples and you maybe write
some unit tests to verify the outputs. This is even more true if you
follow this guide : [‘Rmd first’: When development starts with
documentation](https://rtask.thinkr.fr/when-development-starts-with-documentation/)
Write them all in the same file while you explore the possibilities and
let {fusen} store them in the right place for you!

*{fusen} was first addressed to people who never wrote a package before*
and know how to write a Rmarkdown file. Understanding package
infrastructure and correctly settling it can be frightening. This
package may help them do the first step!

*{fusen} is also addressed to more advanced developers who care about
their users and the sustainability of their products, and are fed up
with switching* between R files, tests files, vignettes while they
prototype their functions. In particular, when changing arguments of a
function, we need to change examples, unit tests in multiple places.
Here, you can do it in one place. No risk to forget one. Think also
about code review: everything related to one function is at the same
place.

## Q&A : All tips and tricks of a {fusen} template

- Can I be lazy in names used?
- Can I knit the content of the flat template ?
- How to declare packages with library() for the future vignette ?
- How to include examples that cannot be run ?
- Document your internal datasets in a function chunk as usual
- How to ignore some chunks ?
- How to create a vignette with different title and Index Entry?
- How not to create a vignette ?
- How to get a pre-filled template for a specific function name ?
- How to inflate multiple flat files ?
- How to store multiple functions in a unique R file ?
- How to read dataset that I usually put in “tests/testthat/” for my
  unit tests?
- Can I load all functions of the current flat file during development
  without having to `inflate()`?
- Can I inflate a Quarto qmd file?
- Can I use {fusen} with {golem}?
- How can I know if R files were created from a flat or not ?
- How can I get rid of everything related to ‘fusen’ in my package ?

=\> See vignette Tips and Tricks:
<https://thinkr-open.github.io/fusen/articles/tips-and-tricks.html>

## Why is this package named {fusen} ?

A fusen is an origami. It is a flat piece of paper that you fold in a
specific way so that at the end, you can magically inflate it to let a
nice box appear.

<img src="man/figures/fusen_seb_crop_small.jpg" width="25%" />

Similarly, the {fusen} package uses a flat Rmd template, that you fill
in a specific way so that at the end, you can magically `inflate()` it
to let a nice package appear.

<details>
<summary>
Click here to fold your {fusen}…
</summary>
<img src="man/figures/fusen-fold-origami.gif" />
</details>

## Contributing

‘fusen’ is partially built using ‘fusen’ itself.  
If you’re not sure on how to contribute using ‘fusen’, you can
contribute as for any other package: a package made with the help of
‘fusen’ does have exactly the same structure as a classical package.  
Knowing that, if you want to contribute to ‘fusen’, you can follow the
instructions in the CONTRIBUTING.md file.

## Acknowledgments

- Thanks to Deemah who asked me to go further ‘Rmd first’ after my
  presentation at useR 2019 in Toulouse: [‘The “Rmd first” method: when
  projects start with
  documentation’](https://github.com/statnmap/prez/blob/master/2019-07_useR_Toulouse.pdf)
  (Video on Youtube: <https://youtu.be/cB1BCxFbhtk>).
- Thanks to @rundel and its package {parsermd} who helped me get back in
  this project with ease : <https://github.com/rundel/parsermd>
  - Now replaced with {lightparser} :
    <https://github.com/ThinkR-open/lightparser>
- Thanks to the [ThinkR team](https://rtask.thinkr.fr) who adopted this
  package for its daily production.

## Code of Conduct

Please note that the {fusen} project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
