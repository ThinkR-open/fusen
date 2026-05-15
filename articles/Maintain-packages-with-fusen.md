# Maintain packages with {fusen}

``` r

library(fusen)
```

## Daily use of {fusen}

### Add a new set of chunks in the current Rmd

Use the Addin “Add {fusen} chunks”

- Name the chunks using function name planned to be created
- Decide if this function is exported to the user or not

### Create a new flat Rmd template

To add a new family of functions, create a new flat Rmd template

``` r

add_flat_template(template = "add")
# or directly
add_additional()
```

## How to maintain a {fusen}? Can I use {fusen} with old-way packages ?

After you
[`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
the “flat_template.Rmd”, your code appears twice in the package. In the
“flat_template.Rmd” itself and in the correct place for it to be a
package. Take it as a documentation for the other developers.  
Maintaining such a package requires a choice:

- **Option 1**: Modifications are only added to the “flat_template.Rmd”
  file, which then is inflated to update all packages files
- **Option 2**: Modifications are realized in the package files directly
  as for any other package, and the “flat_template.Rmd” file must be
  deprecated using
  [`deprecate_flat_file()`](https://thinkr-open.github.io/fusen/reference/deprecate_flat_file.md).

Your first
[`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
may not directly work as expected as with any R code that you write. In
this case, you can continue to implement your functionality using
**Option 1**.

> Advice 1: Use Option 1 until you find it too complicated to be used !
> I assure you, you will find the moment when you say : ok this is not
> possible anymore…

> Advice 2: Use git as soon as possible, this will avoid losing your
> work if you made some modifications in the wrong place

> Advice 3: Create a Readme.Rmd in the “dev/” directory with a chunk
> having
> [`fusen::draw_package_structure()`](https://thinkr-open.github.io/fusen/reference/get_package_structure.md)
> and knit it. This will help to understand the structure of your
> package and see what is inflated or not. Have a look at the {fusen}
> package itself on GitHub to see how it looks like.

### Option 1: Continue with the “flat_template.Rmd”

- (+) You are encouraged to start with documenting and testing your
  package before you start coding, which is generally a good practice
  for prototyping.
- (+) This does not require to fully understand the package structure
  and files to continue building your package, and you continue to
  develop in a unique file
- (-) You need to pay attention to checking and debugging tools that may
  direct you to the R file directly. This requires to pay attention and
  always be sure you are modifying code in the flat template file, to be
  inflated.
- (-) This may trouble co-developers who already built packages

=\> {fusen} itself is built as is. Each modification is added to the
dedicated dev_history file and then inflated =\> The tree structure in
the “dev/Readme.md” file is very useful to understand what is inflated
or not, and to see the structure of the package

### Option 2: Maintain like a classical package

- (+) You can use dedicated checking and debugging tools as is, in
  particular in RStudio. There are built to direct you as quickly as
  possible to the source of the problem
- (+) This allows collaboration with more advanced developers who are
  used to debug in the package structure directly
- (-) You need to remember that you need to care about your users and
  maintainers, and that you need to document and test your code as soon
  as possible, in a different place than the code itself.
- (-) This requires to understand the structure and files of a package
  and how they interact each other, and be able to jump from one file to
  the other, in the correct folder. This may drives you lazy to continue
  documenting and testing your modifications

You can use
[`deprecate_flat_file()`](https://thinkr-open.github.io/fusen/reference/deprecate_flat_file.md)
to protect the original flat template file, so that you never use it
again. Inflated files are also unprotected, so that you can modify them
directly.

=\> This is the way I add new functionalities in packages that started
in the old way, and in specific cases where inflating is now too
complicated, like for function
[`inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.md)
and all its unit tests in this very {fusen} package.

## What about packages already built the old way ?

The “flat_template.Rmd” template only modifies files related to
functions presented inside the template. **This does not remove or
modify previous functions, tests or vignettes, provided that names are
different.**

- {fusen} itself was started in the classical way before having enough
  functions to be able to build a package from itself. This does not
  prevent me at all to use {fusen} to build himself now !
- If you want to modify existing functionalities, you will need to
  continue maintain your already-built package in the classical way
- If you want to add new functionalities, correctly documented and
  tested, you can use {fusen}. This will not delete previous work.
- Use the “Option 2” above to continue after the development of your
  functionality

### Let’s try to convince package developers with an example

- Install {fusen} : `install.packages("fusen")`
- Open a project for **one of your already existing package**
  - Commit your previous state if you are afraid of {fusen}
  - If you are not confident enough to try it on an existing package,
    then create a new package while following the guide in [“How to use
    fusen”](https://thinkr-open.github.io/fusen/articles/How-to-use-fusen.html)
- Run in the Console : `fusen::add_flat_template("add")`
  - A Rmd file appears in “dev/flat_additional.Rmd”. Open it.
- Write a new function in the `function` chunk. For instance:

``` r

#' My median
#'
#' @param x Vector of Numeric values
#' @inheritParams stats::median
#'
#' @return
#' Median of vector x
#' @export
#'
#' @examples
my_median <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("x should be numeric")
  }
  stats::median(x, na.rm = na.rm)
}
```

- Add a corresponding example in the `example` chunk.
  - If you’re looking for an equivalent of `load_all()` but for all
    functions inside this flat file before inflating, you can use
    [`fusen::load_flat_functions()`](https://thinkr-open.github.io/fusen/reference/load_flat_functions.md)

``` r

my_median(1:12)
```

- Add a corresponding unit test in the `test` chunk. For instance:

``` r

test_that("my_median works properly and show error if needed", {
  expect_true(my_median(1:12) == 6.5)
  expect_error(my_median("text"))
})
```

- Run the command of the last chunk of the flat file:
  `fusen::inflate(flat_file = "dev/flat_additional.Rmd")`
  - This will run {attachment} behind the scene and may modify the list
    of dependencies in the DESCRIPTION file accordingly. Use
    `fusen::inflate(flat_file = "dev/flat_additional.Rmd", document = FALSE)`
    to avoid that.
  - This will also run
    [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
    Use
    `fusen::inflate(flat_file = "dev/flat_additional.Rmd", check = FALSE)`
    to avoid that.
  - This will create a new vignette in your package. You may want to
    avoid this with
    `fusen::inflate(flat_file = "dev/flat_additional.Rmd", vignette_name = NA)`

**That’s it!**  
You added a new function in your package, along with example, test and a
new vignette:

- R/my_median.R
- tests/testthat/test-my_median.R
- vignettes/get-started.Rmd

## Compare a classical way of building packages with the {fusen} way

[TABLE]
