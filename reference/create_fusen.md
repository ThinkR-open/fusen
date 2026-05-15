# Create a new fusen project

Create a new fusen project

## Usage

``` r
create_fusen(
  path,
  template = c("full", "minimal", "teaching", "dev_history"),
  flat_name = template,
  open = TRUE,
  overwrite = FALSE,
  with_git = FALSE
)
```

## Arguments

- path:

  Character. Path where to create the new fusen project.

- template:

  Character. Name of the template to be used among "full", "minimal",
  "teaching" and "dev_history".

- flat_name:

  Character. Filename of the flat file created. This is also used to
  name the first function of the file in `minimal` template.

- open:

  Logical. Should the newly created project be opened ?

- overwrite:

  Logical. Allow to overwrite 'dev/' files if path exists.

- with_git:

  Logical. Should git be initialized in the newly created project ?

## Value

Path to dev and flat files. Side-effect: Create a new directory to build
a package

## Details

See
[`add_flat_template`](https://thinkr-open.github.io/fusen/reference/add_flat_template.md)
for details about the different options for `template`. Template
"additional" is not available here as it is meant to be used with an
already existing 'fusen'.

## Examples

``` r
my_path <- tempfile("mypkg")
create_fusen(path = my_path, template = "full", open = FALSE)
#> ── Creating new directory: /tmp/Rtmp005QYy/mypkg1b68d341b5 ─────────────────────
#> ✔ Creating /tmp/Rtmp005QYy/mypkg1b68d341b5/.
#> ✔ Setting active project to "/tmp/Rtmp005QYy/mypkg1b68d341b5".
#> ✔ Creating R/.
#> ✔ Writing a sentinel file .here.
#> ☐ Build robust paths within your project via `here::here()`.
#> ℹ Learn more at <https://here.r-lib.org>.
#> ✔ Setting active project to "<no active project>".
#> ✔ New directory created: /tmp/Rtmp005QYy/mypkg1b68d341b5
#> ✔ Added /tmp/Rtmp005QYy/mypkg1b68d341b5/dev/flat_full.Rmd, /tmp/Rtmp005QYy/mypkg1b68d341b5/dev/0-dev_history.Rmd
```
