# Daily dev - Inflates ----
# testthat::test_file("tests/testthat/test-build_fusen_chunks.R")
# install.packages("grkstyle", repos = "https://gadenbuie.r-universe.dev")
fusen::inflate_all()
fusen::inflate_all(args = c("--no-manual", "--no-tests"))
fusen::inflate_all_no_check()
fusen::inflate_all_no_check(stylers = function() {
  grkstyle::grk_style_pkg()
  grkstyle::grk_style_file(
    list.files("dev", pattern = "[.](Rmd|qmd|rmd)$", full.names = TRUE)
  )
})
fusen::inflate_all(codecov = TRUE)

# Clean style ----
grkstyle::grk_style_pkg()
grkstyle::grk_style_file(
  list.files("dev", pattern = "[.](Rmd|qmd|rmd)$", full.names = TRUE)
)

# Dependencies ----
# devtools::install_github("ThinkR-open/attachment")
# attachment::att_from_namespace()
attachment::att_amend_desc(
  pkg_ignore = c(
    "testthat",
    "dummypackage",
    "rstudioapi",
    "knitr",
    "rmarkdown",
    "R6",
    "gert",
    "covr"
  ),
  extra.suggests = c(
    "testthat",
    "pkgload",
    "rstudioapi",
    "rmarkdown",
    "knitr",
    "gert",
    "styler",
    "covr"
  ),
  # "MASS", "lattice", "Matrix")
  update.config = TRUE # attachment >= 0.4.0.
)
# attachment::create_dependencies_file()


# Update lightparser
remotes::install_github("ThinkR-open/lightparser")
# Utils for dev ----
devtools::install(upgrade = "never")
# devtools::load_all()
devtools::test()
# testthat::test_file("tests/testthat/test-build_fusen_chunks.R")
devtools::check(vignettes = TRUE)
rcmdcheck::rcmdcheck()
rcmdcheck::rcmdcheck(args = "--as-cran")
# ascii
stringi::stri_trans_general("é", "hex")
stringi::stri_escape_unicode("é") # Remember to remove a "\""

# Review PR ----
usethis::pr_fetch(41)
usethis::pr_push()
usethis::pr_finish(41)
# Clean names for vignettes
# name <- "# y  _ p n@ é ! 1"
#   name <- "# y  _ p n@ \u00E9 ! 1"
# stringi::stri_trans_general("é", "hex")
# # name <- "get-started"
# cleaned_name <- gsub("^-|-$", "",
#                      gsub("-+", "-",
#                           gsub("-_|_-", "-",
#                           gsub("[^([:alnum:]*_*-*)*]", "-", name))))
# grepl("^[[:alpha:]][[:alnum:]_-]*$", cleaned_name)
# # asciify from {usethis} usethis:::asciify()
# cleaned_name <- gsub("[^a-zA-Z0-9_-]+", "-", cleaned_name)
# usethis::use_vignette(name = cleaned_name, title = name)


# Prepare for CRAN ----
# See dev/dev_history_cran.R
