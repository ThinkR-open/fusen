# Hide this file from build
usethis::use_build_ignore("devstuff_history.R")
usethis::use_build_ignore("inst/dev")
usethis::use_build_ignore("rsconnect")
usethis::use_git_ignore("docs/")
usethis::use_git_ignore("rsconnect/")
usethis::use_build_ignore("img")
# usethis::create_package(".")
usethis::use_git_ignore("docs")
usethis::use_git_ignore("pkgdown")
usethis::use_git_ignore("cran-comments.md")
usethis::use_build_ignore("docs")
usethis::use_build_ignore("pkgdown")

usethis::use_lifecycle_badge("Experimental")
usethis::git_vaccinate()
usethis::use_testthat(edition = 3)

# description ----
library(desc)
unlink("DESCRIPTION")
my_desc <- description$new("!new")
my_desc$set_version("0.0.0.9000")
my_desc$set(Package = "fusen")
my_desc$set(Title = "Build A Package From Rmarkdown file")
my_desc$set(Description = "Use Rmd First method to build your package. Start your package with documentation. Everything can be set from a Rmarkdown file in your project.")
my_desc$set(
  "Authors@R",
  'c(
  person("Sebastien", "Rochette", email = "sebastien@thinkr.fr", role = c("aut", "cre"), comment = c(ORCID = "0000-0002-1565-9313")),
  person(given = "ThinkR", role = "cph")
)'
)
my_desc$set("VignetteBuilder", "knitr")
my_desc$del("Maintainer")
my_desc$del("URL")
my_desc$del("BugReports")
my_desc$write(file = "DESCRIPTION")

# Licence ----
usethis::use_mit_license("ThinkR")
# usethis::use_gpl3_license("ThinkR")

# Pipe ----
usethis::use_roxygen_md()
usethis::use_pipe()

# Package quality ----

# _Tests
usethis::use_testthat()
usethis::use_test("app")

# _CI
usethis::use_git()
usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()
# usethis::use_coverage()

# _rhub
# rhub::check_for_cran()


# Documentation ----
usethis::use_data_raw()
usethis::use_roxygen_md()
roxygen2md::roxygen2md(scope = "simple")
# _Readme
usethis::use_readme_rmd()
# _News
usethis::use_news_md()
# _Vignette
thinkridentity::create_vignette_thinkr("aa-data-get-started")
usethis::use_vignette("ab-model")
devtools::build_vignettes()
fusen::add_flat_template("add", open = TRUE)
# contributing
usethis::use_tidy_contributing()
usethis::use_build_ignore("CONTRIBUTING.md")

# _Pkgdown
chameleon::build_pkgdown(
  # lazy = TRUE,
  yml = system.file("pkgdown/_pkgdown.yml", package = "thinkridentity"),
  favicon = system.file("pkgdown/favicon.ico", package = "thinkridentity"),
  move = FALSE, clean_before = TRUE, clean_after = FALSE
)

# Doc
usethis::use_github_action("check-standard")
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")
usethis::use_coverage()
usethis::use_build_ignore("_pkgdown.yml")
# usethis::use_github_action(url = "https://github.com/DavisVaughan/extrachecks-html5/blob/main/R-CMD-check-HTML5.yaml")

# Inflates ----
# testthat::test_file("tests/testthat/test-build_fusen_chunks.R")
fusen::inflate_all()
fusen::inflate_all(args = c("--no-manual", "--no-tests"))
fusen::inflate_all_no_check()
fusen::inflate_all_no_check(stylers = function() {
  styler::style_pkg()
  styler::style_dir("dev")
})

# Clean style ----
styler::style_pkg()
styler::style_file(list.files("dev", pattern = "[.](Rmd|qmd|rmd)$", full.names = TRUE))

# Dependencies ----
# devtools::install_github("ThinkR-open/attachment")
# attachment::att_from_namespace()
attachment::att_amend_desc(
  pkg_ignore = c(
    "testthat", "dummypackage", "rstudioapi",
    "knitr", "rmarkdown", "R6", "gert"
  ),
  extra.suggests = c(
    "testthat", "pkgload", "rstudioapi",
    "rmarkdown", "knitr", "gert", "styler"
  ),
  # "MASS", "lattice", "Matrix")
  update.config = TRUE # attachment >= 0.4.0.
)
# attachment::create_dependencies_file()



# Description and Bibliography
chameleon::create_pkg_desc_file(out.dir = "inst", source = c("archive"), to = "html")
thinkridentity::create_pkg_biblio_file_thinkr()

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
# _Update template Rmd ----
skeleton_dir <- tempfile()
dir.create(skeleton_dir)
# When opening, verify that "skeleton" is written in the correct places
the_flat <- fusen::add_additional(
  pkg = skeleton_dir,
  dev_dir = "dev",
  flat_name = "skeleton",
  open = TRUE
)
file.copy(
  the_flat,
  here::here("inst/rmarkdown/templates/additional/skeleton/skeleton.Rmd"),
  overwrite = TRUE
)
unlink(skeleton_dir, recursive = TRUE)

# _Check in interactive test-inflate for templates and Addins ----
pkgload::load_all()

Sys.setenv("NOT_CRAN" = "true")
testthat::test_dir("tests/testthat/")
testthat::test_file("tests/testthat/test-inflate-part1.R")
testthat::test_file("tests/testthat/test-inflate-part2.R")
testthat::test_file("tests/testthat/test-inflate_all.R")
testthat::test_file("tests/testthat/test-add_flat_template.R")
testthat::test_file("tests/testthat/test-skeleton.R")
Sys.setenv("NOT_CRAN" = "false")

# Run line by line
Sys.setenv("FUSEN_TEST_PUBLISH" = "TRUE")
testthat::test_file("tests/testthat/test-init_share_on_github.R")
Sys.setenv("FUSEN_TEST_PUBLISH" = "FALSE")
# testthat::test_file("tests/testthat/test-build_fusen_chunks.R")
# Test no output generated in the user files
# pkgload::load_all(export_all = FALSE)
# remotes::install_github("ropensci-review-tools/autotest")

# Run examples in interactive mode too
devtools::run_examples()

# Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
devtools::check(args = c("--no-manual", "--as-cran"))

# Check content
# remotes::install_github("ThinkR-open/checkhelper")
tags <- checkhelper::find_missing_tags()
View(tags$functions)

# Remettre: Config/testthat/parallel: false dans DESCRIPTION
out <- checkhelper::check_clean_userspace(pkg = ".")
out
checkhelper::check_as_cran()
# Remettre: Config/testthat/parallel: true dans DESCRIPTION

# Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# remotes::install_github("r-lib/urlchecker")
urlchecker::url_check()
urlchecker::url_update()

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[2])

# check on other distributions
# _rhub
devtools::check_rhub()
rhub::platforms()
rhub::check_on_windows(check_args = "--force-multiarch", show_status = FALSE)
rhub::check_on_solaris(show_status = FALSE)
rhub::check(platform = "debian-clang-devel", show_status = FALSE)
rhub::check(platform = "debian-gcc-devel", show_status = FALSE)
rhub::check(platform = "fedora-clang-devel", show_status = FALSE)
rhub::check(platform = "macos-m1-bigsur-release", show_status = FALSE)
rhub::check_for_cran(show_status = FALSE)

# Run locally in Docker
# docker pull rhub/debian-clang-devel
# docker run -ti rhub/debian-clang-devel bash
# docker run -v /mnt/Data/github/ThinkR-open/fusen:/home/root/toto -ti rhub/debian-clang-devel bash
rhub::local_check_linux(image = "rhub/debian-clang-devel")
rhub::local_check_linux(image = "rhub/fedora-clang-devel")
# a55df815-38f2-4854-a3bc-29cdcac878cc-2

rstudioapi::navigateToFile(system.file(package = "rhub", "bin", "rhub-linux-docker.sh"))
# docker container start -i 7181196d-bc3c-4fc8-a0e8-dc511150335d-2
# docker exec -it 7181196d-bc3c-4fc8-a0e8-dc511150335d-2 bash
# https://www.thorsten-hans.com/how-to-run-commands-in-stopped-docker-containers/
# /opt/R-devel/bin/R


rhub::check(platform = "windows-x86_64-devel", show_status = FALSE)

# _win devel
devtools::check_win_devel()
devtools::check_win_release()
# remotes::install_github("r-lib/devtools")
devtools::check_mac_release()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
devtools::release()

# Back to dev
usethis::use_version(which = c("patch", "minor", "major", "dev")[4])
