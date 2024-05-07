# Prepare for CRAN

# > Special for {fusen} ====================
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

# /!\ While running these tests manually with `test_file()`,
# do not do anything else in your IDE /!\
# Do not change file open, do not change text in the current file, nothing...
# Unit tests open files interactively and close them
# The focus needs to be kept on what the test is doing
Sys.setenv("NOT_CRAN" = "true")
testthat::test_dir("tests/testthat/")
testthat::test_file("tests/testthat/test-inflate-part1.R")
testthat::test_file("tests/testthat/test-inflate-part2.R")
testthat::test_file("tests/testthat/test-inflate_all.R")
testthat::test_file("tests/testthat/test-inflate_all_utils.R")
testthat::test_file("tests/testthat/test-build_fusen_chunks.R") # Opens files
testthat::test_file("tests/testthat/test-add_flat_template.R")
testthat::test_file("tests/testthat/test-skeleton.R")
testthat::test_file("tests/testthat/test-register_config_file.R") # interactivity
testthat::test_file("tests/testthat/test-rename_flat_file.R")
testthat::test_file("tests/testthat/test-deprecate_flat_file.R")
testthat::test_file("tests/testthat/test-get_package_structure.R")
Sys.setenv("NOT_CRAN" = "false")

# Run line by line
Sys.setenv("FUSEN_TEST_PUBLISH" = "TRUE")
testthat::test_file("tests/testthat/test-init_share_on_github.R") # interactivity
Sys.setenv("FUSEN_TEST_PUBLISH" = "FALSE")
# testthat::test_file("tests/testthat/test-build_fusen_chunks.R")
# Test no output generated in the user files
# pkgload::load_all(export_all = FALSE)
# remotes::install_github("ropensci-review-tools/autotest")

# Run with r-devel using {rig}
#> rig run
devtools::check()

# Update the map of the package
fusen::draw_the_tree()

rmarkdown::render("dev/README.Rmd",
  output_format = "github_document", output_file = "README.md"
)


# > Copy from Prepare-for-cran - https://github.com/ThinkR-open/prepare-for-cran ====================


# Update dependencies in DESCRIPTION
# install.packages('attachment', repos = 'https://thinkr-open.r-universe.dev')
attachment::att_amend_desc()

# Check package coverage
covr::package_coverage()
covr::report()

# Run tests
devtools::test()
testthat::test_dir("tests/testthat/")

# Run examples
devtools::run_examples()

# autotest::autotest_package(test = TRUE)

# Check package as CRAN using the correct CRAN repo
withr::with_options(list(repos = c(CRAN = "https://cloud.r-project.org/")), {
  callr::default_repos()
  rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
})
# devtools::check(args = c("--no-manual", "--as-cran"))

# Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
# All functions must have either `@noRd` or an `@export`.
checkhelper::find_missing_tags()

# Check that you let the house clean after the check, examples and tests
# If you used parallel testing, you may need to avoid it for the next check with `Config/testthat/parallel: false` in DESCRIPTION
all_files_remaining <- checkhelper::check_clean_userspace()
all_files_remaining
# If needed, set back parallel testing with `Config/testthat/parallel: true` in DESCRIPTION

# Check spelling - No typo
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub
devtools::check_rhub()
# List all R-hub platforms:
rhub::platforms()
buildpath <- devtools::build()
rhub::check_on_windows(
  check_args = "--force-multiarch",
  show_status = FALSE,
  path = buildpath
)
rhub::check_on_solaris(show_status = FALSE, path = buildpath)
rhub::check(
  platform = "debian-clang-devel",
  show_status = FALSE,
  path = buildpath
)
rhub::check(
  platform = "debian-gcc-devel",
  show_status = FALSE,
  path = buildpath
)
rhub::check(
  platform = "fedora-clang-devel",
  show_status = FALSE,
  path = buildpath
)
rhub::check(
  platform = "macos-highsierra-release-cran",
  show_status = FALSE,
  path = buildpath
)
rhub::check_for_cran(show_status = FALSE, path = buildpath)

# _win devel CRAN
devtools::check_win_devel()
# _win release CRAN
devtools::check_win_release()
# _macos CRAN
# Need to follow the URL proposed to see the results
devtools::check_mac_release()

# Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
usethis::use_git_ignore("revdep/")
usethis::use_build_ignore("revdep/")

devtools::revdep()
library(revdepcheck)
# In another session because Rstudio interactive change your config:
id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
rstudioapi::terminalKill(id)
# if [Exit Code] is not 0, there is a problem !
# to see the problem: execute the command in a new terminal manually.

# See outputs now available in revdep/
revdep_details(revdep = "pkg")
revdep_summary() # table of results by package
revdep_report()
# Clean up when on CRAN
revdep_reset()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
devtools::release()
