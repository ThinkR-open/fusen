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
Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = 0) # If clock not available
testthat::test_dir("tests/testthat/") # interactivity - answer yes
testthat::test_file("tests/testthat/test-inflate-part1.R")
testthat::test_file("tests/testthat/test-inflate-part2.R")
testthat::test_file("tests/testthat/test-inflate_all.R")
testthat::test_file("tests/testthat/test-inflate_all_utils.R")
testthat::test_file("tests/testthat/test-build_fusen_chunks.R") # Opens files
testthat::test_file("tests/testthat/test-add_flat_template.R")
testthat::test_file("tests/testthat/test-skeleton.R")
testthat::test_file("tests/testthat/test-register_config_file.R") # interactivity - answer yes
testthat::test_file("tests/testthat/test-rename_flat_file.R")
testthat::test_file("tests/testthat/test-deprecate_flat_file.R")
testthat::test_file("tests/testthat/test-get_package_structure.R")
testthat::test_file("tests/testthat/test-sepuku.R") # interactivity - answer yes
Sys.setenv("NOT_CRAN" = "false")

# Test on Windows if directory is like `D:\\2025` messes with regex
if (.Platform$OS.type == "windows") {
  tmpdir_orig <- tempdir()
  tmpdir_new <- "D:\\2025"
  Sys.setenv(TMPDIR = tmpdir_new) # Windows only
  file.copy(tmpdir_orig, tmpdir_new, recursive = TRUE)
  unlink(tempdir(), recursive = TRUE)
  tempdir(check = TRUE)
  # To manage temp vscode config files
  dir.create(tmpdir_orig, recursive = TRUE)
  fs::dir_copy(
    file.path(tmpdir_new, basename(tmpdir_orig), "vscode-R"),
    tmpdir_orig
  )
  # That should be ok
  tempdir()
  tempfile()
  # Run tests
  testthat::test_dir("tests/testthat/")
  # Back to normal
  Sys.setenv(TMPDIR = tmpdir_orig) # Windows only
  unlink(file.path(tmpdir_new, basename(tmpdir_orig)), recursive = TRUE)
  unlink(list.files(tmpdir_new, pattern = "Rtmp", full.names = TRUE), recursive = TRUE)
  tempdir(check = TRUE)
  tempdir()
}

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
suppressMessages(devtools::test()) # interactivity

# Update the tree structure of the package
fusen::draw_package_structure()

# Update Readmes
rmarkdown::render(
  "dev/README.Rmd",
  output_format = "github_document",
  output_file = "README.md"
)

rmarkdown::render(
  "README.Rmd",
  output_format = "github_document",
  output_file = "README.md"
)
file.remove("README.html")

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
rhub::rhub_setup(overwrite = TRUE)
rhub::rhub_doctor()
rhub::rhub_platforms()
rhub::rhub_check(gh_url = "https://github.com/ThinkR-open/fusen")

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
