# Hide this file from build
usethis::use_build_ignore("devstuff_history.R")
usethis::use_build_ignore("inst/dev")
usethis::use_build_ignore("rsconnect")
usethis::use_git_ignore("docs/")
usethis::use_git_ignore("rsconnect/")
usethis::use_build_ignore("img")
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

# Documentation ----
usethis::use_data_raw()
usethis::use_roxygen_md()
roxygen2md::roxygen2md(scope = "simple")
# _Readme
usethis::use_readme_rmd()
devtools::build_readme()
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

# Doc
usethis::use_github_action("check-standard")
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")
usethis::use_coverage()
usethis::use_build_ignore("_pkgdown.yml")
# usethis::use_github_action(url = "https://github.com/DavisVaughan/extrachecks-html5/blob/main/R-CMD-check-HTML5.yaml")

# Daily dev ----
# See dev/dev_history_daily.R

# Prepare for CRAN ----
# See dev/dev_history_cran.R
