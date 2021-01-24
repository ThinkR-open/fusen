#' Fill DESCRIPTION file of the package
#'
#' @param pkg Path to package
#' @param overwrite Whether to overwrite existing DESCRIPTION
#' @param fields A named list of fields to add to DESCRIPTION, potentially
#'  overriding default values. See \code{\link[usethis]{use_description}} for how you can set
#'  personalized defaults using package options
#'
#' @return
#' FIll DESCRIPTION file with fields. Return path to file.
#' @export
#'
#' @examples
#' # Create a new project
#' tmpdir <- tempdir()
#' dummypackage <- file.path(tmpdir, "dummypackage")
#' dir.create(dummypackage)
#' browseURL(dummypackage)
#' 
#' fill_description(
#'   pkg = dummypackage,
#'   fields = list(
#'     Title = "Build A Package From Rmarkdown file",
#'     Description = "Use Rmd First method to build your package. Start your package with documentation. Everything can be set from a Rmd file in your project.",
#'     `Authors@R` = c(
#'       person("Sebastien", "Rochette", email = "sebastien@thinkr.fr", role = c("aut", "cre"), comment = c(ORCID = "0000-0002-1565-9313")),
#'       person(given = "ThinkR", role = "cph")
#'     )
#'   )
#' )
#' 
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
fill_description <- function(pkg = ".", fields, overwrite = FALSE) {
  old <- setwd(pkg)
  on.exit(setwd(old))

  path <- normalizePath(pkg)

  desc_file <- file.path(path, "DESCRIPTION")

  if (file.exists(desc_file) & !isTRUE(overwrite)) {
    stop("DESCRIPTION already exists. Set overwrite = TRUE to overwrite.")
  }
  # usethis::use_description(fields = fields)

  fields_new <- usethis::use_description_defaults(
    package = basename(path),
    roxygen = TRUE,
    fields = fields
  )
  desc <- desc::desc(text = glue::glue("{names(fields_new)}: {fields_new}"))

  desc$write(file = desc_file)
  desc_file
}
