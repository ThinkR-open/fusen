#' Fill DESCRIPTION file of the package
#'
#' @param pkg Path to package
#' @param overwrite Whether to overwrite existing DESCRIPTION
#' @param fields A named list of fields to add to DESCRIPTION, potentially
#'  overriding default values. See \code{\link[usethis]{use_description}} for how you can set
#'  personalized defaults using package options
#'
#' @return
#' Fill DESCRIPTION file with fields. Return path to file.
#' @export
#'
#' @examples
#' # Create a new project
#' dummypackage <- tempfile("dummy.package.desc")
#' dir.create(dummypackage)
#'
#' fill_description(
#'   pkg = dummypackage,
#'   fields = list(
#'     Title = "Build A Package From Rmarkdown file",
#'     Description = paste(
#'       "Use Rmd First method to build your package.",
#'       "Start your package with documentation.",
#'       "Everything can be set from a Rmarkdown file in your project."
#'     ),
#'     `Authors@R` = c(
#'       person("John", "Doe",
#'         email = "john@email.me",
#'         role = c("aut", "cre"), comment = c(ORCID = "0000-0000-0000-0000")
#'       )
#'     )
#'   )
#' )
#'
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
fill_description <- function(pkg = ".", fields, overwrite = FALSE) {
  # old <- setwd(pkg)
  # on.exit(setwd(old))

  path <- normalizePath(pkg)

  project_name <- get_pkg_name(pkg = pkg)
  clean_pkg_name <- asciify_name(project_name, to_pkg = TRUE)
  if (project_name != clean_pkg_name) {
    warning(
      "Your package was renamed: `", clean_pkg_name,
      "` as a package name should only contain letters, numbers and dots."
    )
  }

  desc_file <- file.path(path, "DESCRIPTION")

  if (capwords(fields[["Title"]]) != fields[["Title"]]) {
    fields[["Title"]] <- capwords(fields[["Title"]])
    cli::cli_alert_warning(paste("Title was modified to 'Title Case'."))
  }

  if (!is.null(fields[["Description"]]) && !grepl("[.]$", fields[["Description"]])) {
    fields[["Description"]] <- paste0(fields[["Description"]], ".")
    cli::cli_alert_warning(paste("Description field should be a sentence. A dot was added at the end."))
  }

  if (file.exists(desc_file) & !isTRUE(overwrite)) {
    stop("DESCRIPTION already exists. Set overwrite = TRUE to overwrite.")
  }
  # usethis::use_description(fields = fields)

  fields_new <- usethis::use_description_defaults(
    package = clean_pkg_name, # basename(path),
    roxygen = TRUE,
    fields = fields
  )

  desc <- desc::desc(text = glue::glue("{names(fields_new)}: {fields_new}"))

  desc$write(file = desc_file)

  desc_file
}


#' To Title Case
#'
#' Example from base::tolower()
#' @noRd
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
