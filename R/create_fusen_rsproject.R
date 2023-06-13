#' Create a new fusen project
#'
#' @param path Character. Path where to create the new fusen project.
#' @param template Character. Name of the template to be used among "full", "minimal" and "teaching".
#' @param open Logical. Should the newly created project be opened ?
#' @param overwrite Logical. Allow to overwrite 'dev/' files if path exists.
#' @param with_git Logical. Should git be initialized in the newly created project ?
#'
#' @details
#' See \code{\link{add_flat_template}} for details about the different options for `template`.
#' Template "additional" is not available here as it is meant to be used with an already
#' existing fusen.
#'
#' @importFrom cli cli_alert_warning cli_alert_danger cat_rule cli_alert_success
#' @export
#' @return Path to dev and flat files. Side-effect: Create a new directory to build
#' a package
#' @examples
#' my_path <- tempfile("mypkg")
#' create_fusen(path = my_path, template = "full", open = FALSE)
create_fusen <- function(path,
                         template = c("full", "minimal", "teaching"),
                         open = TRUE,
                         overwrite = FALSE,
                         with_git = FALSE) {
  path <- normalizePath(path, mustWork = FALSE)
  template <- match.arg(template)

  project_name <- get_pkg_name(pkg = path)
  if (project_name != asciify_name(project_name, to_pkg = TRUE)) {
    stop(
      "Please rename your project/directory with: `", asciify_name(project_name, to_pkg = TRUE),
      "` as a package name should only contain letters, numbers and dots."
    )
  }

  if (dir.exists(path)) {
    cli::cli_alert_warning(
      paste(
        "The path:", path, "already exists."
      )
    )
    if (!isTRUE(overwrite)) {
      cli::cli_alert_danger(
        paste(
          "Aborting fusen project creation.",
          "Set `create_fusen(overwrite = TRUE)` to avoid a stop."
        )
      )
      stop("Could not create fusen project", call. = FALSE)
    } else {
      cli::cli_alert_warning(
        paste(
          "You set `create_fusen(overwrite = TRUE)`.",
          "Some files may be overwritten in 'dev/'."
        )
      )
    }
  } else {
    ## Initialize Rstudio project or create directory
    cli::cat_rule(paste0("Creating new directory: ", path))
    usethis::create_project(path, open = FALSE)
    cli::cli_alert_success(paste0("New directory created: ", path))
  }

  ## Eventually initialise git
  if (isTRUE(with_git)) {
    cat_rule("Initializing git repository")
    git_output <- system(
      command = paste("git init", path),
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
    if (git_output != 0) {
      warning("Error initializing git repository")
    } else {
      cli::cli_alert_success("Initialized git repository")
    }
  }

  # Equivalent to local git vaccinate
  local_file_ignore(
    file = file.path(path, ".gitignore"),
    ignores = c(".Rproj.user", ".Rhistory", ".RData", ".DS_Store", ".httr-oauth")
  )

  ## Add dev/flat_template.Rmd in newly created project
  if (template == "minimal") {
    template <- "minimal_package"
  }
  cli::cat_rule(glue::glue("Adding dev/flat_{template}.Rmd"))
  dev_file <- add_flat_template(
    template = template,
    pkg = path,
    overwrite = TRUE,
    open = FALSE,
    dev_dir = "dev"
  )
  cli::cli_alert_success(paste("Added", paste(dev_file, collapse = ", ")))

  ## Open new project if function is called from Rstudio console
  ## Rstudio project wizard will spontaneously open the new project
  if (isTRUE(open) && # !rstudio_project_wizard_context &&
    requireNamespace("rstudioapi") &&
    rstudioapi::isAvailable() &&
    rstudioapi::hasFun("openProject")) {
    cli::cat_rule("Opening new fusen project")
    rstudioapi::openProject(path = path)
    cli::cli_alert_success("Opened new fusen project")
  } else if (isTRUE(open)) {
    utils::browseURL(path)
  }

  return(invisible(dev_file))
}


#' This will only work with Rstudio Project Wizard
#' @noRd
create_fusen_gui <- function(path,
                             template,
                             with_git) {
  create_fusen(
    path = file.path(getwd(), path),
    template = template,
    open = FALSE, # Project opening is done spontaneously by Rstudio Project Wizard
    with_git = with_git
  )
}
