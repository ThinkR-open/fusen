#' Create a new fusen project
#'
#' @param path Character. Path where to create the new fusen project.
#' @param name Character. Name of the template to be used among "full", "minimal" and "teaching".
#' @param open Logical. Should the newly created project be opened ?
#' @param overwrite Logical. Allow to overwrite 'dev/' files if path exists.
#' @param with_git Logical. Should git be initialised in the newly created project ?
#'
#' @details
#' See \code{\link{add_dev_history}} for details about the different options for `name`.
#' Template "additional" is not available here as it is meant to be used with an already
#' existing fusen.
#'
#' @importFrom cli cli_alert_warning cli_alert_danger cat_rule cli_alert_success
#' @export
#' @examples
#' my_path <- tempfile("mypkg")
#' create_fusen(path = my_path, name = "full", open = FALSE)
create_fusen <- function(
  path,
  name = c("full", "minimal", "teaching"),
  open = TRUE,
  overwrite = FALSE,
  with_git = FALSE
) {

  path <- normalizePath(path, mustWork = FALSE)
  name <- match.arg(name)

  if (dir.exists(path)){
    cli::cli_alert_warning(
      paste(
        "The path:", path, "already exists."#,
        # "Continuing will overwrite dev/dev_history.Rmd.\n",
        # "Are you sure you want to continue ?"
      )
    )
    # overwrite <- utils::menu(c("Yes", "No")) == 1
    if (!isTRUE(overwrite)) {
      cli::cli_alert_danger(
        paste(
          "Aborting fusen project creation.",
          "Set `create_fusen(overwrite = TRUE)` to avoid a stop."))
      stop("Could not create fusen project", call. = FALSE)
    } else {
      cli::cli_alert_warning(
        paste(
          "You set `create_fusen(overwrite = TRUE)`.",
          "Some files may be overwritten in 'dev/'.")
      )
    }
  } else {
    ## Initialize Rstudio project or create directory
    cli::cat_rule(paste0("Creating new directory: ", path))
    usethis::create_project(path, open = FALSE)
    cli::cli_alert_success(paste0("New directory created: ", path))
  }

  ## Eventually initialise git
  if (with_git) {
    cat_rule("Initializing git repository")
    git_output <- system(
      command = paste("git init", path),
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
    if (git_output) {
      cli::cli_alert_warning("Error initializing git repository")
    } else {
      cli::cli_alert_success("Initialized git repository")
    }
  }

  ## Add dev/dev_history.Rmd in newly created project
  cli::cat_rule("Adding dev/dev_history.Rmd")
  add_dev_history(
    pkg = path,
    overwrite = TRUE,
    open = FALSE,
    dev_dir = "dev",
    name = name
  )
  cli::cli_alert_success("Added dev/dev_history.Rmd")

  ## Open new project if function is called from Rstudio console
  ## Rstudio project wizard will spontaneously open the new project
  if (isTRUE(open) && #!rstudio_project_wizard_context &&
      requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("openProject")) {
    cli::cat_rule("Opening new fusen project")
    rstudioapi::openProject(path = path)
    cli::cli_alert_success("Opened new fusen project")
  } else if (isTRUE(open)) {
    utils::browseURL(path)
  }

  return(invisible(path))
}


#' This will only work with Rstudio Project Wizard
#' @noRd
create_fusen_gui <- function(
  path,
  name,
  with_git
) {

  create_fusen(
    path = file.path(getwd(), path),
    name = name,
    open = FALSE, # Project opening is done spontaneously by Rstudio Project Wizard
    with_git = with_git
  )

}
