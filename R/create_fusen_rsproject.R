

#' Create a new fusen project
#'
#' @param path path where to create the new fusen project.
#' @param name name of the template to be used among "full", "minimal" and "teaching".
#' @param open logical, should the newly created Rstudio project be opened.
#'
#' @details
#' See \code{\link{add_dev_history}} for details about the different options for `name`.
#' Template "additional" is not available here as it is meant to be used with an already
#' existing fusen.
#'
#' @importFrom cli cli_alert_warning cli_alert_danger cat_rule cli_alert_success
#' @noRd
create_fusen <- function(
  path,
  name = c("full", "minimal", "teaching"),
  open = TRUE
) {

  # If the function is triggered by Rstudio project wizard
  # - The path in "Create project as subdirectory of:" is set to working directory
  # - The value of "Directory name:" is passed as the path argument of create_fusen()
  is_rstudio_project_wizard_context <- function(path) {
    dirname(path) == "." && basename(path) == path
  }

  rstudio_project_wizard_context <- is_rstudio_project_wizard_context(path)
  if ( rstudio_project_wizard_context ) {
    # In that case we need to reconstruct the entire path as
    path_fusen <- file.path(getwd(), path)
  } else {
    path <- path.expand(path)
    path_fusen <- path
  }


  if (dir.exists(path)){
    cli::cli_alert_warning(
      paste(
        "The path", path, "already exists.\n",
        "Continuing will overwrite dev/dev_history.Rmd.\n",
        "Are you sure you want to continue ?"
      )
    )
    overwrite <- utils::menu(c("Yes", "No")) == 1
    if (!overwrite){
      cli::cli_alert_danger("Aborting fusen project creation")
      stop("Could not create fusen project", call. = FALSE)
    }
  }

  ## Initialize Rstudio project or create directory
  if ( rstudioapi::isAvailable() ) {
    cli::cat_rule("Rstudio project initialisation")
    rstudioapi::initializeProject(path = path)
    cli::cli_alert_success("Rstudio project initialised")
  } else {
    cli::cat_rule(paste0("Creating new directory: ", path))
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
    cli::cli_alert_success(paste0("New directory created: ", path))
  }

  ## Add dev/dev_history.Rmd in newly created project
  cli::cat_rule("Adding dev/dev_history.Rmd")
  add_dev_history(
    pkg = path_fusen,
    overwrite = TRUE,
    open = FALSE,
    dev_dir = "dev",
    name = name
  )
  cli::cli_alert_success("Added dev/dev_history.Rmd")

  ## Open new project if function is called from Rstudio console
  ## Rstudio project wizard will spontaneously open the new project
  if (open & !rstudio_project_wizard_context & rstudioapi::isAvailable() ) {
    cli::cat_rule("Opening new fusen project")
    rstudioapi::openProject(path = path)
    cli::cli_alert_success("Opened new fusen project")
  }


}


# Minimal version of the function
# This will only work with Rstudio Project Wizard
create_fusen_gui <- function(
  path,
  name = c("full", "minimal", "additional", "teaching")
) {
  # browser()
  rstudioapi::initializeProject(path = path)
  setwd(file.path(getwd(), path))
  add_dev_history(
    overwrite = FALSE,
    open = FALSE,
    dev_dir = "dev",
    name = name
  )
}
