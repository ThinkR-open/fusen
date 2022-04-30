#' Add dev_history.Rmd file that drives package development
#'
#' @description
#'
#' `add_dev_history()` is deprecated. We will soon be totally
#' switching to [add_flat_template()], which is more consistent with development
#' processes
#'
#' `add_dev_history(name = "teaching")` is equivalent to
#' `add_flat_template(template = "teaching")`
#'
#' @param pkg Path where to save file
#' @param overwrite Whether to overwrite existing dev_history.Rmd file
#' @param open Logical. Whether to open file after creation
#' @param dev_dir Name of directory for development Rmarkdown files. Default to "dev".
#' @param name Name of the template file. See [add_flat_template()]
#'
#' @return
#' Create flat Rmd file(s) and return its (their) path
#' @export
#'
#' @examples
#' # Create a new project
#' dummypackage <- tempfile("dummypackage")
#' dir.create(dummypackage)
#'
#' # Add - Use `add_flat_template()` instead
#' add_flat_template(template = "teaching", pkg = dummypackage)
#'
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
add_dev_history <- function(pkg = ".", overwrite = FALSE,
                            open = TRUE, dev_dir = "dev",
                            name = c("full", "minimal", "additional", "teaching")) {
  .Deprecated("add_flat_template", package = "fusen", old = "add_dev_history")

  name <- match.arg(name)

  add_flat_template(
    template = name,
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = name,
    overwrite = overwrite,
    open = open
  )
}
