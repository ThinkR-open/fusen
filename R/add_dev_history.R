#' Add dev_history.Rmd file that drives package development
#'
#' @param pkg Path where to save file
#' @param overwrite Whether to overwrite existing dev_history.Rmd file
#' @param open Logical. Whether to open file after creation
#' @param dev_dir Name of directory for development Rmd files. Default to "dev".
#'
#' @return
#' Create a dev_history.Rmd file and return its path
#' @export
#'
#' @examples
#' # Create a new project
#' tmpdir <- tempdir()
#' dummypackage <- file.path(tmpdir, "dummypackage")
#' dir.create(dummypackage)
#' browseURL(dummypackage)
#' 
#' # Add
#' add_dev_history(pkg = dummypackage)
#' 
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
add_dev_history <- function(pkg = ".", overwrite = FALSE,
                            open = TRUE, dev_dir = "dev") {
  old <- setwd(pkg)
  on.exit(setwd(old))

  pkg_path <- normalizePath(pkg)
  if (!dir.exists(dev_dir)) {dir.create(dev_dir)}
  dev_path <- file.path(pkg_path, dev_dir, "dev_history.Rmd")

  if (file.exists(dev_path) & overwrite == FALSE) {
    n <- length(list.files(dev_dir, pattern = "^dev_history"))
    dev_path <- file.path(pkg_path, dev_dir, paste0("dev_history_", n + 1, ".Rmd"))
    message(
      "dev_history.Rmd already exists. New dev file is renamed '",
      basename(dev_path), "'. Use overwrite = TRUE, if you want to ",
      "overwrite the existing dev_history.Rmd file, or rename it."
    )
  }
  file.copy(
    system.file("dev-template.Rmd", package = "fusen"),
    dev_path
  )

  # .Rbuildignore
  # Files to ignore
  # lines <- paste0("^", gsub(".", "\\.", basename(dev_path), fixed = TRUE), "$")
  lines <- dev_dir

  buildfile <- normalizePath(file.path(pkg_path, ".Rbuildignore"), mustWork = FALSE)
  if (!file.exists(buildfile)) {
    existing_lines <- ""
  } else {
    existing_lines <- readLines(buildfile, warn = FALSE, encoding = "UTF-8")
  }
  new <- setdiff(lines, existing_lines)
  if (length(new) != 0) {
    all <- c(existing_lines, new)
    cat(enc2utf8(all), file = buildfile, sep = "\n")
  }

  # Add a gitignore file in dev_dir
  if (!file.exists(file.path(dev_dir, ".gitignore"))) {
    cat(enc2utf8(c("*.html", "*.R")), sep = "\n", file = file.path(dev_dir, ".gitignore"))
  }
  
  dev_path
}
