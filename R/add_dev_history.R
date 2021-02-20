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

  pkg <- normalizePath(pkg)
  if (!dir.exists(dev_dir)) {dir.create(dev_dir)}
  dev_path <- file.path(pkg, dev_dir, "dev_history.Rmd")

  if (file.exists(dev_path) & overwrite == FALSE) {
    n <- length(list.files(dev_dir, pattern = "^dev_history.*[.]Rmd"))
    dev_path <- file.path(pkg, dev_dir, paste0("dev_history_", n + 1, ".Rmd"))
    message(
      "dev_history.Rmd already exists. New dev file is renamed '",
      basename(dev_path), "'. Use overwrite = TRUE, if you want to ",
      "overwrite the existing dev_history.Rmd file, or rename it."
    )
  }
  
  # Change lines asking for pkg name
  lines_template <- readLines(system.file("dev-template.Rmd", package = "fusen"))
  
  lines_template[grepl("<my_package_name>", lines_template)] <-
    gsub("<my_package_name>", basename(pkg), 
         lines_template[grepl("<my_package_name>", lines_template)])
  
  cat(enc2utf8(lines_template), file = dev_path, sep = "\n")

  # .Rbuildignore
  # usethis::use_build_ignore(dev_dir) # Cannot be used outside project
  lines <- c(paste0("^", dev_dir, "$"), "^\\.here$")

  buildfile <- normalizePath(file.path(pkg, ".Rbuildignore"), mustWork = FALSE)
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
  # Files to ignore
  lines <- c("*.html", "*.R")

  gitfile <- normalizePath(file.path(dev_dir, ".gitignore"), mustWork = FALSE)
  if (!file.exists(gitfile)) {
    existing_lines <- ""
  } else {
    existing_lines <- readLines(gitfile, warn = FALSE, encoding = "UTF-8")
  }
  new <- setdiff(lines, existing_lines)
  if (length(new) != 0) {
    all <- c(existing_lines, new)
    cat(enc2utf8(all), file = gitfile, sep = "\n")
  }

  here::set_here(pkg)
  if (isTRUE(open) & interactive()) {usethis::edit_file(dev_path)}
  
  dev_path
}
