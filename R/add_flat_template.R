# do not edit by hand

#' Add flat Rmd file that drives package development
#'
#' @param template Name of the template to use. See details.
#' @param pkg Path where to save file
#' @param overwrite Whether to overwrite existing dev_history.Rmd file
#' @param open Logical. Whether to open file after creation
#' @param dev_dir Name of directory for development Rmarkdown files. Default to "dev".
#' @param flat_name Name of the file to write in dev.
#'
#' @importFrom tools file_path_sans_ext
#'
#' @details
#' Choose `template` among the different templates available:
#'
#' - "full": the full template with a reproducible package to inflate directly. Default.
#' - "minimal": Minimal template to start a new package when you already know {fusen}.
#' - "additional": Template for an additional vignette, thus additional functions.
#' - "teaching": Template with a reproducible package, simpler than "full", but everything to
#'  teach the minimal structure of a package.
#'
#' Abbreviated names can also be used for the different templates. For example "add"
#' instead of additional.
#'
#' @return
#' Create flat Rmd file(s) and return its (their) path
#' @export
#'
#' @examples
#' # Create a new project
#' tmpdir <- tempdir()
#' dummypackage <- file.path(tmpdir, "dummypackage")
#' dir.create(dummypackage)
#' 
#' # Add
#' add_flat_template(pkg = dummypackage)
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
#' 
#' # For classical use in your package
#' \dontrun{
#' # first time ever using 'fusen'
#' add_flat_template("full") 
#' # first time in your new package
#' add_flat_template("minimal")
#' # add new flat file for new functions
#' add_flat_template("add")
#' #'}
add_flat_template <- function(
    template = c("full", "minimal", "additional", "teaching", "dev_history"),
    pkg = ".", 
    dev_dir = "dev",
    flat_name = template,
    overwrite = FALSE,
    open = TRUE) {

  project_name <- basename(normalizePath(pkg))
  if (project_name != asciify_name(project_name, to_pkg = TRUE)) {
    stop("Please rename your project/directory with: ", asciify_name(project_name, to_pkg = TRUE),
         " as a package name should only contain letters, numbers and dots.")
  }

  template <- match.arg(template)
  flat_name <- paste0("flat_", 
                      asciify_name(file_path_sans_ext(flat_name[1])), ".Rmd")
  
  pkg <- normalizePath(pkg)
  dev_dir <- file.path(pkg, dev_dir)
  if (!dir.exists(dev_dir)) {dir.create(dev_dir)}
  dev_file_path <- file.path(dev_dir, flat_name) #"dev_history.Rmd")
  
  # Which template
  if (template == "dev_history") {
    dev_file_path <- character(0)
  } else {
    template_file <- system.file(paste0("flat-template-", template, ".Rmd"), package = "fusen")
    
    if (file.exists(dev_file_path) & overwrite == FALSE) {
      n <- length(list.files(dev_dir, pattern = "^flat_.*[.]Rmd"))
      dev_file_path <- file.path(dev_dir, paste0(file_path_sans_ext(flat_name), "_", n + 1, ".Rmd"))
      message(
        flat_name, " already exists. New flat file is renamed '",
        basename(dev_file_path), "'. Use overwrite = TRUE, if you want to ",
        "overwrite the existing file or rename it."
      )
    }
    dev_name <- basename(dev_file_path)
    
    # Change lines asking for pkg name
    lines_template <- readLines(template_file)
    
    lines_template[grepl("<my_package_name>", lines_template)] <-
      gsub("<my_package_name>", basename(pkg),
           lines_template[grepl("<my_package_name>", lines_template)])
    
    # Change dev_history file name
    lines_template[grepl("dev_history.Rmd", lines_template)] <-
      gsub("dev_history.Rmd", dev_name,
           lines_template[grepl("dev_history.Rmd", lines_template)])
    
    cat(enc2utf8(lines_template), file = dev_file_path, sep = "\n")
  }
  
  # Add the-dev-history when needed
  if (template %in% c("full", "minimal", "dev_history")) {
    dev_file <- file.path(dev_dir, "0-dev_history.Rmd")
    if (file.exists(dev_file) & !isTRUE(overwrite)) {
      message("'0-dev_history.Rmd' already exists. It was not overwritten. ",
              "Set `add_flat_template(overwrite = TRUE)` if you want to do so.")
    } else {
      copy <- file.copy(
        system.file("the-dev-history.Rmd", package = "fusen"),
        dev_file,
        overwrite = overwrite
      )
      if (!copy) {
        stop("'0-dev_history.Rmd' could not be created in '", dev_dir, "'")
      }
      dev_file_path <- c(dev_file_path, dev_file)
    }
    
  }
  
  # .Rbuildignore
  # usethis::use_build_ignore(dev_dir) # Cannot be used outside project
  if (length(list.files(pkg, pattern = "[.]Rproj")) == 0) {
    lines <- c(paste0("^", basename(dev_dir), "/$"), "^\\.here$")
  } else {
    lines <- c(paste0("^", basename(dev_dir), "/$"))
  }
  
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
  
  if (length(list.files(pkg, pattern = "[.]Rproj")) == 0) {
    here::set_here(pkg)
  }
  if (isTRUE(open) & interactive()) {usethis::edit_file(dev_file_path)}
  
  dev_file_path
}
