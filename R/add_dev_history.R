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
#' \dontrun{
#' # Create a new project
#' tmpdir <- tempdir()
#' dummypackage <- file.path(tmpdir, "dummypackage")
#' dir.create(dummypackage)
#'
#' # Add
#' add_dev_history(pkg = dummypackage, name = "teaching")
#'
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
#' }
add_dev_history <- function(pkg = ".", overwrite = FALSE,
                            open = TRUE, dev_dir = "dev",
                            name = c("full", "minimal", "additional", "teaching")) {

  .Deprecated('add_flat_template', package = 'fusen', old = 'add_dev_history')

  project_name <- basename(normalizePath(pkg))
  if (project_name != asciify_name(project_name, to_pkg = TRUE)) {
    stop("Please rename your project/directory with: ", asciify_name(project_name, to_pkg = TRUE),
         " as a package name should only contain letters, numbers and dots.")
  }

  # old <- setwd(pkg)
  # on.exit(setwd(old))

  name <- match.arg(name)
  # Which template
  template <- system.file(paste0("dev-template-", name, ".Rmd"), package = "fusen")

  pkg <- normalizePath(pkg)
  dev_dir <- file.path(pkg, dev_dir)
  if (!dir.exists(dev_dir)) {dir.create(dev_dir)}
  dev_path <- file.path(dev_dir, "dev_history.Rmd")

  if (file.exists(dev_path) & overwrite == FALSE) {
    n <- length(list.files(dev_dir, pattern = "^dev_history.*[.]Rmd"))
    number <- n + 1
    dev_path <- file.path(dev_dir, paste0("dev_history_", number, ".Rmd"))
    message(
      "dev_history.Rmd already exists. New dev file is renamed '",
      basename(dev_path), "'. Use overwrite = TRUE, if you want to ",
      "overwrite the existing dev_history.Rmd file, or rename it."
    )
    name_vignette <- paste0("get-started_", number)
  }
  dev_name <- basename(dev_path)

  # Change lines asking for pkg name
  lines_template <- readLines(template)

  lines_template[grepl("<my_package_name>", lines_template)] <-
    gsub("<my_package_name>", basename(pkg),
         lines_template[grepl("<my_package_name>", lines_template)])

  # Change dev_history file name

    lines_template[grepl("dev_history.Rmd", lines_template)] <-
      gsub("dev_history.Rmd", dev_name,
           lines_template[grepl("dev_history.Rmd", lines_template)])

    # Add name of vignette if exists
    if(exists("name_vignette")){
      line_to_replace <- which(grepl("fusen::inflate", lines_template) & grepl("dev_history", lines_template))
      dev_name_vignette <- paste0("fusen::inflate(rmd = \"dev/", dev_name,"\", name = \"",name_vignette,"\")")
      lines_template[line_to_replace] <- dev_name_vignette
    }

  cat(enc2utf8(lines_template), file = dev_path, sep = "\n")

  # .Rbuildignore
  # usethis::use_build_ignore(dev_dir) # Cannot be used outside project
  if (length(list.files(pkg, pattern = "[.]Rproj")) == 0) {
    lines <- c(paste0("^", basename(dev_dir), "$"), "^\\.here$")
  } else {
    lines <- c(paste0("^", basename(dev_dir), "$"))
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
  if (isTRUE(open) & interactive()) {usethis::edit_file(dev_path)}

  dev_path
}

#' Clean names for any file and package
#' @param name Character to clean
#' @param to_pkg Transform all non authorized characters to dots for packages, instead of dash
#' @noRd
asciify_name <- function(name, to_pkg = FALSE) {
  # name <- "y  _ p n@ é ! 1"
  cleaned_name <- gsub("^-|-$", "",
                       gsub("-+", "-",
                            gsub("-_|_-", "-",
                                 gsub("[^([:alnum:]*_*-*)*]", "-", name))))
  # grepl("^[[:alpha:]][[:alnum:]_-]*$", cleaned_name)

  if (isTRUE(to_pkg)) {
    cleaned_name <- gsub("[^a-zA-Z0-9]+", ".",
                         gsub("^[0-9]+", "", cleaned_name))
  } else {
    # asciify from {usethis} usethis:::asciify()
    cleaned_name <- tolower(
        gsub("[^a-zA-Z0-9_-]+", "-", cleaned_name)
    )
  }
  cleaned_name
}
