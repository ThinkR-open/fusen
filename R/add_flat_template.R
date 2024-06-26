# WARNING - Generated by {fusen} from dev/flat_create_flat.Rmd: do not edit by hand

#' @rdname add_flat_template
#' @export
add_additional <- function(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "additional",
  overwrite = FALSE,
  open = TRUE
) {
  add_flat_template(
    template = "additional",
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = flat_name,
    overwrite = overwrite,
    open = open
  )
}

#' @rdname add_flat_template
#' @export
add_minimal_flat <- function(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "minimal",
  overwrite = FALSE,
  open = TRUE
) {
  add_flat_template(
    template = "minimal_flat",
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = flat_name,
    overwrite = overwrite,
    open = open
  )
}

#' @rdname add_flat_template
#' @export
add_minimal_package <- function(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "minimal",
  overwrite = FALSE,
  open = TRUE
) {
  add_flat_template(
    template = "minimal_package",
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = flat_name,
    overwrite = overwrite,
    open = open
  )
}

#' @rdname add_flat_template
#' @export
add_full <- function(
  pkg = ".",
  dev_dir = "dev",
  flat_name = "full",
  overwrite = FALSE,
  open = TRUE
) {
  add_flat_template(
    template = "full",
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = flat_name,
    overwrite = overwrite,
    open = open
  )
}

#' @rdname add_flat_template
#' @export
add_dev_history <- function(
  pkg = ".",
  dev_dir = "dev",
  overwrite = FALSE,
  open = TRUE
) {
  add_flat_template(
    template = "dev_history",
    pkg = pkg,
    dev_dir = dev_dir,
    flat_name = "fake",
    overwrite = overwrite,
    open = open
  )
}

flat_template_choices <- c(
  "full",
  "minimal_package",
  "minpkg",
  "minimal_flat",
  "minflat",
  "add",
  "additional",
  "teach",
  "teaching",
  "dev_history",
  "dev"
)

create_fusen_choices <- c("full", "minimal", "teaching", "dev_history")

#' Add flat Rmd file that drives package development
#'
#' @param template Name of the template to use. See details.
#' @param pkg Path where to save file
#' @param overwrite Whether to overwrite existing flat Rmd template file with same name
#' @param open Logical. Whether to open file after creation
#' @param dev_dir Name of directory for development Rmarkdown files. Default to "dev".
#' @param flat_name Name of the file to write in dev.
#' Use the name of the main function of your template to get chunks pre-filled with this function name.
#'
#' @importFrom tools file_path_sans_ext
#' @details
#' Choose `template` among the different templates available:
#'
#' - "full": The full template with a reproducible package that can directly be inflated.
#' It comes along with the "dev_history" template. Default.
#' - "minimal_package": Minimal template to start a new package when you already know 'fusen', along with the "dev_history" template. Note that this is called "minimal" in `create_fusen()`.
#' - "minimal_flat" or "additional": Template for a new minimal flat file only.
#' - "teaching": Template with a reproducible package, simpler than "full", but everything to
#'  teach the minimal structure of a package.
#' - "dev_history": Template with functions commonly used during package development.
#' This does not contain chunks to write your own functions.
#'
#' Abbreviated names can also be used for the different templates:
#' "add" for additional, "minflat" for minimal_flat, "minpkg" for minimal_package "teach" for teaching, "dev" for "dev_history".
#'
#' `add_additional()`, `add_minimal_flat()`, `add_dev_history()`, `add_minimal_package()`, `add_full()` are wrapper around `add_flat_template("additional")`, ...
#'
#' @rdname add_flat_template
#' @return
#' Create flat Rmd file(s) template(s) and return its (their) path
#' @export
#'
#' @examples
#' # Create a new project
#' dummypackage <- tempfile("dummy.package.flat")
#' dir.create(dummypackage)
#'
#' # Add
#' add_flat_template(template = "teaching", pkg = dummypackage)
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
#'
#' # For classical use in your package
#' \dontrun{
#' # first time ever using 'fusen'
#' add_flat_template("full")
#'
#' # first time in your new package
#' add_flat_template("minimal_package")
#'
#' # add new flat file for new functions
#' add_flat_template("add")
#' add_additional()
#' add_minimal_flat()
#'
#' # add only the dev_history file in an existing package
#' add_dev_history()
#'
#' # add new flat template for teaching (a reduced full template)
#' add_flat_template("teaching")
#' }
add_flat_template <- function(
  template = c("full", "minimal_package", "minimal_flat", "additional", "teaching", "dev_history"),
  pkg = ".",
  dev_dir = "dev",
  flat_name = NULL,
  overwrite = FALSE,
  open = TRUE
) {
  project_name <- get_pkg_name(pkg = pkg)

  if (project_name != asciify_name(project_name, to_pkg = TRUE)) {
    stop(
      "Please rename your project/directory with: `",
      asciify_name(project_name, to_pkg = TRUE),
      "` as a package name should only contain letters, numbers and dots."
    )
  }

  template <- template[1]
  template <- match.arg(template, choices = flat_template_choices)

  if (template %in% c("additional", "add")) {
    template <- "additional"
    if (is.null(flat_name)) {
      flat_name <- "additional"
    }
  } else if (template %in% c("minimal_flat", "minflat")) {
    template <- "additional"
    if (is.null(flat_name)) {
      flat_name <- "minimal"
    }
  } else if (template %in% c("minpkg", "minimal", "minimal_package", "min")) {
    template <- "minimal_package"
    if (is.null(flat_name)) {
      flat_name <- "minimal"
    }
  } else if (template %in% c("teach")) {
    template <- "teaching"
    if (is.null(flat_name)) {
      flat_name <- "teaching"
    }
  } else if (is.null(flat_name)) {
    flat_name <- template
  }


  if (!template %in% c("full", "teaching", "dev_history") &
    !flat_name %in% c("minimal", "minimal_package", "minimal_flat", "additional")) {
    fun_name <- clean_function_name(flat_name)
  } else {
    fun_name <- NA
  }
  flat_name <- paste0(
    "flat_",
    asciify_name(gsub("[.]Rmd$", "", flat_name[1])),
    ".Rmd"
  )

  pkg <- normalizePath(pkg)
  full_dev_dir <- file.path(pkg, dev_dir)
  if (!dir.exists(full_dev_dir)) {
    dir.create(full_dev_dir)
  }
  dev_file_path <- file.path(full_dev_dir, flat_name) # "dev_history.Rmd")

  # Which template ----
  if (template == "dev_history") {
    dev_file_path <- character(0)
  } else {
    template_file <- system.file(paste0("flat-template-", template, ".Rmd"), package = "fusen")

    if (file.exists(dev_file_path) & overwrite == FALSE) {
      n <- length(list.files(full_dev_dir, pattern = "^flat_.*[.]Rmd"))
      dev_file_path <- file.path(full_dev_dir, paste0(file_path_sans_ext(flat_name), "_", n + 1, ".Rmd"))
      message(
        flat_name,
        " already exists. New flat file is renamed '",
        basename(dev_file_path),
        "'. Use overwrite = TRUE, if you want to ",
        "overwrite the existing file or rename it."
      )
    }
    dev_name <- basename(dev_file_path)

    # Change lines asking for pkg name
    lines_template <- readLines(template_file)

    lines_template[grepl("<my_package_name>", lines_template)] <-
      gsub(
        "<my_package_name>",
        project_name,
        lines_template[grepl("<my_package_name>", lines_template)]
      )

    # Change flat_template file name
    # _inflate
    lines_template[grepl("dev/flat_template.Rmd", lines_template)] <-
      gsub(
        "dev/flat_template.Rmd",
        file.path(dev_dir, dev_name),
        lines_template[grepl("dev/flat_template.Rmd", lines_template)]
      )
    # _title
    lines_template[grepl("flat_template.Rmd", lines_template)] <-
      gsub(
        "flat_template.Rmd",
        dev_name,
        lines_template[grepl("flat_template.Rmd", lines_template)]
      )

    # Change my_fun to fun_name
    if (!is.na(fun_name)) {
      lines_template[grepl("my_fun", lines_template)] <-
        gsub(
          "my_fun",
          fun_name,
          lines_template[grepl("my_fun", lines_template)]
        )
    }

    cat(enc2utf8(lines_template), file = dev_file_path, sep = "\n")
  }

  # Add the-dev-history when needed ----
  if (template %in% c("full", "minimal_package", "dev_history")) {
    dev_file <- file.path(full_dev_dir, "0-dev_history.Rmd")
    if (file.exists(dev_file) & !isTRUE(overwrite)) {
      message(
        "'0-dev_history.Rmd' already exists. It was not overwritten. ",
        "Set `add_flat_template(overwrite = TRUE)` if you want to do so."
      )
    } else {
      copy <- file.copy(
        system.file("the-dev-history.Rmd", package = "fusen"),
        dev_file,
        overwrite = overwrite
      )
      if (!copy) {
        stop("'0-dev_history.Rmd' could not be created in '", full_dev_dir, "'")
      }
      dev_file_path <- c(dev_file_path, dev_file)
    }
  }

  # Add data for the full template exemple
  if (template %in% c("full")) {
    inst_dir <- file.path(pkg, "inst")
    # Create "inst/" directory
    if (!dir.exists(inst_dir)) {
      dir.create(inst_dir)
    }
    # Example dataset
    file.copy(system.file("nyc_squirrels_sample.csv", package = "fusen"), inst_dir)
  }

  # .Rbuildignore ----
  # usethis::use_build_ignore(dev_dir) # Cannot be used outside project
  if (length(list.files(pkg, pattern = "[.]Rproj")) == 0) {
    ignores <- c(paste0("^", dev_dir, "$"), "^\\.here$")
  } else {
    ignores <- c(paste0("^", dev_dir, "$"))
  }

  local_file_ignore(file = file.path(pkg, ".Rbuildignore"), ignores)

  # Add a gitignore file in dev_dir ----
  # Files to ignore
  ignores <- c("*.html")
  local_file_ignore(file = file.path(full_dev_dir, ".gitignore"), ignores)

  if (length(list.files(pkg, pattern = "[.]Rproj")) == 0 &
    !any(grepl("^[.]here$", list.files(pkg, all.files = TRUE)))) {
    here::set_here(pkg)
  }
  if (isTRUE(open) & interactive()) {
    lapply(dev_file_path, usethis::edit_file)
  }

  dev_file_path
}

#' Add new lines in a file if they are different from what exists
#' @noRd
local_file_ignore <- function(file, ignores) {
  buildfile <- normalizePath(file, mustWork = FALSE)
  if (!file.exists(buildfile)) {
    existing_lines <- character(0)
  } else {
    existing_lines <- readLines(buildfile, warn = FALSE, encoding = "UTF-8")
  }
  new <- setdiff(ignores, existing_lines)
  if (length(new) != 0) {
    all <- c(existing_lines, new)
    cat(enc2utf8(all), file = buildfile, sep = "\n")
  }
}
