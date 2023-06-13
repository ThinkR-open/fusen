# The regex to identify chunk names
regex_functions_vec <- c(
  "^function", "^fun$", "^fun-", "^fun_",
  "^funs$", "^funs-", "^funs_"
)
regex_functions <- paste(regex_functions_vec, collapse = "|")
regex_tests_vec <- c("^test")
regex_tests <- paste(regex_tests_vec, collapse = "|")
regex_development_vec <- c("^development", "^dev$", "^dev-", "^dev_")
regex_development <- paste(regex_development_vec, collapse = "|")
regex_desc_vec <- c("^description", "^desc")
regex_desc <- paste(regex_desc_vec, collapse = "|")
regex_example_vec <- c("^example", "^ex$", "^ex-", "^ex_")
regex_example <- paste(regex_example_vec, collapse = "|")

#' Inflate Rmd to package
#'
#' @param pkg Path to package
#' @param flat_file Path to Rmarkdown file to inflate
#' @param vignette_name Character. Title of the resulting vignette.
#' Use `NA` if you do not want to create a vignette.
#' @param open_vignette Logical. Whether to open vignette file at the end of the process
#' @param check Logical. Whether to check package after Rmd inflating
#' @param document Logical. Whether to document your package using \code{\link[attachment:att_amend_desc]{att_amend_desc}}
#' @param overwrite Logical (TRUE, FALSE) or character ("ask", "yes", "no).
#' Whether to overwrite vignette and functions if already exists.
#' @param ... Arguments passed to `devtools::check()`.
#'     For example, you can do `inflate(check = TRUE, quiet = TRUE)`, where `quiet` is
#'     passed to `devtools::check()`.
#'
#' @importFrom parsermd parse_rmd as_tibble
#' @importFrom utils getFromNamespace
#' @importFrom glue glue
#' @importFrom methods formalArgs
#'
#' @return
#' Package structure. Return path to current package.
#' @export
#'
#' @examples
#' # Create a new project
#' dummypackage <- tempfile("dummypackage")
#' dir.create(dummypackage)
#'
#' # {fusen} steps
#' dev_file <- add_flat_template(template = "full", pkg = dummypackage, overwrite = TRUE)
#' flat_file <- dev_file[grepl("flat", dev_file)]
#' fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
#' inflate(
#'   pkg = dummypackage, flat_file = flat_file,
#'   vignette_name = "Exploration of my Data", check = FALSE
#' )
#'
#' # Explore directory of the package
#' # browseURL(dummypackage)
#'
#' # Try pkgdown build
#' # usethis::use_pkgdown()
#' # pkgdown::build_site(dummypackage)
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
inflate <- function(pkg = ".", flat_file,
                    vignette_name = "Get started",
                    open_vignette = TRUE,
                    check = TRUE, document = TRUE,
                    overwrite = "ask",
                    ...) {
  if (!is.null(list(...)[["name"]])) {
    stop(paste0(
      "The `name` argument to `inflate()` is deprecated since {fusen} version 0.3.0.",
      "\nPlease use `vignette_name = '", list(...)[["name"]], "'` instead.\n"
    ))
    vignette_name <- list(...)[["name"]]
  }
  if (!is.null(list(...)[["rmd"]])) {
    stop(paste0(
      "The `rmd` argument to `inflate()` is deprecated since {fusen} version 0.3.0.",
      "\nPlease use `flat_file = '", list(...)[["rmd"]], "'` instead.\n"
    ))
    flat_file <- list(...)[["rmd"]]
  }

  # Save all open files
  if (
    requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("documentSaveAll")
  ) {
    rstudioapi::documentSaveAll()
  }

  # If flat_file empty
  if (missing(flat_file) && requireNamespace("rstudioapi") && rstudioapi::isAvailable() &&
    rstudioapi::hasFun("documentPath")) {
    current_file <- rstudioapi::documentPath()
    if (!is.null(current_file) && grepl("^flat.*[.](R|r|q)md$", basename(current_file))) {
      if (overwrite == "ask") {
        sure <- paste0(
          "You did not specify parameter 'flat_file'. The current file will be inflated:\n",
          current_file, ".\n",
          "With vignette name: ", vignette_name, "\n",
          "Are you sure this is what you planned? (y/n)\n"
        )
        do_it <- readline(sure) == "y"
      } else {
        do_it <- isTRUE(overwrite)
      }
      if (do_it) {
        message(
          "The current file will be inflated: ",
          current_file
        )
        flat_file <- current_file
      }
    }
  }

  if (missing(flat_file)) {
    stop(
      "`flat_file` argument is empty. ",
      "Did you run `inflate()` directly in the console, ",
      "instead of the one at the bottom of your flat file?"
    )
  }

  old <- setwd(pkg)
  on.exit(setwd(old))

  old_proj <- usethis::proj_get()
  if (normalizePath(old_proj) != normalizePath(pkg)) {
    on.exit(usethis::proj_set(old_proj))
    usethis::proj_set(pkg)
  }

  pkg <- normalizePath(pkg)
  needs_restart <- isFALSE(is_pkg_proj(pkg))
  flat_file <- normalizePath(flat_file, mustWork = TRUE)

  if (!file.exists(file.path(normalizePath(pkg), "DESCRIPTION"))) {
    stop(
      "DESCRIPTION file does not exist in your directory:", normalize_path_winslash(pkg), ".\n",
      "Have you run the content of the 'description' chunk of your {fusen} template?"
    )
  }

  if (length(list.files(pkg, pattern = ".Rproj")) > 0) {
    if (!file.exists(".Rbuildignore")) {
      file.create(".Rbuildignore")
    }
    # usethis::use_build_ignore(basename(flat_file))
    usethis::use_build_ignore(paste0(basename(pkg), ".Rproj"))
    usethis::use_build_ignore(".Rproj.user")
  }

  if (grepl(pkg, flat_file, fixed = TRUE)) {
    # Rmd already contains pkgpath
    flat_file_path <- flat_file
  } else {
    flat_file_path <- file.path(pkg, flat_file)
  }

  if (!file.exists(flat_file_path)) {
    stop(flat_file, " does not exists, please use fusen::add_flat_template() to create it.")
  }

  # Are you sure ?
  if (is.logical(overwrite)) {
    overwrite <- ifelse(isTRUE(overwrite), "yes", "no")
  }
  overwrite <- match.arg(overwrite, choices = c("ask", "yes", "no"))
  cleaned_vignette_name <- asciify_name(vignette_name)
  vignette_path <- file.path(pkg, "vignettes", paste0(cleaned_vignette_name, ".Rmd"))
  if (file.exists(vignette_path)) {
    if (overwrite == "ask") {
      rm_exist_vignette <- getFromNamespace("can_overwrite", "usethis")(vignette_path)
    } else {
      rm_exist_vignette <- ifelse(overwrite == "yes", TRUE, FALSE)
    }
    if (rm_exist_vignette) {
      file.remove(vignette_path)
    } else {
      stop(
        "Vignette already exists, answer 'yes' to the previous question",
        " or set inflate(..., overwrite = 'yes') to always overwrite."
      )
    }
  }

  # Create NAMESPACE
  namespace_file <- file.path(pkg, "NAMESPACE")
  if (!file.exists(namespace_file)) {
    roxygen2::roxygenise(pkg)
  }

  parsed_flat_file <- parse_rmd(flat_file)
  parsed_tbl <- as_tibble(parsed_flat_file)

  parsed_tbl$order <- 1:nrow(parsed_tbl)

  # Set start for group variables ----
  parsed_tbl$options <- parsermd::rmd_get_options(parsed_tbl)
  # Get filename option in chunk
  parsed_tbl$chunk_filename <- unlist(
    lapply(
      parsed_tbl[["options"]],
      function(x) {
        ifelse(is.null(x[["filename"]]),
          NA_character_, gsub('"', "", x[["filename"]])
        )
      }
    )
  )
  # Define sec_title to group functions in same R file
  sec_title <- paste(parsed_tbl[["sec_h1"]],
    parsed_tbl[["sec_h2"]],
    sep = "-"
  )

  if (length(sec_title) != 0) {
    parsed_tbl$sec_title <- sec_title
  } else {
    parsed_tbl$sec_title <- "fake-section-title"
  }

  # Get flat file path relative to package root
  # To be inserted in "DO NOT EDIT" comments
  relative_flat_file <- gsub(
    "^/", "",
    sub(normalize_path_winslash(pkg), "", normalize_path_winslash(flat_file),
      fixed = TRUE
    )
  )

  # Check if there are functions ----
  fun_code <- get_functions_tests(parsed_tbl)

  # Get functions and create R and tests files ----s
  if (!is.null(fun_code)) {
    script_files <- create_functions_all(parsed_tbl, fun_code, pkg, relative_flat_file)
  } else {
    message("No chunks named 'function-xx' or 'fun-xx' were found in the Rmarkdown file: ", flat_file)
    script_files <- tibble::tibble(type = character(0), path = character(0))
  }

  # Create vignette ----
  if (!(is.null(vignette_name) || is.na(vignette_name) || vignette_name == "")) {
    vignette_file <- create_vignette(
      parsed_tbl = parsed_tbl,
      pkg = pkg,
      relative_flat_file = relative_flat_file,
      vignette_name = vignette_name,
      open_vignette = open_vignette
    )

    all_files <- rbind(
      script_files,
      tibble::tibble(type = "vignette", path = vignette_file)
    )
  } else {
    all_files <- script_files
    message("`vignette_name` is empty: no vignette created")
  }

  # Update version in Description
  desc_file <- file.path(pkg, "DESCRIPTION")
  version <- as.character(utils::packageVersion("fusen"))
  the_desc <- desc::desc(file = desc_file)
  the_desc$set(`Config/fusen/version` = version)
  the_desc$write(file = desc_file)

  # config file store ----

  inflate_default_parameters <- formalArgs(fusen::inflate)
  inflate_default_parameters <- inflate_default_parameters[which(inflate_default_parameters != "...")]
  inflate_default_parameters <- inflate_default_parameters[which(inflate_default_parameters != "pkg")]

  inflate_default_parameters <- lapply(inflate_default_parameters, function(param) get(param)) %>%
    setNames(inflate_default_parameters)

  inflate_dots_parameters <- list(...)

  if (length(inflate_dots_parameters) > 0) {
    inflate_default_parameters <- c(inflate_default_parameters, inflate_dots_parameters)
  }

  inflate_default_parameters[["flat_file"]] <- relative_flat_file

  cli::cat_rule(glue("config file for {relative_flat_file}"))
  config_file <- df_to_config(
    df_files = all_files,
    flat_file_path = relative_flat_file,
    clean = TRUE,
    state = "active",
    # TODO - Set to force = FALSE when there is a possibility to clean the config
    # when there are manually deleted file ----
    force = TRUE,
    inflate_parameters = inflate_default_parameters
  )

  # TODO - Propose to clean all files with 'clean_fusen_files()' ----

  # if (check_for_obsolete) {
  #   clean_fusen_files()
  # }

  # Document and check package
  document_and_check_pkg(
    pkg = pkg,
    check = check,
    document = document,
    ...
  )


  # Restart RStudio
  if (needs_restart) {
    cli::cat_rule("RStudio restart needed")
    getFromNamespace("restart_rstudio", "usethis")("A restart of RStudio is required to activate the Build pane")
  }
  invisible(pkg)
}


#' Create function code, doc and tests ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_code tibble as issued from `get_functions`
#' @param relative_flat_file Path to the flat file to show in R scripts.
#' @param pkg Path to package
#' @importFrom stats na.omit
#' @noRd
create_functions_all <- function(parsed_tbl, fun_code, pkg, relative_flat_file) {
  fun_names <- fun_code[["fun_name"]]

  if (length(unique(na.omit(fun_names))) != length(na.omit(fun_names))) {
    stop("Some functions names are not unique: ", paste(sort(fun_names), collapse = ", "))
  }

  # Add funs if there are or deal with tests alone
  parsed_tbl <- add_names_to_parsed(parsed_tbl, fun_code)

  # Verify labels are unique
  dev_labels_noex <- c(
    regex_development_vec,
    regex_desc_vec,
    regex_functions_vec,
    regex_tests_vec
  )
  dev_labels_noex_regex <- paste(dev_labels_noex, collapse = "|")
  labels_in_vignette <- na.omit(parsed_tbl[["label"]][
    !grepl(dev_labels_noex_regex, parsed_tbl[["label"]])
  ])
  labels_in_vignette <- labels_in_vignette[!grepl("^$", labels_in_vignette)]

  if (any(duplicated(labels_in_vignette))) {
    stop(
      "There are duplicated chunk names, ",
      "please rename chunks with 'examples-fun_name' for instance.\n",
      "Duplicates: ",
      paste(labels_in_vignette[duplicated(labels_in_vignette)],
        collapse = ", "
      )
    )
  }

  # If there are functions
  if (nrow(fun_code) != 0) {
    # _Get examples
    fun_code <- add_fun_code_examples(parsed_tbl, fun_code)

    # _Create function files in R/
    # Create R directory if needed
    R_dir <- file.path(pkg, "R")
    if (!dir.exists(R_dir)) {
      dir.create(R_dir)
    }

    r_files <- create_r_files(fun_code, pkg, relative_flat_file)
  } else {
    r_files <- character(0)
  }

  # If there are tests
  test_files <- create_tests_files(parsed_tbl, pkg, relative_flat_file)

  script_files <- tibble::tibble(
    type =
      c(
        rep("R", length(r_files)),
        rep("test", length(test_files))
      ),
    path = c(r_files, test_files)
  )

  return(script_files)
}

#' Get function names ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @importFrom parsermd rmd_get_chunk
#' @noRd
get_functions_tests <- function(parsed_tbl) {
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
    grepl(regex_functions, parsed_tbl$label))
  which_parsed_tests <- which(!is.na(parsed_tbl$label) &
    grepl(regex_tests, parsed_tbl$label))

  rmd_fun <- parsed_tbl[which_parsed_fun, ]

  if (nrow(rmd_fun) != 0) {
    # At least one function
    fun_code <- lapply(seq_len(nrow(rmd_fun)), function(x) parse_fun(rmd_fun[x, ]))
    fun_code <- do.call("rbind", fun_code)
    fun_code$sec_h1 <- rmd_fun[["sec_h1"]]
    fun_code$sec_title <- rmd_fun[["sec_title"]]
  } else if (length(which_parsed_tests) != 0) {
    # Some tests but no function at all
    # Needs to be an empty tibble, and not a NULL
    # 0 lines allows to avoid dealing with examples associated with no functions
    fun_code <- tibble::tibble(
      fun_name = character(0),
      code = list(), # empty to avoid writing R file
      example_pos_start = logical(0),
      example_pos_end = logical(0),
      rox_filename = character(0),
      sec_title = character(0)
    )
  } else {
    fun_code <- NULL
  }

  return(fun_code)
}

#' create R file with code content and fun name
#' @param fun_code R code of functions in Rmd as character
#' @param pkg Path to package
#' @param relative_flat_file Path to the flat file to show in R scripts
#' @noRd
create_r_files <- function(fun_code, pkg, relative_flat_file) {
  fun_code <- fun_code[(lengths(fun_code[["code"]]) != 0), ]

  # Combine code with same sec_title to be set in same R file
  # fun_code$sec_title <- fun_code$sec_title[1] # for tests
  # Change "fun_name" afterwards if needed for file name
  fun_code <- group_code(
    fun_code,
    group_col = "file_name",
    code_col = "code_example"
  )

  r_files <- lapply(seq_len(nrow(fun_code)), function(x) {
    file_name <- fun_code[x, ][["file_name"]]

    r_file <- file.path(pkg, "R", paste0(asciify_name(file_name), ".R"))
    if (file.exists(r_file)) {
      cli::cli_alert_warning(paste(basename(r_file), "has been overwritten"))
    }
    lines <- c(
      sprintf("# WARNING - Generated by {fusen} from %s: do not edit by hand\n", relative_flat_file),
      unlist(fun_code[x, ][["code_example"]])
    )
    write_utf8(path = r_file, lines = lines)
    r_file
  })

  r_files <- unlist(r_files)
  return(r_files)
}

#' Check if there are unit tests ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param pkg Path to package
#' @param relative_flat_file Path to the flat file to show in R scripts
#'
#' @importFrom parsermd rmd_node_code
#'
#' @noRd
create_tests_files <- function(parsed_tbl, pkg, relative_flat_file) {
  project_name <- get_pkg_name(pkg = pkg)

  rmd_test <- parsed_tbl[!is.na(parsed_tbl$label) &
    grepl(regex_tests, parsed_tbl$label), ]

  # If there is at least one test
  if (nrow(rmd_test) != 0) {
    # Stop for tests chunks not having file_name
    if (any(is.na(rmd_test[["file_name"]]) | rmd_test[["file_name"]] == "")) {
      stop(
        "Some `test` chunks can not be handled: ",
        paste(rmd_test[["label"]][!is.na(rmd_test[["file_name"]])],
          collapse = ", "
        ),
        ". Please associate these `test` chunks with a `function` chunk, ",
        "under a section title or with a `filename='mytestfile.R'` chunk option."
      )
    }

    # Group code by file_name
    rmd_test[["code"]] <- rmd_node_code(rmd_test[["ast"]])
    rmd_test <- group_code(rmd_test, group_col = "file_name", code_col = "code")

    # Filter if code is still empty after code grouped
    rmd_test[["is_empty"]] <- lapply(
      rmd_test[["code"]], function(x) grepl("^\\s*$", paste(x, collapse = ""))
    ) %>%
      unlist()
    rmd_test <- rmd_test[!rmd_test[["is_empty"]], ]

    if (nrow(rmd_test) != 0) {
      # Add directory
      requireNamespace("testthat")

      # setup testhat
      test_dir <- file.path(pkg, "tests")
      if (!dir.exists(test_dir)) {
        dir.create(test_dir)
        dir.create(file.path(test_dir, "testthat"))
        cat(
          enc2utf8(c(
            "library(testthat)",
            paste0("library(", project_name, ")"),
            "",
            paste0('test_check("', project_name, '")')
          )),
          sep = "\n",
          file = file.path(test_dir, "testthat.R")
        )
      }

      out <- unlist(lapply(
        seq_len(nrow(rmd_test)),
        function(x) parse_test(rmd_test[x, ], pkg, relative_flat_file)
      ))

      return(out)
    }
  }
  return(NULL)
}

#' Create vignette
#' @param parsed_tbl tibble of a parsed Rmd
#' @param pkg Path to package
#' @param relative_flat_file Path to the flat file to show in R scripts.
#' @param vignette_name Name of the resulting vignette
#' @param open_vignette Logical. Whether to open vignette file
#' @noRd
create_vignette <- function(parsed_tbl, pkg, relative_flat_file, vignette_name, open_vignette = TRUE) {
  old_proj <- usethis::proj_get()

  if (normalizePath(old_proj) != normalizePath(pkg)) {
    on.exit(usethis::proj_set(old_proj))
    usethis::proj_set(pkg)
  }


  # Create vignette directory if needed
  vignette_dir <- file.path(pkg, "vignettes")
  if (!dir.exists(vignette_dir)) {
    dir.create(vignette_dir)
  }

  # _remove dev, description, function and tests.
  # Keep examples and unnamed
  not_in_vignette <-
    paste(c(
      regex_desc,
      regex_tests,
      regex_development,
      regex_functions
    ), collapse = "|")
  vignette_tbl <- parsed_tbl[
    !(grepl(not_in_vignette, parsed_tbl[["label"]]) |
      grepl("rmd_yaml_list", parsed_tbl[["type"]])),
  ]

  flat_yaml <- parsed_tbl[grepl("rmd_yaml_list", parsed_tbl[["type"]]), ]
  # Make chunk names unique
  # vignette_tbl[["label"]][grepl("unnamed", vignette_tbl[["label"]])] <-
  #   gsub("unnamed-", "parsermd-", vignette_tbl[["label"]][grepl("unnamed", vignette_tbl[["label"]])])
  #   is.na(vignette_tbl[["label"]]) & vignette_tbl[["type"]] == "rmd_chunk",
  #                                   gsub("[.]+", "-", make.names(vignette_name)),
  #                                   vignette_tbl[["label"]])
  #
  # vignette_tbl[["label"]] <- make.unique(vignette_tbl[["label"]], sep = "-")
  # # /!\ Not re-used in as_document(), this must be in ast

  # ast <- vignette_tbl[["ast"]][[21]]

  # To correct for {parsermd} unnamed attribution
  fix_unnamed_chunks <- function(ast) {
    if (inherits(ast, "rmd_chunk") && grepl("unnamed-chunk-", ast[["name"]])) {
      ast[["name"]] <- gsub("unnamed-", "parsermd-", ast[["name"]])
    }
    ast
  }

  ast_class <- class(vignette_tbl[["ast"]])
  vignette_tbl[["ast"]] <- lapply(vignette_tbl[["ast"]], fix_unnamed_chunks)
  class(vignette_tbl[["ast"]]) <- ast_class

  # File to save
  cleaned_vignette_name <- asciify_name(vignette_name)
  vignette_file <- file.path("vignettes", paste0(cleaned_vignette_name, ".Rmd"))

  # Yaml info
  yaml_options <- flat_yaml[["ast"]][[1]]
  # Vignette
  # Copied from usethis::use_vignette() to allow to not open vignette created
  usethis::use_package("knitr", "Suggests")
  # desc <- desc::desc(file = usethis::proj_get())
  desc <- desc::desc(file = pkg)
  desc$set("VignetteBuilder", "knitr")
  desc$write()
  usethis::use_git_ignore("inst/doc")

  # Vignette head
  head <- create_vignette_head(
    pkg = pkg,
    vignette_name = vignette_name,
    yaml_options = yaml_options
  )

  # Write vignette
  lines <- c(
    head,
    "",
    sprintf(
      "<!-- WARNING - This vignette is generated by {fusen} from %s: do not edit by hand -->\n",
      relative_flat_file
    )
  )
  if (nrow(vignette_tbl) != 0) {
    lines <- c(
      lines,
      parsermd::as_document(vignette_tbl)
    )
  }

  write_utf8(path = vignette_file, lines = lines)

  if (isTRUE(open_vignette) & interactive()) {
    usethis::edit_file(vignette_file)
  }

  return(vignette_file)
}
