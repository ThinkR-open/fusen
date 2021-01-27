#' Inflate Rmd to package
#'
#' @param pkg Path to package
#' @param name Name of the resulting vignette
#' @param rmd Path to Rmd file to inflate
#' @param check Logical. Whether to check package after Rmd inflating
#'
#' @importFrom parsermd parse_rmd as_tibble
#' @return
#' Package structure. Return path to current package.
#' @export
#'
#' @examples
#' # Create a new project
#' tmpdir <- tempdir()
#' dummypackage <- file.path(tmpdir, "dummypackage")
#' dir.create(dummypackage)
#' 
#' # {fusen} steps
#' fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
#' dev_file <- add_dev_history(pkg = dummypackage, overwrite = TRUE)
#' inflate(pkg = dummypackage, rmd = dev_file, name = "exploration", check = FALSE)
#' 
#' # Try pkgdown build
#' # pkgdown::build_site(dummypackage)
#' # usethis::use_build_ignore("docs")
#' # usethis::use_git_ignore("docs")
#' # Delete dummy package
#' unlink(dummypackage, recursive = TRUE)
inflate <- function(pkg = ".", rmd = file.path("dev", "dev_history.Rmd"), name = "exploration", check = TRUE) {
  old <- setwd(pkg)
  on.exit(setwd(old))

  pkg_path <- normalizePath(pkg)
  rmd <- normalizePath(rmd, mustWork = FALSE)

  if (length(list.files(pkg_path, pattern = ".Rproj")) > 0) {
    if (!file.exists(".Rbuildignore")) {
      file.create(".Rbuildignore")
    }
    # usethis::use_build_ignore(basename(rmd))
    usethis::use_build_ignore(paste0(basename(pkg_path), ".Rproj"))
    usethis::use_build_ignore(".Rproj.user")
  }

  if (grepl(pkg_path, rmd, fixed = TRUE)) {
    # Rmd already contains pkgpath
    rmd_path <- rmd
  } else {
    rmd_path <- file.path(pkg_path, rmd)
  }

  if (!file.exists(rmd_path)) {
    stop(rmd, " does not exists, please use fusen::add_dev_history() to create it.")
  }

  # Create NAMESPACE
  namespace_file <- file.path(pkg_path, "NAMESPACE")
  if (!file.exists(namespace_file)) {
    roxygen2::roxygenise(pkg_path)
  }

  parsed_rmd <- parse_rmd(rmd)
  parsed_tbl <- as_tibble(parsed_rmd)

  # Get functions ----
  # Check if there are functions ----
  fun_code <- get_functions(parsed_tbl)
  fun_names <- fun_code[["fun_name"]]
  parsed_tbl <- add_fun_to_parsed(parsed_tbl, fun_names)
  # _Get examples
  fun_code <- add_fun_code_examples(parsed_tbl, fun_code)

  # _Create function files in R/
  # Create R directory if needed
  R_dir <- file.path(pkg_path, "R")
  if (!dir.exists(R_dir)) {
    dir.create(R_dir)
  }

  create_r_files(fun_code, pkg_path)
  create_tests_files(parsed_tbl, pkg_path)
  create_vignette(parsed_tbl, pkg_path, name)

  # Run attachment
  attachment::att_amend_desc(path = pkg_path)

  # Check
  if (isTRUE(check)) {
    rcmdcheck::rcmdcheck(pkg = pkg_path)
  }

  pkg_path
}

#' Get function names ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @importFrom parsermd rmd_get_chunk
get_functions <- function(parsed_tbl) {
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
    grepl("function", parsed_tbl$label))
  rmd_fun <- parsed_tbl[which_parsed_fun, ]

  if (nrow(rmd_fun) != 0) {
    parse_fun <- function(x) { # x <- rmd_fun[3,]
      # browser()
      code <- rmd_get_chunk(x)$code
      # find function name
      fun_name <- stringr::str_extract(
        code[grep("function(\\s*)\\(", code)],
        "\\w*(?=(\\s*)(<-|=)(\\s*)function)"
      ) %>%
        gsub(" ", "", .) # remove spaces

      all_arobase <- grep("^#'\\s*@|function(\\s*)\\(", code)
      example_pos_start <- grep("^#'\\s*@example", code)[1]

      example_pos_end <- all_arobase[all_arobase > example_pos_start][1] - 1
      example_pos_end <- ifelse(is.na(example_pos_end),
        grep("function(\\s*)\\(", code) - 1,
        example_pos_end
      )

      tibble::tibble(
        fun_name = fun_name[1],
        code = list(code),
        example_pos_start = example_pos_start,
        example_pos_end = example_pos_end
      )
    }
    fun_code <- lapply(seq_len(nrow(rmd_fun)), function(x) parse_fun(rmd_fun[x, ]))
    fun_code <- do.call("rbind", fun_code)
    fun_code
  }
}

#' Add function name to parsed_tbl ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_names Names of functions in Rmd
#' @importFrom stats na.omit
add_fun_to_parsed <- function(parsed_tbl, fun_names) {
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
    grepl("function", parsed_tbl$label))

  parsed_tbl$order <- 1:nrow(parsed_tbl)
  parsed_tbl$sec_title <- paste(parsed_tbl[["sec_h1"]], parsed_tbl[["sec_h2"]], sep = "-")
  parsed_tbl$fun_name <- NA_character_
  # Function name
  parsed_tbl[["fun_name"]][which_parsed_fun] <- fun_names

  pkg_filled <- lapply(na.omit(unique(parsed_tbl[["sec_title"]])), function(x) {
    group <- which(parsed_tbl[["sec_title"]] == x)
    parsed_tbl[group, ] <- tidyr::fill(parsed_tbl[group, ], fun_name)
  }) %>%
    do.call("rbind", .)
  parsed_tbl[["fun_name"]][pkg_filled[["order"]]] <- pkg_filled[["fun_name"]]
  parsed_tbl
}

#' Add examples in function code
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_code R code of functions in Rmd as character
add_fun_code_examples <- function(parsed_tbl, fun_code) {
  #  Example already in skeleton
  fun_code$example_in <- apply(fun_code, 1, function(x) {
    # browser()
    if (!is.na(x[["example_pos_start"]]) && length(x[["example_pos_start"]]) == 1) {
      list(x[["code"]][x[["example_pos_start"]]:x[["example_pos_end"]]])
    } else {
      list("#' @examples")
    }
  }) %>% lapply(., function(x) x[[1]])

  # Example in separate chunk
  which_parsed_ex <- which(!is.na(parsed_tbl$label) &
    grepl("example", parsed_tbl$label))
  rmd_ex <- parsed_tbl[which_parsed_ex, ]

  if (nrow(rmd_ex) != 0) {
    example_code <- lapply(
      seq_len(nrow(rmd_ex)),
      function(x) {
        tibble::tibble(
          fun_name = rmd_ex[x, ][["fun_name"]],
          example_chunk = list(paste("#'", rmd_get_chunk(rmd_ex[x, ])$code))
          # example_chunk = rmd_get_chunk(rmd_ex[x, ])$code
        )
      }
    ) %>% do.call("rbind", .)
    # Add to function tibble
    fun_code <- merge(fun_code, example_code, by = "fun_name", all.x = TRUE) %>%
      tibble::as_tibble()
    fun_code[["example"]] <- lapply(seq_len(nrow(fun_code)), function(x) {
      example <- stats::na.omit(unlist(c(
        fun_code[["example_in"]][x],
        fun_code[["example_chunk"]][x]
      )))
    })
  } else {
    fun_code[["example"]] <- fun_code[["example_in"]]
  }

  # Remove if example is empty
  fun_code[["example"]] <- lapply(fun_code[["example"]], function(example) {
    if (length(example) == 1 & example[1] == "#' @examples") {
      return(NA)
    } else {
      return(example)
    }
  })

  # Add to function code
  fun_code[["code_example"]] <- lapply(seq_len(nrow(fun_code)), function(x) {
    fun_code_x <- fun_code[x, ]
    all_fun_code <- stats::na.omit(c(
      # begin
      unlist(fun_code_x[["code"]])[1:(
        ifelse(is.na(fun_code_x[["example_pos_start"]]),
          fun_code_x[["example_pos_end"]],
          fun_code_x[["example_pos_start"]] - 1
      ))],
      # examples
      unlist(fun_code_x[["example"]]),
      # end
      unlist(fun_code_x[["code"]])[
        (fun_code_x[["example_pos_end"]] + 1):length(unlist(fun_code_x[["code"]]))
      ]
    ))
  })

  fun_code
}

#' create R file with code content and fun name
#' @param fun_code R code of functions in Rmd as character
#' @param pkg_path Path to package
create_r_files <- function(fun_code, pkg_path) {
  r_files <- lapply(seq_len(nrow(fun_code)), function(x) {
    fun_name <- fun_code[x, ][["fun_name"]]
    r_file <- file.path(pkg_path, "R", paste0(fun_name, ".R"))
    if (file.exists(r_file)) {
      warning(basename(r_file), " has been overwritten")
    }
    cat(
      enc2utf8(unlist(fun_code[x, ][["code_example"]])),
      file = r_file, sep = "\n"
    )
    r_file
  })
}

#' Check if there are unit tests ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param pkg_path Path to package
#' @importFrom parsermd rmd_get_chunk
create_tests_files <- function(parsed_tbl, pkg_path) {
  rmd_test <- parsed_tbl[!is.na(parsed_tbl$label) &
    grepl("test", parsed_tbl$label), ]
  if (nrow(rmd_test) != 0) {
    requireNamespace("testthat")
    # setup testhat
    test_dir <- file.path(pkg_path, "tests")
    if (!dir.exists(test_dir)) {
      dir.create(test_dir)
      dir.create(file.path(test_dir, "testthat"))
      cat(enc2utf8(c("library(testthat)",
        paste0("library(", basename(pkg_path), ")"),
        "",
        paste0('test_check("', basename(pkg_path), '")')
        )),
        sep = "\n",
        file = file.path(test_dir, "testthat.R")
      )
    }

    parse_test <- function(x) { # x <- rmd_test[1,]
      code <- rmd_get_chunk(x)$code

      # create R file with code content and fun name
      fun_name <- x[["fun_name"]]
      if (is.na(fun_name) || fun_name == "") {
        stop("No function found associated to chunk ", x[["label"]])
      }

      test_file <- file.path(pkg_path, "tests", "testthat", paste0("test_", fun_name, ".R"))
      if (file.exists(test_file)) {
        warning(basename(test_file), " has been overwritten")
      }
      cat(enc2utf8(code), file = test_file, sep = "\n")

      fun_name
    }
    out <- unlist(lapply(seq_len(nrow(rmd_test)), function(x) parse_test(rmd_test[x, ])))
  }
}

#' Create vignette
#' @param parsed_tbl tibble of a parsed Rmd
#' @param pkg_path Path to package
#' @param name Name of the resulting vignette
create_vignette <- function(parsed_tbl, pkg_path, name) {
  # Create vignette directory if needed
  vignette_dir <- file.path(pkg_path, "vignettes")
  if (!dir.exists(vignette_dir)) {
    dir.create(vignette_dir)
  }

  # _remove dev, description, function and tests.
  # Keep examples and unnamed
  vignette_tbl <- parsed_tbl[
    !(grepl("description|function|test|development", parsed_tbl[["label"]]) |
      grepl("rmd_yaml_list", parsed_tbl[["type"]])),
  ]

  # Make chunk names unique
  # vignette_tbl[["label"]] <- ifelse(
  #   is.na(vignette_tbl[["label"]]) & vignette_tbl[["type"]] == "rmd_chunk",
  #                                   gsub("[.]+", "-", make.names(name)),
  #                                   vignette_tbl[["label"]])
  #
  # vignette_tbl[["label"]] <- make.unique(vignette_tbl[["label"]], sep = "-")
  # # Not re-used in as_document()

  usethis::use_vignette(name)
  vignette_file <- file.path("vignettes", paste0(gsub("[^a-zA-Z0-9_-]+", "-", name), ".Rmd"))
  if (!file.exists(vignette_file)) {
    stop(
      "Vignette could not be filled because of naming problem.",
      "Have you used some special characters in `name`?"
    )
  }

  # Write vignette
  cat("",
    enc2utf8(parsermd::as_document(vignette_tbl)),
    sep = "\n", append = TRUE,
    file = vignette_file
  )
}
