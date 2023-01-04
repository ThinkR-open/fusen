
#' Parse function code as tibble and get positions
#' @param x One row out of function parsed tibble
#' @importFrom parsermd rmd_node_code
#' @noRd
parse_fun <- function(x) { # x <- rmd_fun[3,]

  code <- unlist(rmd_node_code(x[["ast"]]))
  # There is a function (or R6Class)
  regex_isfunction <- paste(
    # function
    "function(\\s*)\\(",
    # R6Class
    "R6Class(\\s*)\\(",
    sep = "|"
  )

  regex_extract_fun_name <- paste(
    # function
    "[\\w[.]]*(?=(\\s*)(<-|=)(\\s*)function)",
    # R6Class
    "[\\w[.]]*(?=(\\s*)(<-|=)(\\s*)R6Class)",
    # R6::R6Class
    "[\\w[.]]*(?=(\\s*)(<-|=)(\\s*)R6::R6Class)",
    sep = "|"
  )

  # stringi::stri_extract_first_regex(
  #   c("zaza <- function()", "zozo <- R6Class()", "zuzu <- R6::R6Class()"),
  #   regex_extract_fun_name
  # )

  # Clean extra space between #' and @
  code <- gsub(pattern = "#'\\s*@", "#' @", code)

  # Get all #'
  all_hastags <- grep("^#'", code)

  # Get all #
  all_comments <- grep("^#", code)

  # Get all functions
  fun_positions <- setdiff(grep(regex_isfunction, code), all_comments)

  # find function name
  fun_name <- stringi::stri_extract_first_regex(
    code[fun_positions],
    regex_extract_fun_name
  ) %>%
    gsub(" ", "", .) # remove spaces

  # Find start of function
  first_function_start <- fun_positions[1]

  # Get last hastags above first fun
  if (length(all_hastags) != 0) {
    last_hastags_above_first_fun <- max(all_hastags[all_hastags < first_function_start])
  } else {
    last_hastags_above_first_fun <- NA
  }

  # Add @noRd if no roxygen doc or no @export before function
  if (!any(grepl("@export|@noRd", code))) {
    if (!is.na(last_hastags_above_first_fun)) {
      code <- c(
        code[1:last_hastags_above_first_fun],
        "#' @noRd",
        code[(last_hastags_above_first_fun + 1):length(code)]
      )
    } else if (all(grepl("^\\s*$", code))) {
      # If all empty
      code <- character(0)
    } else if (!is.na(first_function_start)) {
      # If there is a function inside
      code <- c("#' @noRd", code)
    }
    # otherwise code stays code
  }

  all_arobase <- grep("^#'\\s*@|function(\\s*)\\(", code)
  example_pos_start <- grep("^#'\\s*@example", code)[1]

  example_pos_end <- all_arobase[all_arobase > example_pos_start][1] - 1
  example_pos_end <- ifelse(is.na(example_pos_end),
    grep("function(\\s*)\\(", code) - 1,
    example_pos_end
  )

  # Get @rdname and @filename for groups
  tag_filename <- gsub(
    "^#'\\s*@filename\\s*", "",
    code[grep("^#'\\s*@filename", code)]
  )
  tag_rdname <- gsub(
    "^#'\\s*@rdname\\s*", "",
    code[grep("^#'\\s*@rdname", code)]
  )
  rox_filename <- c(tag_filename, tag_rdname)[1]
  # Clean code for @filename as non standard roxygen
  code[grep("^#'\\s*@filename", code)] <- "#'"

  tibble::tibble(
    fun_name = fun_name[1],
    code = list(code),
    example_pos_start = example_pos_start,
    example_pos_end = example_pos_end,
    rox_filename = rox_filename
  )
}


#' Add function name and filenames to parsed_tbl ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_code Information extracted from functions code
#' @importFrom stats na.omit
#' @noRd
add_names_to_parsed <- function(parsed_tbl, fun_code) {
  # Which parts were functions
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
    grepl(regex_functions, parsed_tbl$label))

  # From fun_code, we retrieve fun_name & rox_filename
  parsed_tbl[["fun_name"]] <- NA_character_
  parsed_tbl[["rox_filename"]] <- NA_character_
  parsed_tbl[["sec_fun_name"]] <- NA_character_

  # Function name
  # There may be no function, but `test` chunks exist to be parsed
  if (length(which_parsed_fun) != 0) {
    fun_names <- fun_code[["fun_name"]]
    parsed_tbl[["fun_name"]][which_parsed_fun] <- fun_names
    # roxygen filename
    rox_filename <- fun_code[["rox_filename"]]
    parsed_tbl[["rox_filename"]][which_parsed_fun] <- rox_filename
    # sec_title - get fun_name of first function
    df <- data.frame(
      sec_title = parsed_tbl[["sec_title"]][which_parsed_fun],
      sec_fun_name = parsed_tbl[["fun_name"]][which_parsed_fun],
      fake = NA,
      stringsAsFactors = FALSE
    )
    sec_title_name <- group_code(df, group_col = "sec_title", code_col = "fake")

    # Fill values for parts that are not function chunks
    pkg_filled <- lapply(na.omit(unique(parsed_tbl[["sec_title"]])), function(x) {
      group <- which(parsed_tbl[["sec_title"]] == x)
      # reorder chunks for fun, ex, test ?
      # however, what happens when multiple groups under same title ?
      sec_fun_name <- sec_title_name[
        sec_title_name[["sec_title"]] == x, "sec_fun_name"
      ]
      parsed_tbl[group, "sec_fun_name"] <- ifelse(length(sec_fun_name) == 0,
        NA_character_, as.character(sec_fun_name)
      )
      parsed_tbl[group, ] <- tidyr::fill(
        parsed_tbl[group, ],
        fun_name, rox_filename, chunk_filename,
        .direction = "down"
      )
      parsed_tbl[group, ] <- tidyr::fill(
        parsed_tbl[group, ],
        fun_name, rox_filename, chunk_filename,
        .direction = "up"
      )
    }) %>%
      do.call("rbind", .)
    parsed_tbl[["fun_name"]][pkg_filled[["order"]]] <- pkg_filled[["fun_name"]]
  } else {
    pkg_filled <- parsed_tbl[, c(
      "fun_name", "chunk_filename", "rox_filename",
      "sec_fun_name", "sec_title"
    )]
    pkg_filled[, "order"] <- 1:nrow(pkg_filled)
  }

  # Choose future filename ----
  # Priority = chunk_filename > rox_filename > sec_title_fun_name > fun_name > sec_title
  # If sec_title, choose fun_name of the first function
  pkg_filled[["file_name"]] <- NA_character_
  # chunk_filename
  pkg_filled[["file_name"]] <- ifelse(!is.na(pkg_filled[["chunk_filename"]]),
    pkg_filled[["chunk_filename"]], NA_character_
  )
  # rox_filename
  pkg_filled[["file_name"]] <- ifelse(
    is.na(pkg_filled[["file_name"]]) &
      !is.na(pkg_filled[["rox_filename"]]),
    pkg_filled[["rox_filename"]],
    pkg_filled[["file_name"]]
  )
  # sec_title - get sec_fun_name
  pkg_filled[["file_name"]] <- ifelse(
    is.na(pkg_filled[["file_name"]]) &
      !is.na(pkg_filled[["sec_fun_name"]]),
    pkg_filled[["sec_fun_name"]],
    pkg_filled[["file_name"]]
  )
  # fun_name
  pkg_filled[["file_name"]] <- ifelse(
    is.na(pkg_filled[["file_name"]]) &
      !is.na(pkg_filled[["fun_name"]]),
    pkg_filled[["fun_name"]],
    pkg_filled[["file_name"]]
  )
  # sec_title alone if not real function
  pkg_filled[["file_name"]] <- ifelse(
    is.na(pkg_filled[["file_name"]]) &
      !is.na(pkg_filled[["sec_title"]]),
    pkg_filled[["sec_title"]],
    pkg_filled[["file_name"]]
  )

  # Clean filename if script extension
  pkg_filled[["file_name"]] <- gsub("[.]R$|[.]r$", "", pkg_filled[["file_name"]])

  parsed_tbl[["file_name"]] <- NA_character_
  parsed_tbl[["file_name"]][pkg_filled[["order"]]] <- pkg_filled[["file_name"]]

  parsed_tbl
}


#' Function to parse test code
#' @param x One line of a tibble of a parsed Rmd
#' @param pkg Path to package
#' @param relative_flat_file Relative path to flat file
#' @noRd
parse_test <- function(x, pkg, relative_flat_file) { # x <- rmd_test[1,]
  # create R file with code content and file_name filename
  file_name <- x[["file_name"]]

  test_file <- file.path(
    pkg, "tests", "testthat",
    paste0("test-", asciify_name(file_name), ".R")
  )

  if (file.exists(test_file)) {
    cli::cli_alert_warning(paste(basename(test_file), "has been overwritten"))
  }

  lines <- c(
    sprintf("# WARNING - Generated by {fusen} from %s: do not edit by hand\n", relative_flat_file),
    x[["code"]][[1]]
  )
  write_utf8(path = test_file, lines = lines)

  return(test_file)
}

#' Add examples in function code
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_code R code of functions in Rmd as character
#' @importFrom parsermd rmd_node_code
#' @noRd
add_fun_code_examples <- function(parsed_tbl, fun_code) {
  # Example in separate chunk
  which_parsed_ex <- which(!is.na(parsed_tbl$label) &
    grepl(regex_example, parsed_tbl$label))
  rmd_ex <- parsed_tbl[which_parsed_ex, ]

  # Get file_name variable
  fun_code$order <- 1:nrow(fun_code)
  fun_file <- parsed_tbl[!is.na(parsed_tbl[["fun_name"]]), c("fun_name", "file_name")]
  fun_file_groups <- fun_file[!duplicated(fun_file), ]
  # Keep all as some are not functions
  fun_code <- as_tibble(merge(fun_code, fun_file_groups, by = "fun_name", all.x = TRUE, sort = FALSE))
  fun_code <- fun_code[order(fun_code[["order"]]), ]
  # Get file_name for not functions. Only last place where possible
  fun_code[["file_name"]] <- ifelse(is.na(fun_code[["file_name"]]),
    fun_code[["sec_title"]],
    fun_code[["file_name"]]
  )

  #  Example already in skeleton
  fun_code$example_in <- apply(fun_code, 1, function(x) {
    if (is.na(x[["fun_name"]])) {
      list(character(0))
    } else if (!is.na(x[["example_pos_start"]]) && length(x[["example_pos_start"]]) == 1) {
      list(x[["code"]][x[["example_pos_start"]]:x[["example_pos_end"]]])
    } else {
      list("#' @examples")
    }
  }) %>% lapply(., function(x) x[[1]])

  # No function, no example to add
  ex_alone <- rmd_ex[is.na(rmd_ex[["fun_name"]]), ]

  if (nrow(ex_alone) != 0) {
    message(
      "Some example chunks are not associated to any function: ",
      paste(ex_alone[["label"]], collapse = ", "), ".",
      "\nIf you plan to include them only in the vignette, then ",
      "you can give them any other name except `dev*`, `fun*`, `test*`"
    )
  }
  rmd_ex <- rmd_ex[!is.na(rmd_ex[["fun_name"]]), ]

  if (nrow(rmd_ex) != 0) {
    # Group rmd_ex for the same function
    rmd_ex$rmd_ex_code <- lapply(1:nrow(rmd_ex), function(x) {
      rmd_node_code(rmd_ex[x, ][["ast"]])
    })
    rmd_ex_group <- group_code(df = rmd_ex, group_col = "fun_name", code_col = "rmd_ex_code")

    # Get example code
    example_code <- lapply(
      seq_len(nrow(rmd_ex_group)),
      function(x) {
        tibble::tibble(
          fun_name = rmd_ex_group[x, ][["fun_name"]],
          # example_chunk = list(paste("#'", rmd_get_chunk(rmd_ex[x, ])$code))
          # example_chunk = list(paste("#'", unlist(rmd_node_code(rmd_ex[x, ][["ast"]]))))
          example_chunk = list(paste("#'", unlist(rmd_ex_group[x, ][["rmd_ex_code"]])))
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
    # example <- fun_code[["example"]][[1]]
    if (length(example) == 0) {
      return(NA)
    } else if (length(example) == 1 && is.na(example)) {
      return(NA)
    } else if (length(example) == 1 && example == "#' @examples") {
      return(NA)
    } else if (length(example) > 1 & all(grepl("^#'\\s+$", example[-1]))) {
      return(NA)
    } else {
      return(example)
    }
  })

  # Add to function code
  fun_code[["code_example"]] <- lapply(seq_len(nrow(fun_code)), function(x) {
    fun_code_x <- fun_code[x, ]
    if (is.na(fun_code_x[["fun_name"]])) {
      return(
        unlist(fun_code_x[["code"]])
      )
    }

    end_skeleton <- ifelse(is.na(fun_code_x[["example_pos_start"]]),
      fun_code_x[["example_pos_end"]],
      fun_code_x[["example_pos_start"]] - 1
    )

    all_fun_code <- stats::na.omit(c(
      # begin
      if (!is.na(end_skeleton)) {
        unlist(fun_code_x[["code"]])[1:end_skeleton]
      },
      # examples
      unlist(fun_code_x[["example"]]),
      # end
      unlist(fun_code_x[["code"]])[
        (fun_code_x[["example_pos_end"]] + 1):length(unlist(fun_code_x[["code"]]))
      ]
    ))
  })

  # Clean double #' due to dontrun
  fun_code[["code_example"]] <- lapply(fun_code[["code_example"]], function(example) {
    gsub("#' #' ", "#' ", example)
  })


  return(fun_code)
}

#' Group df by column. One column to combine, all other, take the first.
#'
#' @param df dataframe to group
#' @param group_col Character. Name of the column to group_by
#' @param code_col Character. Name of column to combine
#' @noRd
group_code <- function(df, group_col, code_col) {
  lapply(
    unique(df[[group_col]]), # sec_title
    function(x) {
      group <- df[df[[group_col]] == x, ] # sec_title
      # Take first content of everything
      group_combined <- group[1, ]
      # Combine code column in a vector of character
      # from all code in the group
      comb_ex <- unlist(
        lapply(group[[code_col]], function(y) c(y, ""))
      )
      # Remove last extra empty line
      comb_ex <- comb_ex[-length(comb_ex)]
      group_combined[[code_col]] <- list(comb_ex)
      group_combined
    }
  ) %>%
    do.call("rbind", .)
}

#' Retrieve name of the package if there is a DESCRIPTION
#' @noRd
get_pkg_name <- function(pkg) {
  desc <- file.path(pkg, "DESCRIPTION")
  if (file.exists(desc)) {
    pkgname <- read.dcf(desc)[colnames(read.dcf(desc)) == "Package"]
  } else {
    pkgname <- basename(pkg)
  }
  pkgname
}


#' Create vignette header
#' @param pkg Path to package
#' @param vignette_name Name of the resulting vignette
#' @param yaml_options List of yaml options
#' @importFrom glue glue
#' @importFrom utils getFromNamespace
#' @noRd
create_vignette_head <- function(pkg, vignette_name, yaml_options = NULL) {
  pkgname <- get_pkg_name(pkg = pkg)

  # Get all yaml options except Title, output, editor_options
  yaml_options <- yaml_options[
    !names(yaml_options) %in% c("output", "title", "editor_options")
  ]

  if (is.null(names(vignette_name)) || names(vignette_name) == "") {
    vignette_title <- vignette_name
  } else {
    vignette_title <- names(vignette_name)
  }

  enc2utf8(
    glue(
      '---
title: ".{vignette_title}."
output: rmarkdown::html_vignette',
      ifelse(length(yaml_options) != 0,
        glue::glue_collapse(
          c(
            "",
            glue("{names(yaml_options)}: \"{yaml_options}\""), ""
          ),
          sep = "\n"
        ),
        "\n"
      ),
      'vignette: >
  %\\VignetteIndexEntry{.{asciify_name(vignette_name)}.}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r setup}
library(.{pkgname}.)
```
    ',
      .open = ".{", .close = "}."
    )
  )
}

#' Write code in files in utf8 simplified from usethis
#' @param path Path to file to write in
#' @param lines Character. Lines of code to write
#' @param append Logical. Whether to append to existing file
#' @noRd
write_utf8 <- function(path, lines, append = FALSE) {
  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")

  base::writeLines(
    enc2utf8(lines),
    sep = "\n",
    con = con,
    useBytes = TRUE
  )

  close(con)
}

#' Is the current project a recognized package
#' @param path Project path
#' @noRd
is_pkg_proj <- function(path = ".") {
  files <- list.files(normalizePath(path), full.names = TRUE)
  rprojfile <- files[grep("[.]Rproj$", files)]
  if (length(rprojfile) != 0) {
    rprojfile <- rprojfile[1]
    rproj_lines <- readLines(rprojfile)
    if (any(grepl("BuildType: Package", rproj_lines))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(NA)
  }
}

#' Clean names for any file and package
#' @param name Character to clean
#' @param to_pkg Transform all non authorized characters to dots for packages, instead of dash
#' @importFrom stringi stri_trans_general
#' @noRd
asciify_name <- function(name, to_pkg = FALSE) {
  # name <- "@.-y  _ p n@ é ! 1"

  # Escape all diacritics
  name <- stri_trans_general(name, id = "Latin-ASCII")

  cleaned_name <- gsub(
    "^[.]*|^-|-$", "",
    gsub(
      "-+", "-",
      gsub(
        "-_|_-", "-",
        gsub("[^([:alnum:]*_*-*)*]", "-", name)
      )
    )
  )
  # grepl("^[[:alpha:]][[:alnum:]_-]*$", cleaned_name)

  if (isTRUE(to_pkg)) {
    cleaned_name <- gsub(
      "[^a-zA-Z0-9]+", ".",
      gsub("^[0-9]+", "", cleaned_name)
    )
  } else {
    # asciify from {usethis} usethis:::asciify()
    cleaned_name <- tolower(
      gsub("[^a-zA-Z0-9_-]+", "-", cleaned_name)
    )
  }
  cleaned_name
}

#' A flavor of normalizePath() that unixifies all its output
#' @noRd
normalize_path_winslash <- function(...) normalizePath(..., winslash = "/")
