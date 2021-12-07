
#' Parse function code as tibble and get positions
#' @param x One row out of function parsed tibble
#' @importFrom parsermd rmd_node_code
#' @noRd
parse_fun <- function(x) { # x <- rmd_fun[3,]

  # browser()
  # code <- rmd_get_chunk(x)$code
  # parsermd::rmd_node_content(x)
  # parsermd::rmd_select(dev_parse, "description")[[1]] %>%
  #   parsermd::rmd_node_code()

  # parsermd::rmd_select(x, parsermd::has_type("ast"))[[1]] %>%
  #   parsermd::rmd_node_code(x)
  # parsermd::rmd_node_attr(x, "ast")

  code <- unlist(rmd_node_code(x[["ast"]]))
  # find function name
  fun_name <- stringr::str_extract(
    code[grep("function(\\s*)\\(", code)],
    "[\\w[.]]*(?=(\\s*)(<-|=)(\\s*)function)"
  ) %>%
    gsub(" ", "", .) # remove spaces

  # Clean extra space between #' and @
  code <- gsub(pattern = "#'\\s*@", "#' @", code)

  # Find start of function
  first_function_start <- grep("function(\\s*)\\(", code)[1]
  # Get all #'
  all_hastags <- grep("^#'", code)
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

  tibble::tibble(
    fun_name = fun_name[1],
    code = list(code),
    example_pos_start = example_pos_start,
    example_pos_end = example_pos_end
  )
}


#' Add function name to parsed_tbl ----
#' @param parsed_tbl tibble of a parsed Rmd
#' @param fun_names Names of functions in Rmd
#' @importFrom stats na.omit
#' @noRd
add_fun_to_parsed <- function(parsed_tbl, fun_names) {
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
                              grepl(regex_functions, parsed_tbl$label))

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
#' @importFrom parsermd rmd_node_code
#' @noRd
add_fun_code_examples <- function(parsed_tbl, fun_code) {
  # fun_code <- fun_code[!is.na(fun_code[["fun_name"]]), ]
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

  # Example in separate chunk
  which_parsed_ex <- which(!is.na(parsed_tbl$label) &
                             grepl(regex_example, parsed_tbl$label))
  rmd_ex <- parsed_tbl[which_parsed_ex, ]
  # No function, no example to add
  rmd_ex <- rmd_ex[!is.na(rmd_ex[["fun_name"]]), ]


  if (nrow(rmd_ex) != 0) {
    example_code <- lapply(
      seq_len(nrow(rmd_ex)),
      function(x) {
        tibble::tibble(
          fun_name = rmd_ex[x, ][["fun_name"]],
          # example_chunk = list(paste("#'", rmd_get_chunk(rmd_ex[x, ])$code))
          example_chunk = list(paste("#'", unlist(rmd_node_code(rmd_ex[x,][["ast"]]))))
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
        unlist(fun_code_x[["code"]]))
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


#' Create vignette header
#' @param pkg Path to package
#' @param vignette_name Name of the resulting vignette
#' @importFrom glue glue
#' @importFrom utils getFromNamespace
#' @noRd
create_vignette_head <- function(pkg, vignette_name) {
  pkgname <- basename(pkg)

  # Copied from usethis::use_vignette() to allow to not open vignette created
  # usethis:::use_dependency("knitr", "Suggests")
  getFromNamespace("use_dependency", "usethis")("knitr", "Suggests")
  getFromNamespace("use_description_field", "usethis")("VignetteBuilder", "knitr", overwrite = TRUE)
  usethis::use_git_ignore("inst/doc")

  enc2utf8(
    glue(
      '---
title: ".{vignette_name}."
output: rmarkdown::html_vignette
vignette: >
  %\\VignetteIndexEntry{.{vignette_name}.}
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
write_utf8 <- function(path, lines, append = FALSE){
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
