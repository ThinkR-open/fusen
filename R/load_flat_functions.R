#' Load the code of all 'function' chunk in a flat file
#'
#' @param flat_file Path to the flat to load functions from
#' @inheritParams base::eval
#'
#' @return Path to flat file loaded.
#' Used for side effect: Load functions in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#' load_flat_functions(flat_file = "dev/flat_full.Rmd")
#' load_flat_functions(flat_file = "dev/flat_clean_fusen_files.Rmd")
#' }
load_flat_functions <- function(flat_file, envir = globalenv()) {
  if (missing(flat_file) && requireNamespace("rstudioapi") && rstudioapi::isAvailable() &&
    rstudioapi::hasFun("documentPath")) {
    current_file <- rstudioapi::documentPath()
    if (!is.null(current_file) && grepl("^flat.*[.](R|r|q)md$", basename(current_file))) {
      flat_file <- current_file
    }
  }

  parsed_flat_file <- parse_rmd(flat_file)
  parsed_tbl <- as_tibble(parsed_flat_file)
  which_parsed_fun <- which(!is.na(parsed_tbl$label) &
    grepl(regex_functions, parsed_tbl$label))

  if (nrow(parsed_tbl) > 0) {
    # to_source <- tempfile()
    content <- unlist(rmd_node_code(parsed_tbl[which_parsed_fun, ][["ast"]]))

    eval(parse(text = content), envir)
    # cat(content, file = to_source)
    #
    # source(to_source, ...)
    # file.remove(to_source)
    cli_alert_success(paste0("'function' chunks from '", flat_file, "' sourced in global env."))
  } else {
    cli_alert_warning("Nothing to source")
  }

  return(invisible(flat_file))
}
