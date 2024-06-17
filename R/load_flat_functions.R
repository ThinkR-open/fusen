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
#' # Use this command directly in the console
#' fusen::load_flat_functions()
#'
#' # Or choose a flat file to load functions from
#' load_flat_functions(flat_file = "dev/flat_full.Rmd")
#' load_flat_functions(flat_file = "dev/flat_clean_fusen_files.Rmd")
#' }
load_flat_functions <- function(flat_file, envir = globalenv()) {
  if (
    missing(flat_file) && requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("getSourceEditorContext")
  ) {
    curr_editor <- rstudioapi::getSourceEditorContext()
    current_file <- curr_editor$path

    if (!is.null(current_file)) {
      flat_file <- current_file
    }
  }

  if (!grepl("^(flat|dev).*[.](R|r|q)md$", basename(flat_file))) {
    stop(
      "Please provide a Rmd or qmd flat file to load functions from",
      " or open a flat file in the current IDE editor.",
      "\n'flat_file' name should start with 'flat' or 'dev'",
      " and end with '.Rmd' or '.qmd'."
    )
  }

  parsed_tbl <- lightparser::split_to_tbl(flat_file)

  which_parsed_fun <- which(
    !is.na(parsed_tbl$label) &
      grepl(regex_functions, parsed_tbl$label)
  )

  if (nrow(parsed_tbl) > 0 && length(which_parsed_fun) > 0) {
    content <- unlist(parsed_tbl[which_parsed_fun, ][["code"]])

    eval(parse(text = content), envir)

    cli_alert_success(
      paste0(
        "'function' chunks from '",
        flat_file,
        "' sourced in global env."
      )
    )
  } else {
    cli_alert_warning("Nothing to source")
  }

  return(invisible(flat_file))
}
