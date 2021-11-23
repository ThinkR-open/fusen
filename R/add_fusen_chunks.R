# do not edit by hand

#' Add `{fusen}` chunks
#'
#' Create `{fusen}` chunks inside your Rmd
#'
#' @param function_name Name of the function to create.
#'     If NULL (the  default), the user will be prompted to enter it.
#' @param export Should the function be exported?
#'     Default is `getOption("fusen.export.functions")`. If NULL, the
#'     user will be prompted to enter it.
#'
#' @export
#'
#' @return A list with the context and the content, invisibly.
#'
#' @examples
#' add_fusen_chunks("this")
add_fusen_chunks <- function(function_name = NULL,
                             export = getOption("fusen.export.functions")) {
  if (
    requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("getSourceEditorContext") &&
      rstudioapi::hasFun("insertText")
  ) {
    # This will allow to interactively have the function name
    if (is.null(function_name)) {
      if (rstudioapi::hasFun("showPrompt")) {
        function_name <- rstudioapi::showPrompt("{fusen}", "Enter the function name")
      } else {
        function_name <- readline("Enter the function name: ")
      }
    }

    if (is.null(export)) {
      if (rstudioapi::hasFun("showQuestion")) {
        export <- rstudioapi::showQuestion("{fusen}", "Should the function be exported?", ok = "yes", cancel = "no")
      } else {
        export <- readline("Should the function be exported? (y/n) ") == "y"
      }
    }

    curr_editor <- rstudioapi::getSourceEditorContext()
    if (!grepl("\\.Rmd$", curr_editor$path)) {
      stop("fusen chunks can only be added inside a Rmd file.")
    }

    s <- curr_editor$selection

    # What happens if the user has selected something in the Rmd?
    # Throw an error
    if (nchar(s[[1L]]$text) != 0L) {
      stop("fusen chunks can't be inserted on top of selected text.")
    }

    chunks <- build_fusen_chunks(
      function_name,
      export
    )

    rstudioapi::insertText(
      location = s[[1L]]$range$start,
      text = chunks
    )

    return(
      invisible(
        list(
          context = curr_editor,
          chunk_text = chunks
        )
      )
    )
  }
}
