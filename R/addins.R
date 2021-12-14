#' Addin to add a new template
#' @noRd
addin_add_template <- function() {
  if (
    requireNamespace("rstudioapi") &&
    rstudioapi::isAvailable()
  ) {
    # This will allow to interactively have the function name
    if (rstudioapi::hasFun("showPrompt")) {
      template <- rstudioapi::showPrompt(
        title = "Enter the flat template type",
        message = "e.g. add = additional; min = minimal;",
        default = "additional")
    } else {
      template <- readline("Enter the flat template type: ")
    }
    if (rstudioapi::hasFun("showPrompt")) {
      flat_name <- rstudioapi::showPrompt(
        title = "Choose the function name",
        message = "Name is used to pre-fill the template",
        default = "my_fun")
    } else {
      flat_name <- readline("Enter the function name: ")
    }

    add_flat_template(template = template, flat_name = flat_name)
  }
}
