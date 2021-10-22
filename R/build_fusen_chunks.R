#' Build fusen chunks
#' 
#' Internal tool to build the chunk text
#'
#' @param function_name Name of the function to build the chunk text with
#'
#' @noRd
#' @examples
#' cat(build_fusen_chunks("pouet"))
build_fusen_chunks <- function(function_name, export = TRUE){
  paste(
    sep = "\n",
    sprintf("## %s", function_name),
    "    ",
    sprintf("```{r function-%s}", function_name),
    "#' Title",
    "#' ",
    "#' Description",
    "#' ",
    "#' @return",
    "#' ",
    {
      if (export){
        "#' @export"
      } else {
        "#' @noRd"
      }
    },
    sprintf("%s <- function(){", function_name),
    "    ",
    "}",
    "```",
    "  ",
    sprintf("```{r example-%s}", function_name),
    sprintf("%s()", function_name),
    "```",
    "  ",
    sprintf("```{r tests-%s}", function_name),
    sprintf("test_that(\"%s works\", {", function_name),
    sprintf("  expect_true(inherits(%s, \"function\")) ", function_name),
    "})",
    "```",
    "  "
  )
}
