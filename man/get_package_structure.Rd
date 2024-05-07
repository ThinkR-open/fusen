% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_package_structure.R
\name{get_package_structure}
\alias{get_package_structure}
\alias{draw_the_tree}
\title{Get structure and information of a 'fusen' built package for developers}
\usage{
get_package_structure(path, emoji = TRUE, silent = FALSE)

draw_the_tree(structure_list)
}
\arguments{
\item{path}{The path to the yaml file
to get the structure from}

\item{emoji}{Add emojis to the output}

\item{structure_list}{A list of information about the package as issued
from \verb{[get_package_structure()]}}
}
\value{
A list of information about the package
}
\description{
Get structure and information of a 'fusen' built package for developers

Draw a tree of the package structure in the console
}
\examples{
\dontrun{
# This only works inside a 'fusen' built package
pkg_structure <- get_package_structure()
draw_the_tree(pkg_structure)
}

# Example with a dummy package
dummypackage <- tempfile("drawpkg.structure")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(
  add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  pkg_structure <- get_package_structure()
  draw_the_tree(pkg_structure)
})
}