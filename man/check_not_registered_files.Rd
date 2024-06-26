% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_config_file.R
\name{check_not_registered_files}
\alias{check_not_registered_files}
\title{Show in a table files that are not already registered in the yaml config file}
\usage{
check_not_registered_files(
  path = ".",
  config_file,
  guess = TRUE,
  to_csv = TRUE,
  open = FALSE
)
}
\arguments{
\item{path}{Path to package to check for not registered files}

\item{config_file}{Path to the configuration file}

\item{guess}{Logical. Guess if the file was inflated by a specific flat file}

\item{to_csv}{Logical. Whether to store along the config file, the outputs in a csv for the user to clean it manually}

\item{open}{Logical. Whether to open the csv of unregistered files.}
}
\value{
Path to csv file if \code{to_csv} is TRUE. \code{dput()} of the dataframe otherwise.
}
\description{
If user start their package without 'fusen' or with version < 0.4, they need to create the config file, with already existing functions.
}
\examples{
\dontrun{
# Run this on the current package in development
out_csv <- check_not_registered_files()
file.edit(out_csv)
}

# Or you can try on the reproducible example
dummypackage <- tempfile("clean")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]
# Inflate once
usethis::with_project(dummypackage, {
  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  # Add a not registered file to the package
  cat("# test R file\n", file = file.path(dummypackage, "R", "to_keep.R"))

  # Use the function to check the list of files
  out_csv <- check_not_registered_files(dummypackage)
  out_csv
  # Read the csv to see what is going on
  content_csv <- read.csv(out_csv, stringsAsFactors = FALSE)
  content_csv
  # Keep it all or delete some files, and then register all remaining
  out_config <- register_all_to_config()
  out_config
  # Open the out_config file to see what's going on
  yaml::read_yaml(out_config)
})
unlink(dummypackage, recursive = TRUE)
}
\seealso{
\code{\link[=register_all_to_config]{register_all_to_config()}} for automatically registering all files already present in the project,
\code{\link[=inflate_all]{inflate_all()}} to inflate every flat files according to the configuration file.
}
