# Deal with noRd, examples and dontrun ----
#' stop() if @noRd but there is an example...
#' Or suggests \dontrun{}, but need to be taken into account in vignette

dummypackage <- tempfile("nordpackage")
dir.create(dummypackage)
dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
flat_file <- dev_file[grepl("flat_", dev_file)]
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))

usethis::with_project(dummypackage, {
  file.copy(
    system.file("tests-templates/dev-template-nord-but-example.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  usethis::use_mit_license("Statnmap")

  test_that("Deal with noRd but examples", {
    # No error
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          check = FALSE, open_vignette = FALSE
        )
      ),
      regexp = NA
    )

    # Check files
    expect_equal(length(list.files(file.path(dummypackage, "R"))), 1)
    expect_true(file.exists(file.path(dummypackage, "R", "my_norox2.R")))
    expect_true(file.exists(file.path(dummypackage, "vignettes", "get-started.Rmd")))

    # Check error
    # skip_on_os(os = c("windows", "solaris"))
    skip_on_cran()

    # Could not find function "my_norox2 in the vignette ?
    expect_error(rcmdcheck::rcmdcheck(dummypackage,
      quiet = TRUE,
      args = c("--no-manual")
    ))
  })
})

# Delete dummy package
unlink(dummypackage, recursive = TRUE)

# Test checks all templates with inflate dots (...) ----
alltemp <- tempfile("all.templates.inflate")
dir.create(alltemp)

create_choices_test <- fusen:::create_fusen_choices[!grepl("dev_history", fusen:::create_fusen_choices)]
for (pkgname in create_choices_test) {
  # pkgname <- create_choices_test[1]
  # No "additional" with create_fusen
  # {fusen} steps
  path_foosen <- normalize_path_winslash(file.path(alltemp, pkgname), mustWork = FALSE)
  dev_file <- create_fusen(path_foosen, template = pkgname, open = FALSE)
  flat_file <- dev_file[grepl("flat_", dev_file)]

  usethis::with_project(path_foosen, {
    fill_description(pkg = path_foosen, fields = list(Title = "Dummy Package"))
    suppressMessages(usethis::use_gpl_license())

    test_that("description is good", {
      expect_true(file.exists(file.path(path_foosen, "DESCRIPTION")))
      lines <- readLines(file.path(path_foosen, "DESCRIPTION"))
      expect_true(lines[1] == paste0("Package: ", basename(path_foosen)))
      expect_false(any(grepl("Jack", lines)))
      expect_false(any(grepl("Sébastien", lines)))
    })

    ok_template <- paste("Check returns OK for template", pkgname)
    test_that(ok_template, {
      # Do not check inside check if on CRAN
      # skip_on_os(os = c("windows", "solaris"))
      skip_on_cran()

      # extract the 'inflate' line in the flat file
      # Add pkg, check, quiet, args, overwrite
      # And inflate
      flat_lines <- readLines(flat_file)
      w.start <- grep("fusen::inflate\\(flat_file", flat_lines)
      w.end <- grep("\\)", flat_lines)
      w.end <- w.end[w.end >= w.start][1]
      inflate_lines <- flat_lines[w.start:w.end]

      if (!interactive()) {
        print(" ==== Not interactive ====")
        # Modify with extra values
        extra_params <- glue(
          'fusen::inflate(pkg = "{path_foosen}",
      check = FALSE, quiet = TRUE, args = c("--no-manual"),
      overwrite = TRUE, open_vignette = FALSE, '
        )
        to_inflate <- gsub("fusen::inflate\\(", extra_params, inflate_lines)

        # No redirection of stdout/stderr when non-interactive
        expect_error(
          suppressMessages(
            eval(parse(text = to_inflate))
          ),
          regexp = NA
        )

        # Run rcmdcheck
        # Do not check inside check if on CRAN
        # skip_on_os(os = c("windows", "solaris"))
        skip_on_cran()

        # If this check is run inside a not "--as-cran" check, then it wont work as expected
        check_out <- rcmdcheck::rcmdcheck(path_foosen,
          quiet = TRUE,
          args = c("--no-manual")
        )

        # No errors
        expect_true(length(check_out[["errors"]]) == 0)
        expect_true(length(check_out[["warnings"]]) <= 1)
        if (length(check_out[["warnings"]]) == 1) {
          expect_true(grepl("there is no package called", check_out[["warnings"]]))
        }

        skip_on_cran()
        expect_true(length(check_out[["notes"]]) <= 1)
        if (length(check_out[["notes"]]) %in% 1:2) {
          note_expected <- grepl("future file timestamps|Package vignette without corresponding tangle output", check_out[["notes"]])
          expect_true(all(note_expected))
          if (!all(note_expected)) {
            # Keep here to see the notes when CI fails
            expect_equal(check_out[["notes"]], expected = "no other note")
          }
        }
      } else {
        print(" ==== Interactive ====")
        # Modify with extra values
        extra_params <- glue(
          'fusen::inflate(pkg = "{path_foosen}",
      check = TRUE, quiet = TRUE, args = c("--no-manual"),
      overwrite = TRUE, open_vignette = FALSE, '
        )
        to_inflate <- gsub("fusen::inflate\\(", extra_params, inflate_lines)

        # No redirection of stdout/stderr when non-interactive
        expect_error(
          suppressMessages(
            eval(parse(text = to_inflate))
          ),
          regexp = NA
        )
      }

      skip_if_not(interactive())
      # Needs MASS, lattice, Matrix installed
      # quiet and checkdir
      checkdir <- normalize_path_winslash(
        file.path(alltemp, paste0("checkout", pkgname)),
        mustWork = FALSE
      )
      extra_params <- glue(
        'fusen::inflate(pkg = "{path_foosen}",
      check = TRUE, check_dir = "{checkdir}",
      quiet = TRUE, args = c("--no-manual"),
      overwrite = TRUE, open_vignette = FALSE, '
      )
      to_inflate <- gsub("fusen::inflate\\(", extra_params, inflate_lines)

      expect_error(
        suppressMessages(
          eval(parse(text = to_inflate))
        ),
        regexp = NA
      )

      # Should not be any errors with templates
      check_lines <- readLines(file.path(checkdir, paste0(basename(path_foosen), ".Rcheck"), "00check.log"))
      expect_equal(check_lines[length(check_lines)], "Status: OK")
      unlink(checkdir, recursive = TRUE)
    })
    # })
  })
} # end of template loop
# Delete dummy package
unlink(alltemp, recursive = TRUE)


# Test included datasets ----
# Should not return error at check in "full" template
dummypackage <- tempfile("dataset")
dir.create(dummypackage)
dummypackage <- normalizePath(dummypackage, winslash = "/")

checkdir <- tempfile("checkdirdata")
dir.create(checkdir)
checkdir <- normalizePath(checkdir, winslash = "/")


# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(
  add_flat_template(
    pkg = dummypackage,
    overwrite = TRUE,
    open = FALSE
  )
)
flat_file <- dev_file[grepl("flat_", dev_file)]

# Run development-dataset chunk
usethis::with_project(dummypackage, {
  test_that("included data can be read", {
    datafile <- file.path(dummypackage, "inst", "nyc_squirrels_sample.csv")
    expect_true(file.exists(datafile))

    flat_lines <- readLines(flat_file)
    # Change directory to current
    flatlines <- gsub("here::here()",
      paste0('"', dummypackage, '"'),
      flat_lines,
      fixed = TRUE
    )

    flatlines_chunk <- grep("```", flatlines)
    flatlines_chunk_data <- grep("```{r development-dataset}",
      flatlines,
      fixed = TRUE
    )
    flatlines_chunk_data_end <- flatlines_chunk[
      flatlines_chunk > flatlines_chunk_data
    ][1]
    lines_data <- flatlines[(flatlines_chunk_data + 1):
    (flatlines_chunk_data_end - 1)]


    # Can read data
    lines_only_read <- lines_data[grepl("read.csv", lines_data)]
    lines_only_read <- gsub(
      "datafile",
      paste0("'", datafile, "'"),
      lines_only_read
    )
    expect_error(eval(parse(text = lines_only_read)), regexp = NA)

    if (interactive()) {
      print(" === data interactive only ===")
      # load_all() in package check does not work
      expect_error(eval(parse(text = lines_data)), regexp = NA)
    }

    # Include dataset in "data/" and document
    usethis::use_data(nyc_squirrels, overwrite = TRUE)
    expect_true(file.exists(file.path("data", "nyc_squirrels.rda")))

    # Add documentation of dataset
    dir.create("R")
    cat("
#' NYC Squirrels dataset reduced
#'
#'
#' @format A data frame
#' @source \\url{https://github.com/rfordatascience/tidytuesday}
\"nyc_squirrels\"
", file = file.path("R", "datasets.R"))


    # _Inflate and check
    # skip_if_not(interactive())
    # Needs MASS, lattice, Matrix installed
    # quiet and checkdir
    usethis::use_gpl_license()

    w.start <- grep("fusen::inflate\\(flat_file", flat_lines)
    w.end <- grep("\\)", flat_lines)
    w.end <- w.end[w.end >= w.start][1]
    inflate_lines <- flat_lines[w.start:w.end]

    # Modify inflate
    extra_params <- glue(
      'fusen::inflate(pkg = "{dummypackage}",
      check = FALSE, check_dir = "{checkdir}",
      quiet = TRUE, args = c("--no-manual"),
      overwrite = TRUE, open_vignette = FALSE, '
    )
    to_inflate <- gsub("fusen::inflate\\(", extra_params, inflate_lines)

    expect_error(
      suppressMessages(
        eval(parse(text = to_inflate))
      ),
      regexp = NA
    )

    # Should not be any errors with templates
    # If this check is run inside a not "--as-cran" check,
    # then it wont work as expected
    check_out <- rcmdcheck::rcmdcheck(dummypackage,
      quiet = TRUE,
      args = c("--no-manual")
    )

    skip_on_cran()
    # No errors
    expect_true(length(check_out[["errors"]]) == 0)
    expect_true(length(check_out[["warnings"]]) <= 1)
    if (length(check_out[["warnings"]]) == 1) {
      expect_true(grepl("there is no package called", check_out[["warnings"]]))
    }

    unlink(checkdir, recursive = TRUE)
  })
})


unlink(dummypackage, recursive = TRUE)
unlink(checkdir, recursive = TRUE)




# Tests empty chunks ----
dummypackage <- tempfile("empty.chunks")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(
  add_flat_template(
    pkg = dummypackage,
    overwrite = TRUE, open = FALSE
  )
)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  file.copy(
    system.file(
      "tests-templates/dev-template-empty-not-function.Rmd",
      package = "fusen"
    ),
    flat_file,
    overwrite = TRUE
  )
  usethis::use_gpl_license()
  # Add cars data
  usethis::use_data(cars)

  test_that("inflate() output no error", {
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          vignette_name = "Get started", check = FALSE,
          open_vignette = FALSE
        )
      ),
      regexp = NA
    )

    # R files with chunk content - Name after title as function name is NA
    expect_equal(
      sort(list.files(file.path(dummypackage, "R"))),
      sort(c(
        "internal-variables.R", "my-data-doc.R",
        "my-pkg-doc.R", "onload.R"
      ))
    )
    pkgdoc <- file.path(dummypackage, "R", "my-pkg-doc.R")
    expect_true(file.exists(pkgdoc))
    pkgdoc_lines <- readLines(pkgdoc)
    expect_equal(length(pkgdoc_lines), 10)
    expect_equal(pkgdoc_lines[4], "\"_PACKAGE\"")
    expect_true(file.exists(
      file.path(
        dummypackage, "man",
        paste0(basename(dummypackage), "-package.Rd")
      )
    ))

    datadoc <- file.path(dummypackage, "R", "my-data-doc.R")
    expect_true(file.exists(datadoc))
    datadoc_lines <- readLines(datadoc)
    expect_equal(length(datadoc_lines), 13)
    expect_equal(datadoc_lines[13], "\"cars\"")
    expect_true(file.exists(file.path(dummypackage, "man", "cars.Rd")))

    myonloadfile <- file.path(dummypackage, "R", "onload.R")
    expect_true(file.exists(myonloadfile))
    # No dot in name
    myonload_lines <- readLines(myonloadfile)
    expect_true(all(myonload_lines[6:8] == c(
      ".onLoad <- function(libname, pkgname) {",
      "        the_message()",
      "}"
    )))
    expect_false(file.exists(
      file.path(dummypackage, "man", "onload.Rd")
    ))

    datavar <- file.path(dummypackage, "R", "internal-variables.R")
    expect_true(file.exists(datavar))
    datavar_lines <- readLines(datavar)
    expect_equal(length(datavar_lines), 3)
    expect_equal(datavar_lines[3], "colors <- c(\"#FFFFFF\", \"#F0F0F0\")")

    # No tests
    expect_false(file.exists(file.path(dummypackage, "tests")))

    checkdir <- tempfile("dircheck")
    # Disable checking for future file timestamps
    if (!interactive()) {
      expect_error(
        suppressMessages(
          inflate(
            pkg = dummypackage, flat_file = flat_file,
            vignette_name = "Get started", check = FALSE,
            quiet = TRUE,
            overwrite = TRUE, open_vignette = FALSE
          )
        ),
        regexp = NA
      )

      # Do not check inside check if on CRAN
      skip_on_os(os = c("windows", "solaris"))

      # If this check is run inside a not "--as-cran" check,
      #  then it wont work as expected
      check_out <- rcmdcheck::rcmdcheck(dummypackage,
        quiet = TRUE,
        args = c("--no-manual"),
        check_dir = checkdir
      )

      # No errors
      expect_true(length(check_out[["errors"]]) == 0)
      expect_true(length(check_out[["warnings"]]) <= 1)
      if (length(check_out[["warnings"]]) == 1) {
        expect_true(grepl(
          "there is no package called",
          check_out[["warnings"]]
        ))
      }

      # Notes are different on CRAN
      skip_on_cran()

      expect_true(length(check_out[["notes"]]) <= 1)
      if (length(check_out[["notes"]]) %in% 1:2) {
        note_expected <- grepl(
          "future file timestamps|Package vignette without corresponding tangle output",
          check_out[["notes"]]
        )
        expect_true(all(note_expected))
        if (!all(note_expected)) {
          # Keep here to see the notes when CI fails
          expect_equal(check_out[["notes"]], expected = "no other note")
        }
      }
    } else {
      expect_error(
        suppressMessages(
          inflate(
            pkg = dummypackage, flat_file = flat_file,
            vignette_name = "Get started", check = TRUE,
            check_dir = checkdir, quiet = TRUE,
            overwrite = TRUE, open_vignette = FALSE
          )
        ),
        regexp = NA
      )

      # Should not be any errors with templates in interactive
      check_lines <- readLines(
        file.path(
          checkdir, paste0(basename(dummypackage), ".Rcheck"),
          "00check.log"
        )
      )
      expect_equal(check_lines[length(check_lines)], "Status: OK")
      unlink(checkdir, recursive = TRUE)
    }
  })
})
unlink(dummypackage, recursive = TRUE)

# Tests r6 chunks ----
dummypackage <- tempfile("r6class")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(
  add_flat_template(
    pkg = dummypackage,
    overwrite = TRUE, open = FALSE
  )
)
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  file.copy(
    system.file("tests-templates/dev-template-r6class.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  usethis::use_gpl_license()
  # Add cars data
  usethis::use_data(cars)

  test_that("inflate() output no error with R6", {
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          vignette_name = "Get started", check = FALSE,
          open_vignette = FALSE,
          # To avoid having {R6} in suggests
          document = FALSE
        )
      ),
      regexp = NA
    )

    r6doc <- file.path(dummypackage, "R", "simple.R")
    expect_true(file.exists(r6doc))
    r6doc_lines <- readLines(r6doc)
    expect_equal(length(r6doc_lines), 9)
    expect_equal(r6doc_lines[4], "Simple <- R6::R6Class(\"Simple\",")

    r6doc <- file.path(dummypackage, "R", "simple2.R")
    expect_true(file.exists(r6doc))
    r6doc_lines <- readLines(r6doc)
    expect_equal(length(r6doc_lines), 9)
    expect_equal(r6doc_lines[4], "Simple2 <- R6Class(\"Simple2\",")
  })
})
unlink(dummypackage, recursive = TRUE)


# Depreaction of name and rmd ----
# Create a new project
dummypackage <- tempfile("inflate.deprecated")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

test_that("rmd and name are deprecated works", {
  usethis::with_project(dummypackage, {
    expect_error(
      suppressMessages(
        inflate(
          pkg = ".",
          # flat_file = flat_file,
          rmd = flat_file,
          vignette_name = "Get started",
          check = FALSE, document = TRUE,
          overwrite = TRUE, open_vignette = FALSE
        )
      ),
      regexp = "The `rmd` argument"
    )
    expect_error(
      suppressMessages(
        inflate(
          pkg = ".",
          flat_file = flat_file,
          # vignette_name = "Get started",
          name = "Get started",
          check = FALSE, document = TRUE,
          overwrite = TRUE, open_vignette = FALSE
        )
      ),
      regexp = "The `name` argument"
    )
  })
})
unlink(dummypackage, recursive = TRUE)

# Test No vignette ----
dummypackage <- tempfile("inflate.no.vignette")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  test_that("inflate() worked correctly", {
    expect_message(
      inflate(
        pkg = dummypackage, flat_file = flat_file,
        vignette_name = NA, check = FALSE,
        open_vignette = FALSE
      ),
      regexp = "no vignette created"
    )
    expect_equal(length(list.files(file.path(dummypackage, "vignettes"))), 0)

    expect_message(
      inflate(
        pkg = dummypackage, flat_file = flat_file,
        vignette_name = NULL, check = FALSE,
        open_vignette = FALSE
      ),
      regexp = "no vignette created"
    )
    expect_equal(length(list.files(file.path(dummypackage, "vignettes"))), 0)

    expect_message(
      inflate(
        pkg = dummypackage, flat_file = flat_file,
        vignette_name = "", check = FALSE,
        open_vignette = FALSE
      ),
      regexp = "no vignette created"
    )
    expect_equal(length(list.files(file.path(dummypackage, "vignettes"))), 0)

    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          vignette_name = "It works then", check = FALSE,
          open_vignette = FALSE
        )
      ),
      regexp = NA
    )
    expect_equal(list.files(file.path(dummypackage, "vignettes")), "it-works-then.Rmd")
  })
})
unlink(dummypackage, recursive = TRUE)

# Two funs same file - dev-template-two-fun-same-title.Rmd ----

# Create a new project
dummypackage <- tempfile("inflate.twofuns")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-two-fun-same-title.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("inflate() worked correctly", {
    # -- R files --
    my_median_file <- file.path(dummypackage, "R", "my_median.R")
    expect_true(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "R", "my_median2.R")
    expect_false(file.exists(my_median2_file))
    # chunk name
    my_chunk1_file <- file.path(dummypackage, "R", "fun_chunk1.R")
    expect_true(file.exists(my_chunk1_file))
    my_chunk1fun_file <- file.path(dummypackage, "R", "my_fun_chunk1.R")
    expect_false(file.exists(my_chunk1fun_file))
    my_chunk2fun_file <- file.path(dummypackage, "R", "my_fun_chunk2.R")
    expect_false(file.exists(my_chunk2fun_file))
    # same rdname
    my_rdname1_file <- file.path(dummypackage, "R", "same_rdname.R")
    expect_true(file.exists(my_rdname1_file))
    my_rdname1fun_file <- file.path(dummypackage, "R", "my_fun_rdname1.R")
    expect_false(file.exists(my_rdname1fun_file))
    my_rdname2fun_file <- file.path(dummypackage, "R", "my_fun_rdname2.R")
    expect_false(file.exists(my_rdname2fun_file))
    # same @filename
    my_filename1_file <- file.path(dummypackage, "R", "same_filename.R")
    expect_true(file.exists(my_filename1_file))
    my_filename1fun_file <- file.path(dummypackage, "R", "my_fun_filename1.R")
    expect_false(file.exists(my_filename1fun_file))
    my_filename2fun_file <- file.path(dummypackage, "R", "my_fun_filename2.R")
    expect_false(file.exists(my_filename2fun_file))
    # Same title
    r_lines <- readLines(my_median_file)
    expect_true(any(grepl("my_median <- function", r_lines)))
    expect_true(any(grepl("my_median2 <- function", r_lines)))
    # example at the right place
    expect_equal(
      r_lines[12:14],
      c("#' @examples", "#' my_median(2:20)", "#' my_median(1:12)")
    )
    expect_equal(
      r_lines[29:31],
      c("#' @examples", "#' my_median2(2:20)", "#' my_median2(1:12)")
    )
    # Same rdname
    r_lines <- readLines(my_rdname1_file)
    expect_true(any(grepl("my_fun_rdname1 <- function", r_lines)))
    expect_true(any(grepl("my_fun_rdname2 <- function", r_lines)))
    expect_equal(
      r_lines[13:15],
      c("#' @examples", "#' my_fun_rdname1(2:20)", "#' my_fun_rdname1(1:12)")
    )
    expect_equal(
      r_lines[21:25],
      c(
        "#' @rdname same_rdname",
        "#' @importFrom stats median", "#' @export",
        "#' @examples", "#' my_fun_rdname2(1:12)"
      )
    )
    # Same chunk name
    r_lines <- readLines(my_chunk1_file)
    expect_true(any(grepl("my_fun_chunk1 <- function", r_lines)))
    expect_true(any(grepl("my_fun_chunk2 <- function", r_lines)))
    # Same @filename
    r_lines <- readLines(my_filename1_file)
    expect_true(any(grepl("my_fun_filename1 <- function", r_lines)))
    expect_true(any(grepl("my_fun_filename2 <- function", r_lines)))
    # @filename cleaned in R file
    expect_false(any(grepl("@filename", r_lines)))

    # -- doc files --
    my_median_file <- file.path(dummypackage, "man", "my_median.Rd")
    expect_true(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "man", "my_median2.Rd")
    expect_true(file.exists(my_median2_file))
    # chunk name
    my_median_file <- file.path(dummypackage, "man", "my_fun_chunk1.Rd")
    expect_true(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "man", "my_fun_chunk2.Rd")
    expect_true(file.exists(my_median2_file))
    # rdname
    my_median_file <- file.path(dummypackage, "man", "my_fun_rdname1.Rd")
    expect_false(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "man", "my_fun_rdname2.Rd")
    expect_false(file.exists(my_median2_file))
    my_median3_file <- file.path(dummypackage, "man", "same_rdname.Rd")
    expect_true(file.exists(my_median3_file))
    # filename
    my_median_file <- file.path(dummypackage, "man", "my_fun_filename1.Rd")
    expect_true(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "man", "my_fun_filename2.Rd")
    expect_true(file.exists(my_median2_file))

    # -- test files --
    my_median_file <- file.path(dummypackage, "tests", "testthat", "test-my_median.R")
    expect_true(file.exists(my_median_file))
    my_median2_file <- file.path(dummypackage, "tests", "testthat", "test-my_median2.R")
    expect_false(file.exists(my_median2_file))
    # chunk name
    my_chunk1_file <- file.path(dummypackage, "tests", "testthat", "test-fun_chunk1.R")
    expect_true(file.exists(my_chunk1_file))
    my_chunk1fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_chunk1.R")
    expect_false(file.exists(my_chunk1fun_file))
    my_chunk2fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_chunk2.R")
    expect_false(file.exists(my_chunk2fun_file))
    # same rdname
    my_rdname1_file <- file.path(dummypackage, "tests", "testthat", "test-same_rdname.R")
    expect_true(file.exists(my_rdname1_file))
    my_rdname1fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_rdname1.R")
    expect_false(file.exists(my_rdname1fun_file))
    my_rdname2fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_rdname2.R")
    expect_false(file.exists(my_rdname2fun_file))
    # same @filename
    my_filename1_file <- file.path(dummypackage, "tests", "testthat", "test-same_filename.R")
    expect_true(file.exists(my_filename1_file))
    my_filename1fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_filename1.R")
    expect_false(file.exists(my_filename1fun_file))
    my_filename2fun_file <- file.path(dummypackage, "tests", "testthat", "test-my_fun_filename2.R")
    expect_false(file.exists(my_filename2fun_file))

    tests_lines <- readLines(my_median_file)
    expect_true(any(grepl("my_median", tests_lines)))
    expect_true(any(grepl("my_median2", tests_lines)))
    # Same rdname
    r_lines <- readLines(my_rdname1_file)
    expect_true(any(grepl("my_fun_rdname1", r_lines)))
    expect_true(any(grepl("my_fun_rdname2", r_lines)))
    # Same chunk name
    r_lines <- readLines(my_chunk1_file)
    expect_true(any(grepl("my_fun_chunk1", r_lines)))
    expect_true(any(grepl("my_fun_chunk2", r_lines)))
    # Same @filename
    r_lines <- readLines(my_filename1_file)
    expect_true(any(grepl("my_fun_filename1", r_lines)))
    expect_true(any(grepl("my_fun_filename2", r_lines)))
  })
})
unlink(dummypackage, recursive = TRUE)

# Test author and date ----
dummypackage <- tempfile("inflate.authors")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-author-date.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("author and date worked correctly", {
    # Check vignette content
    the_vignette <- file.path(dummypackage, "vignettes", "get-started.Rmd")
    expect_true(file.exists(the_vignette))
    content_vignette <- readLines(the_vignette)

    expect_true(any(
      grepl("^author: ", content_vignette, fixed = FALSE)
    ))
    expect_true(any(
      grepl("author: \"S\\u00e9bastien Rochette\"", content_vignette, fixed = TRUE)
    ))
    expect_true(any(
      grepl("^date: ", content_vignette, fixed = FALSE)
    ))
    expect_true(any(
      grepl("date: \"`r Sys.Date()`\"", content_vignette, fixed = TRUE)
    ))
  })
})

unlink(dummypackage, recursive = TRUE)

# Test unit tests only ----
dummypackage <- tempfile("inflate.tests")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests-only.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("unit tests only worked correctly", {
    # Check tests exist
    the_tests <- file.path(dummypackage, "tests", "testthat")
    expect_true(dir.exists(the_tests))
    expect_equal(length(list.files(the_tests)), 2)
    expect_true(file.exists(file.path(the_tests, "test-one-title-no-function-no-name.R")))
    expect_true(file.exists(file.path(the_tests, "test-the_test.R")))

    # Check other files
    the_r <- file.path(dummypackage, "R")
    expect_false(dir.exists(the_r))
    the_vignettes <- file.path(dummypackage, "vignettes")
    expect_true(dir.exists(the_vignettes))
    expect_true(file.exists(file.path(the_vignettes, "get-started.Rmd")))
  })
})

unlink(dummypackage, recursive = TRUE)

# Test unit tests only no section works ----
dummypackage <- tempfile("inflate.tests")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests-only-no-section.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  test_that("unit tests only no section worked correctly", {
    # no title may return
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          vignette_name = "Get started", check = FALSE,
          open_vignette = FALSE
        )
      ),
      regexp = NA # "Some `test` chunks can not be handled: tests-fails."
    )

    # Check tests do not exist
    the_tests <- file.path(dummypackage, "tests", "testthat")
    expect_true(dir.exists(the_tests))
    expect_equal(length(list.files(the_tests)), 1)
    expect_true(file.exists(file.path(the_tests, "test-fake-section-title.R")))

    # Check other files
    # R created just before tests but no functions
    the_r <- file.path(dummypackage, "R")
    expect_false(dir.exists(the_r))
    # vignette not created, as after tests Error
    the_vignettes <- file.path(dummypackage, "vignettes")
    expect_true(dir.exists(the_vignettes))
  })
})

unlink(dummypackage, recursive = TRUE)

# Test unit tests and examples only ----
dummypackage <- tempfile("inflate.tests")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-tests-examples-only.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  test_that("unit tests and examples only works", {
    # no title may return
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          vignette_name = "Get started", check = FALSE,
          open_vignette = FALSE
        )
      ),
      regexp = NA # "Some `test` chunks can not be handled: tests-fails."
    )

    # Check tests do not exist
    the_tests <- file.path(dummypackage, "tests", "testthat")
    expect_true(dir.exists(the_tests))
    expect_equal(length(list.files(the_tests)), 2)
    # expect_true(file.exists(file.path(the_tests, "test-fake-section-title.R")))

    # Check other files
    # R created just before tests but no functions
    the_r <- file.path(dummypackage, "R")
    expect_false(dir.exists(the_r))
    # vignette created with one example
    the_vignettes <- file.path(dummypackage, "vignettes")
    expect_true(dir.exists(the_vignettes))
    vignette_lines <- readLines(file.path(the_vignettes, "get-started.Rmd"))
    expect_length(grep("```{r examples}", vignette_lines, fixed = TRUE), 1)
  })
})

unlink(dummypackage, recursive = TRUE)

# Test some examples without functions and others with ----
dummypackage <- tempfile("inflate.examples")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-some-examples-without-functions.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  test_that("unit tests and examples only works", {
    # no title may return
    expect_message(
      inflate(
        pkg = dummypackage, flat_file = flat_file,
        vignette_name = "Get started", check = FALSE,
        open_vignette = FALSE
      ),
      regexp = "Some example chunks are not associated to any function"
    )

    # Check tests do not exist
    the_tests <- file.path(dummypackage, "tests", "testthat")
    expect_true(dir.exists(the_tests))
    expect_equal(length(list.files(the_tests)), 2)

    # Check other files
    # R created just before tests but no functions
    the_r <- file.path(dummypackage, "R")
    expect_true(dir.exists(the_r))
    expect_equal(length(list.files(the_r)), 1)

    # vignette created with 2 examples
    the_vignettes <- file.path(dummypackage, "vignettes")
    expect_true(dir.exists(the_vignettes))
    vignette_lines <- readLines(file.path(the_vignettes, "get-started.Rmd"))
    expect_length(grep("```{r examples-2}", vignette_lines, fixed = TRUE), 1)
    expect_length(grep("```{r examples-my_median}", vignette_lines, fixed = TRUE), 1)
  })
})

unlink(dummypackage, recursive = TRUE)

# vignette name and slug independent ----
dummypackage <- tempfile("vignette.name")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = c("Super title" = "01-Super Slug"), check = FALSE,
      open_vignette = FALSE
    )
  )

  the_vignette <- file.path(dummypackage, "vignettes", "01-super-slug.Rmd")
  test_that("vignette header is good", {
    expect_true(file.exists(the_vignette))
    the_vignette_lines <- readLines(the_vignette)
    expect_true(grepl("title:.*Super title.*", the_vignette_lines[2]))
    expect_true(any(grep("01-Super Slug", the_vignette_lines)))
  })
})

unlink(dummypackage, recursive = TRUE)

# Test "function ()" in documentation not read as a function ----

# Create a new project
dummypackage <- tempfile("inflate.fun.in.roxygen")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-word-function-in-doc.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )
  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = NA, check = FALSE
    )
  )

  test_that("inflate() worked correctly", {
    # Check only the first function is saved in a .R
    the_codes <- file.path(dummypackage, "R")
    expect_equal(list.files(the_codes), "my_function.R") # not c("add_one.R", "my_function.R")
    # Check that .R contains example
    code <- readLines(file.path(dummypackage, "R", "my_function.R"))
    expect_true(any(grepl("^#'\\s*my_function\\(x", code)))
  })
})

# Clean
unlink(dummypackage, recursive = TRUE)

# Two examples for one function ----
dummypackage <- tempfile("twoexamples")
dir.create(dummypackage)
dev_file <- add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE)
flat_file <- dev_file[grepl("flat_", dev_file)]
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))

usethis::with_project(dummypackage, {
  file.copy(
    system.file("tests-templates/dev-template-one-func-two-examples.Rmd", package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  usethis::use_mit_license("Statnmap")

  test_that("Deal with 2 examples for one function", {
    # No error
    expect_error(
      suppressMessages(
        inflate(
          pkg = dummypackage, flat_file = flat_file,
          check = FALSE, open_vignette = FALSE
        )
      ),
      regexp = NA
    )
  })

  lines <- readLines(file.path(dummypackage, "R", "my_twoexamples.R"))

  expect_equal(
    lines,
    c(
      "# WARNING - Generated by {fusen} from dev/flat_full.Rmd: do not edit by hand",
      "",
      "#' my_twoexamples",
      "#' @param x x",
      "#' @export",
      "#' @examples",
      "#' \\dontrun{",
      "#' my_twoexamples(10)",
      "#' }", "#'",
      "#' my_twoexamples(20)",
      "my_twoexamples <- function(x) {",
      "  x + 10",
      "}"
    )
  )
})

# Clean
unlink(dummypackage, recursive = TRUE)


# Test function name recognized with linebreaks between it and the function ----
# Create a new project
dummypackage <- tempfile("inflate.fun.in.roxygen")
dir.create(dummypackage)

# {fusen} steps
fill_description(pkg = dummypackage, fields = list(Title = "Dummy Package"))
dev_file <- suppressMessages(add_flat_template(pkg = dummypackage, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage, {
  # More complicated example for tests
  testfile <- "tests-templates/dev-template-function-name-linebreak.Rmd"
  file.copy(
    system.file(testfile, package = "fusen"),
    flat_file,
    overwrite = TRUE
  )

  suppressMessages(
    inflate(
      pkg = dummypackage, flat_file = flat_file,
      vignette_name = NA, check = FALSE
    )
  )

  test_that("inflate() worked correctly with linebreaks", {
    # Check that the functions are saved in a .R with the right name
    the_codes <- file.path(dummypackage, "R")
    expect_equal(sort(list.files(the_codes)), sort(paste0(c("real_name"), 1:11, ".R")))

    # Example is included in .R in the right place for the first 3 functions
    code_fct1 <- readLines(file.path(dummypackage, "R", "real_name1.R"))
    expect_true(all(code_fct1[5:8] == c(
      "#' @examples", "#' real_name1(1)", "real_name1 <-", "  function(x){"
    )))
    code_fct2 <- readLines(file.path(dummypackage, "R", "real_name2.R"))
    expect_true(all(code_fct2[5:10] == c(
      "#' @examples", "#' real_name2(2)", "", "# a comment", "real_name2 <- ", "  function(x){"
    )))
    code_fct3 <- readLines(file.path(dummypackage, "R", "real_name3.R"))
    expect_true(all(code_fct3[5:8] == c(
      "#' @examples", "#' real_name3(3)", "real_name3 <- # a comment", "  function(x){"
    )))
    code_fct10 <- readLines(file.path(dummypackage, "R", "real_name10.R"))
    expect_true(all(code_fct10[10:12] == c(
      "#' @examples", "#' real_name10(2)", "real_name10 <- function(x){"
    )))
    code_fct11 <- readLines(file.path(dummypackage, "R", "real_name11.R"))
    expect_true(all(code_fct11[5:8] == c(
      "#' @examples", "#' real_name11(1)", "real_name11 <-", "  function(x) {"
    )))


    # Example is included in .rd
    the_docs <- file.path(dummypackage, "man")
    expect_equal(
      sort(list.files(the_docs)),
      sort(c("real_name1.Rd", "real_name10.Rd", "real_name11.Rd"))
    )

    # Number of tests
    expect_equal(
      sort(list.files(file.path(dummypackage, "tests", "testthat"))),
      sort(c(
        "test-real_name1.R", "test-real_name11.R",
        "test-real_name2.R", "test-real_name3.R"
      ))
    )
  })
})

# Clean
unlink(dummypackage, recursive = TRUE)

# Test special character in directory names ----
dummypackage.special <- tempfile("dummypackage_@ (special)")
dir.create(dummypackage.special)

# {fusen} steps
test_that("fill_description renames package name if not clean", {
  expect_warning(
    desc_file <- fill_description(pkg = dummypackage.special, fields = list(Title = "Dummy Package")),
    "Your package was renamed: `dummypackage[.]special[.]"
  )

  desc_file_lines <- readLines(desc_file)
  expect_true(
    grepl(
      "dummypackage[.]special[.]",
      desc_file_lines[grepl("Package", desc_file_lines)][1]
    )
  )
  expect_false(
    grepl(
      "dummypackage_@ \\(special\\)",
      desc_file_lines[grepl("Package", desc_file_lines)][1]
    )
  )
})

dev_file <- suppressMessages(add_flat_template(pkg = dummypackage.special, overwrite = TRUE, open = FALSE))
flat_file <- dev_file[grepl("flat_", dev_file)]

usethis::with_project(dummypackage.special, {
  suppressMessages(
    inflate(
      pkg = dummypackage.special, flat_file = flat_file,
      vignette_name = "Get started", check = FALSE,
      open_vignette = FALSE
    )
  )

  test_that("inflate with special character in directory worked", {
    # config files
    config_file <- file.path(dummypackage.special, "dev", "config_fusen.yaml")
    config_content <- read_yaml(config_file)
    expect_equal(
      sort(config_content[["flat_full.Rmd"]][["R"]]),
      expected = sort(c("R/my_median.R", "R/my_other_median.R"))
    )
    expect_equal(
      sort(config_content[["flat_full.Rmd"]][["tests"]]),
      expected = sort(c(
        "tests/testthat/test-my_median.R",
        "tests/testthat/test-my_other_median.R"
      ))
    )
    expect_equal(
      sort(config_content[["flat_full.Rmd"]][["vignettes"]]),
      expected = sort(c("vignettes/get-started.Rmd"))
    )
  })
})

# Clean
unlink(dummypackage.special, recursive = TRUE)
