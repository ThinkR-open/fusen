# do not edit by hand

# Create a new project
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
pkg_name <- basename(dummypackage)

# add_flat_template ----
test_that("add_flat_template adds flat_template.Rmd and co.", {
  dev_file_path <- expect_error(add_flat_template(pkg = dummypackage, open = FALSE), 
               regexp = NA)
  flat_file <- dev_file_path[grepl("flat", dev_file_path)]
  
  expect_true(all(file.exists(dev_file_path)))
  expect_true(file.exists(file.path(dummypackage, "dev", "0-dev_history.Rmd")))
  expect_true(file.exists(file.path(dummypackage, ".here")))

  rbuildignore_file <- file.path(dummypackage, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("^dev$", rbuildignore_lines, fixed = TRUE)))
  expect_true(any(grepl("[.]here", rbuildignore_lines)))

  dev_lines <- readLines(flat_file)
  expect_true(length(grep(pkg_name, dev_lines)) == 1)

  # Second time message and new file
  expect_message(add_flat_template(pkg = dummypackage))
  expect_true(file.exists(file.path(dummypackage, "dev", "flat_full_2.Rmd")))
  # _New file has path changed in title and inflate
  lines_2 <- readLines(file.path(dummypackage, "dev", "flat_full_2.Rmd"))
  expect_length(grep(x = lines_2, pattern = "flat_full_2[.]Rmd"), 2)
})
unlink(dummypackage, recursive = TRUE)

# Test with .Rproj and no .here, it works ----
# Create a new project
dummypackage2 <- tempfile(pattern = "dummy2")
dir.create(dummypackage2)
cat("", file = file.path(dummypackage2, 'dummy.Rproj'))

# Add
dev_file_path <- add_flat_template(pkg = dummypackage2, open = FALSE)
flat_file <- dev_file_path[grepl("flat", dev_file_path)]
  
test_that("add_flat_template works with .Rproj and no .here", {
  expect_true(all(file.exists(dev_file_path)))
  expect_false(file.exists(file.path(dummypackage2, ".here")))

  rbuildignore_file <- file.path(dummypackage2, ".Rbuildignore")
  expect_true(file.exists(rbuildignore_file))
  rbuildignore_lines <- readLines(rbuildignore_file)
  expect_true(any(grepl("dev", rbuildignore_lines, fixed = TRUE)))
  expect_false(any(grepl("[.]here", rbuildignore_lines)))
})
unlink(dummypackage2, recursive = TRUE)

# Test "dev_history" template ----
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
# Add
test_that("add dev_history template works", {
  withr::with_dir(dummypackage, {
    
    dev_file_path <- expect_error(add_flat_template(pkg = dummypackage, template = "dev_history", open = FALSE), regexp = NA)
    expect_true(file.exists(dev_file_path))
    
    usethis::with_project(dummypackage, {
      # Extract and test the description chunk
      dev_lines <- readLines(dev_file_path)
      # Change path of project
      dev_lines <- gsub("here::here()",
                        paste0('"', dummypackage, '"'), dev_lines, 
                        fixed = TRUE)
      dev_parse <- parsermd::parse_rmd(dev_lines)
      desc_code <- tempfile("desc")
      parsermd::rmd_select(dev_parse, "description")[[1]] %>% 
        parsermd::rmd_node_code() %>% 
        cat(., sep = "\n", file = desc_code)
      # Execute code
      expect_error(source(desc_code), regexp = NA)
    })
    # expect_error(source(desc_code), regexp = NA)
    expect_true(file.exists(file.path(dummypackage, "DESCRIPTION")))
    expect_true(file.exists(file.path(dummypackage, "LICENSE")))
    expect_true(file.exists(file.path(dummypackage, "LICENSE.md")))
    
  })
})

unlink(dummypackage)
  
# Add failed with malformed package name ----
# Create a new project
dummypackage3 <- tempfile(pattern = "dummy_3")
dir.create(dummypackage3)
cat("", file = file.path(dummypackage3, 'dummy.Rproj'))

# Add
test_that("add_flat_template fails", {
  expect_error(add_flat_template(pkg = dummypackage3, open = FALSE), "package name")
})
unlink(dummypackage3, recursive = TRUE)

# More complicated example for tests
# This will render the Rmd template that is supposed to build a package
# But we need to be inside a project,
# in the correct working directory,
# with the correct here()

# Test all templates to knit ----
all_templates <- c("full", "minimal", "additional", "teaching") # "dev_history"

for (template in all_templates) {
  dummypackage4 <- tempfile(pattern = "dummy4")
  dir.create(dummypackage4)
  # Add
  dev_file_path <- add_flat_template(pkg = dummypackage4, template = template, open = FALSE)
  flat_file <- dev_file_path[grepl("flat", dev_file_path)]
  
  # Change lines asking for pkg name
  lines_template <- readLines(system.file("tests-templates/dev-template-tests.Rmd", package = "fusen"))
  lines_template[grepl("<my_package_name>", lines_template)] <-
    gsub("<my_package_name>", basename(dummypackage4),
         lines_template[grepl("<my_package_name>", lines_template)])
  cat(enc2utf8(lines_template), file = flat_file, sep = "\n")
  
  withr::with_dir(dummypackage4, {
    usethis::proj_set(dummypackage4)
    here:::do_refresh_here(dummypackage4)
    
    if (rmarkdown::pandoc_available("1.12.3")) {
      rmarkdown::render(
        input = file.path(dummypackage4, "dev", paste0("flat_", template, ".Rmd")),
        output_file = file.path(dummypackage4, "dev", paste0("flat_", template, ".html")),
        envir = new.env(), quiet = TRUE)
    }
  })
  
  test_that(paste0("template", template, "runs as markdown"), {
    expect_true(file.exists(file.path(dummypackage4, "dev", paste0("flat_", template, ".Rmd"))))
    if (template %in% c("full", "minimal")) {
        expect_true(file.exists(file.path(dirname(flat_file), "0-dev_history.Rmd")))
    }
    
    if (rmarkdown::pandoc_available("1.12.3")) {
      expect_true(file.exists(file.path(dummypackage4, "DESCRIPTION")))
      expect_true(file.exists(file.path(dummypackage4, "LICENSE")))
      if (template %in% c("full", "teaching")) {
        expect_true(file.exists(file.path(dummypackage4, "inst", "nyc_squirrels_sample.csv")))
      }
      expect_true(file.exists(file.path(dummypackage4, "dev", paste0("flat_", template, ".html"))))
    }
  })
  unlink(dummypackage4, recursive = TRUE)
} # end of loop on template knit


# Test other names works ----
dummypackage <- tempfile(pattern = "dummy")
dir.create(dummypackage)
# Add
test_that("Other flat_name works", {
  dev_file_path <- expect_error(
    add_flat_template(pkg = dummypackage, flat_name = "hello", open = FALSE),
    regexp = NA)
  expect_true(file.exists(file.path(dummypackage, "dev/flat_hello.Rmd")))
})
# Delete dummy package
unlink(dummypackage, recursive = TRUE)


