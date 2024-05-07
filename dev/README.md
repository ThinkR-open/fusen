
# Here is a map of the architecture of the ‘fusen’ project

``` r
pkgload::load_all()
withr::with_dir(here::here(), {
  pkg_structure <- get_package_structure()
  draw_the_tree(pkg_structure)
})
```

── /mnt/Data/github/ThinkR-open/fusen/dev/config\_fusen.yaml
─────────────────────────────────────────────────────────────
── Reading NAMESPACE file
───────────────────────────────────────────────────────────────────────────────────────────────
── flat\_addins.Rmd
──────────────────────────────────────────────────────────────────────────────────────────────────────
── flat\_create\_flat.Rmd
─────────────────────────────────────────────────────────────────────────────────────────────────
── flat\_deal\_with\_flat\_files.Rmd
────────────────────────────────────────────────────────────────────────────────────────
── flat\_get\_package\_structure.Rmd
───────────────────────────────────────────────────────────────────────────────────────
── flat\_history\_core.Rmd
────────────────────────────────────────────────────────────────────────────────────────────────
── flat\_history\_maintain.Rmd
────────────────────────────────────────────────────────────────────────────────────────────
── flat\_inflate\_all.Rmd
─────────────────────────────────────────────────────────────────────────────────────────────────
── flat\_inflate\_all\_utils.Rmd
───────────────────────────────────────────────────────────────────────────────────────────
── flat\_init\_share\_on\_github.Rmd
────────────────────────────────────────────────────────────────────────────────────────
── flat\_register\_config\_file.Rmd
────────────────────────────────────────────────────────────────────────────────────────
── keep
─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  - flat\_addins.Rmd
      - flat\_title
          - addins
      - path
          - dev/flat\_addins.Rmd
      - state
          - 🍏 active
      - R
          - R/build\_fusen\_chunks.R
              - 👀 add\_fusen\_chunks
              - 🙈 build\_fusen\_chunks
      - tests
          - tests/testthat/test-build\_fusen\_chunks.R
      - vignettes
  - flat\_create\_flat.Rmd
      - flat\_title
          - dev\_history.Rmd empty
      - path
          - dev/flat\_create\_flat.Rmd
      - state
          - 🍏 active
      - R
          - R/add\_flat\_template.R
              - 👀 add\_additional
              - 👀 add\_minimal\_flat
              - 👀 add\_minimal\_package
              - 👀 add\_full
              - 👀 add\_dev\_history
              - 👀 add\_flat\_template
              - 🙈 local\_file\_ignore
      - tests
          - tests/testthat/test-add\_flat\_template.R
      - vignettes
  - flat\_deal\_with\_flat\_files.Rmd
      - flat\_title
          - flat\_deal\_with\_flat\_files.Rmd empty
      - path
          - dev/flat\_deal\_with\_flat\_files.Rmd
      - state
          - 🍏 active
      - R
          - R/deprecate\_flat\_file.R
              - 👀 deprecate\_flat\_file
          - R/rename\_flat\_file.R
              - 👀 rename\_flat\_file
      - tests
          - tests/testthat/test-rename\_flat\_file.R
          - tests/testthat/test-deprecate\_flat\_file.R
      - vignettes
          - vignettes/deal-with-a-fusen-flat-file.Rmd
  - flat\_get\_package\_structure.Rmd
      - flat\_title
          - flat\_get\_package\_structure.Rmd empty
      - path
          - dev/flat\_get\_package\_structure.Rmd
      - state
          - 🍏 active
      - R
          - R/get\_all\_created\_funs.R
              - 👀 get\_all\_created\_funs
          - R/get\_package\_structure.R
              - 👀 get\_package\_structure
              - 👀 draw\_the\_tree
      - tests
          - tests/testthat/test-get\_package\_structure.R
          - tests/testthat/test-get\_all\_created\_funs.R
      - vignettes
          - vignettes/draw-a-map-of-your-package-files-and-functions.Rmd
  - flat\_history\_core.Rmd
      - flat\_title
          - dev\_history.Rmd
      - path
          - dev/flat\_history\_core.Rmd
      - state
          - 🛑 inactive
      - R
          - R/fill\_description.R
              - 👀 fill\_description
          - R/inflate.R
              - 👀 inflate
              - 🙈 create\_functions\_all
              - 🙈 get\_functions\_tests
              - 🙈 create\_r\_files
              - 🙈 create\_tests\_files
              - 🙈 create\_vignette
      - tests
          - tests/testthat/test-fill\_description.R
          - tests/testthat/test-inflate-part1.R
          - tests/testthat/test-inflate-part2.R
      - vignettes
          - vignettes/How-to-use-fusen.Rmd
  - flat\_history\_maintain.Rmd
      - flat\_title
          - flat\_history\_maintain.Rmd for working package
      - path
          - dev/flat\_history/flat\_history\_maintain.Rmd
      - state
          - 🛑 deprecated
      - R
      - tests
      - vignettes
          - vignettes/Maintain-packages-with-fusen.Rmd
  - flat\_inflate\_all.Rmd
      - flat\_title
          - flat\_inflate\_all.Rmd empty
      - path
          - dev/flat\_inflate\_all.Rmd
      - state
          - 🍏 active
      - R
          - R/inflate\_all.R
              - 👀 inflate\_all
              - 👀 inflate\_all\_no\_check
      - tests
          - tests/testthat/test-inflate\_all.R
      - vignettes
          - vignettes/inflate-all-your-flat-files.Rmd
  - flat\_inflate\_all\_utils.Rmd
      - flat\_title
          - flat\_inflate\_all\_utils.Rmd empty
      - path
          - dev/flat\_inflate\_all\_utils.Rmd
      - state
          - 🍏 active
      - R
          - R/inflate\_all\_utils.R
              - 🙈 pre\_inflate\_all\_diagnosis
              - 🙈 read\_inflate\_params
          - R/pre\_inflate\_all\_diagnosis\_eval.R
              - 🙈 pre\_inflate\_all\_diagnosis\_eval
      - tests
          - tests/testthat/test-pre\_inflate\_all\_diagnosis\_eval.R
          - tests/testthat/test-inflate\_all\_utils.R
      - vignettes
  - flat\_init\_share\_on\_github.Rmd
      - flat\_title
          - flat\_init\_share\_on\_github.Rmd empty
      - path
          - dev/flat\_init\_share\_on\_github.Rmd
      - state
          - 🍏 active
      - R
          - R/init\_share\_on\_github.R
              - 👀 init\_share\_on\_github
      - tests
          - tests/testthat/test-init\_share\_on\_github.R
      - vignettes
          - vignettes/share-on-a-github-website.Rmd
  - flat\_register\_config\_file.Rmd
      - flat\_title
          - flat\_df\_to\_config.Rmd empty
      - path
          - dev/flat\_register\_config\_file.Rmd
      - state
          - 🍏 active
      - R
          - R/register\_config\_file.R
              - 👀 check\_not\_registered\_files
              - 🙈 guess\_flat\_origin
              - 🙈 get\_list\_paths
              - 🙈 df\_to\_config
              - 🙈 write\_yaml\_verbatim
              - 🙈 files\_list\_to\_vector
              - 🙈 update\_one\_group\_yaml
              - 👀 register\_all\_to\_config
              - 🙈 deal\_with\_registered\_keep
      - tests
          - tests/testthat/test-register\_config\_file.R
      - vignettes
          - vignettes/register-files-in-config.Rmd
  - keep
      - path
          - keep
      - state
          - 🍏 active
      - R
          - R/addins.R
              - 🙈 addin\_add\_template
          - R/create\_fusen\_rsproject.R
              - 👀 create\_fusen
              - 🙈 create\_fusen\_gui
          - R/fusen-package.R
              - 🙈
          - R/globals.R
              - 🙈
          - R/inflate-utils.R
              - 🙈 parse\_fun
              - 🙈 add\_names\_to\_parsed
              - 🙈 parse\_test
              - 🙈 add\_fun\_code\_examples
              - 🙈 group\_code
              - 🙈 get\_pkg\_name
              - 🙈 create\_vignette\_head
              - 🙈 write\_utf8
              - 🙈 is\_pkg\_proj
              - 🙈 asciify\_name
              - 🙈 clean\_function\_name
              - 🙈 normalize\_path\_winslash
              - 🙈 document\_and\_check\_pkg
          - R/load\_flat\_functions.R
              - 👀 load\_flat\_functions
          - R/utils-pipe.R
              - 🙈
      - tests
          - tests/testthat/test-user-story.R
          - tests/testthat/test-create\_fusen\_rsproject.R
          - tests/testthat/test-inflate\_qmd.R
          - tests/testthat/test-inflate\_utils.R
          - tests/testthat/test-load\_flat\_functions.R
          - tests/testthat/test-skeleton.R
      - vignettes
          - vignettes/tips-and-tricks.Rmd
