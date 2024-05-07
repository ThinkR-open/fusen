# get_package_structure works

    Code
      pkg_structure
    Output
      $flat_full.Rmd
      $flat_full.Rmd$flat_title
      [1] "flat_full.Rmd for working package"
      
      $flat_full.Rmd$path
      [1] "dev/flat_full.Rmd"
      
      $flat_full.Rmd$state
      [1] "🍏 active"
      
      $flat_full.Rmd$R
      $flat_full.Rmd$R$`R/my_median.R`
      [1] "👀 my_median"
      
      $flat_full.Rmd$R$`R/my_other_median.R`
      [1] "👀 my_other_median" "🙈 sub_median"     
      
      
      $flat_full.Rmd$tests
      [1] "tests/testthat/test-my_median.R"       "tests/testthat/test-my_other_median.R"
      
      $flat_full.Rmd$vignettes
      [1] "vignettes/get-started.Rmd"
      
      

---

    Code
      draw_the_tree(pkg_structure)
    Output
      
      - flat_full.Rmd
        - flat_title
            + flat_full.Rmd for working package
        - path
            + dev/flat_full.Rmd
        - state
            + 🍏 active
        - R
          - R/my_median.R
            + 👀 my_median
          - R/my_other_median.R
            + 👀 my_other_median
            + 🙈 sub_median
        - tests
            + tests/testthat/test-my_median.R
            + tests/testthat/test-my_other_median.R
        - vignettes
            + vignettes/get-started.Rmd
