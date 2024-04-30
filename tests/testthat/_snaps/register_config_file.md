# register_all_to_config can append new files in keep section

    Code
      df_to_config(all_files, flat_file_path = "dev/flat_test.Rmd", state = "active", inflate_parameters = list(
        flat_file = "dev/flat_test.Rmd", vignette_name = "My new vignette", open_vignette = FALSE, check = FALSE,
        document = TRUE, overwrite = "yes", clean = "ask"))
    Message
      v R: R/to_keep.R was added to the config file
      v R: R/to_remove.R was added to the config file
      v tests: tests/testthat/test-zaza.R was added to the config file
    Output
      [1] "dev/config_fusen.yaml"

---

    Code
      update_one_group_yaml(all_files_new, complete_yaml = yaml::read_yaml(config_file), flat_file_path = "dev/flat_test.Rmd",
      state = "active", clean = TRUE, inflate_parameters = list(flat_file = "dev/flat_test.Rmd",
        vignette_name = "My new vignette", open_vignette = FALSE, check = FALSE, document = TRUE,
        overwrite = "yes", clean = TRUE))
    Message
      ! R: R/to_remove.R was removed from the config file and from the repository
      v R: R/to_add.R was added to the config file
    Output
      $path
      [1] "dev/flat_test.Rmd"
      
      $state
      [1] "active"
      
      $R
      [1] "R/to_keep.R" "R/to_add.R" 
      
      $tests
      [1] "tests/testthat/test-zaza.R"
      
      $vignettes
      character(0)
      
      $inflate
      $inflate$flat_file
      [1] "dev/flat_test.Rmd"
      
      $inflate$vignette_name
      [1] "My new vignette"
      
      $inflate$open_vignette
      [1] FALSE
      
      $inflate$check
      [1] FALSE
      
      $inflate$document
      [1] TRUE
      
      $inflate$overwrite
      [1] "yes"
      
      $inflate$clean
      [1] TRUE
      
      

