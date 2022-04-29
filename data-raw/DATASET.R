## code to prepare `DATASET` dataset goes here

# Create fake dataset using internal ThinkR package
library(dplyr)
library(fakir)
database <- fake_ticket_client(vol = 1500, n = 500, seed = 4321, split = TRUE)

clients <- database$clients %>%
  arrange(id_dpt, departement) %>%
  tidyr::fill(departement) %>%
  mutate(
    entry_year = lubridate::year(entry_date),
    age_class = cut(age,
      breaks = c(18, 25, 40, 55, 70, 100),
      include.lowest = TRUE
    )
  )

# usethis::use_data(clients, overwrite = TRUE)

readr::write_csv(clients, "inst/clients.csv")

# usethis::use_data("DATASET")
