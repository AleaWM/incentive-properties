library(quarto)
library(dplyr)
library(stringr)
library(purrr)
library(fs)

municipality_names <- project_data |>
  distinct(municipality) |>
  pull(municipality)

dir_create("municipality")

walk(municipality_names, function(muni) {
  
  slug <- muni |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_remove_all("^-|-$")
  
  quarto_render(
    input = "municipality-template.qmd",
    execute_params = list(
      municipality = muni
    ),
    output_file = paste0(slug, ".html"),
    output_dir = "municipality"
  )
})