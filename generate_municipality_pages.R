# Generate one parameterized Quarto source page for every municipality.
# The full site render will convert these generated .qmd files to HTML in docs/municipalities/.
# Run from the PINs-to-Projects website project directory.

library(readr)
library(dplyr)
library(stringr)
library(purrr)

slugify <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_remove("^-") |>
    str_remove("-$")
}

comm_ind <- read_csv(
  "../Merriman RA/ptax/Output/comm_ind_PINs_2011to2022_timeseries.csv",
  show_col_types = FALSE
) |>
  filter(year == 2022)

municipalities <- comm_ind |>
  filter(!is.na(clean_name), clean_name != "") |>
  distinct(clean_name) |>
  arrange(clean_name) |>
  pull(clean_name)

dir.create("municipalities", showWarnings = FALSE, recursive = TRUE)

# Remove previously generated sources so renamed or removed municipalities do not linger.
old_pages <- list.files(
  "municipalities",
  pattern = "-projects\\.qmd$",
  full.names = TRUE
)
if (length(old_pages) > 0) file.remove(old_pages)

template <- readLines("municipality_report_template.qmd", warn = FALSE)

walk(municipalities, function(place) {
  page <- template
  page <- sub(
    '^title: ".*"$',
    paste0('title: "', place, ' Municipality Projects"'),
    page
  )
  page <- sub(
    '^  municipality: ".*"$',
    paste0('  municipality: "', place, '"'),
    page
  )

  output_path <- file.path(
    "municipalities",
    paste0(slugify(place), "-projects.qmd")
  )

  writeLines(page, output_path, useBytes = TRUE)
})

message(
  "Generated ", length(municipalities),
  " municipality pages in municipalities/."
)
