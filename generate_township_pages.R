# Generate one parameterized Quarto source page for every township.
# The full site render will convert these generated .qmd files to HTML in docs/townships/.
# Run from the PINs-to-Projects website project directory.

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

slugify <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_remove("^-") |>
    str_remove("-$")
}

keypins <- read_xlsx("../Merriman RA/ptax/Output/projects_checked_MAINFILE.xlsx") |>
  mutate(keypin = main_keypin)

townships <- keypins |>
  filter(!is.na(Township), Township != "", Township != "Chicago") |>
  distinct(Township) |>
  arrange(Township) |>
  pull(Township)

dir.create("townships", showWarnings = FALSE, recursive = TRUE)

# Remove previously generated sources so renamed or removed townships do not linger.
old_pages <- list.files("townships", pattern = "-projects\\.qmd$", full.names = TRUE)
if (length(old_pages) > 0) file.remove(old_pages)

template <- readLines("township_report_template.qmd", warn = FALSE)

walk(townships, function(place) {
  page <- template
  page <- sub(
    '^title: ".*"$',
    paste0('title: "', place, ' Township Projects"'),
    page
  )
  page <- sub(
    '^  township: ".*"$',
    paste0('  township: "', place, '"'),
    page
  )

  output_path <- file.path(
    "townships",
    paste0(slugify(place), "-projects.qmd")
  )

  writeLines(page, output_path, useBytes = TRUE)
})

message("Generated ", length(townships), " township pages in townships/.")
