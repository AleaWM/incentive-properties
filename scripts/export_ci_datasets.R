library(tidyverse)


# Run from the website root. Exports flat CSV files; no maps or website rendering.
# Adjust input paths here if your directory layout differs.
universe_path <- "Output/commercial_industrial_pin_universe_2026_09_21.csv"
history_path <- "../Merriman RA/ptax/Output/comm_ind_PINs_ever_2006to2024.csv"
assignments_path <- "Output/project_assignments_phase7_2026_09_22.csv"
sales_path <- "../dissertation/data/raw/Assessor_Parcel_Sales_20250105.csv"
bor_paths <- c("../Merriman RA/ptax/Output/borappeals_UPDATED.csv")
export_dir <- "Output/ci_exports"
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

collapse_values <- function(x) {
  x <- trimws(as.character(x))
  x <- sort(unique(x[!is.na(x) & nzchar(x)]))
  paste(x, collapse = "; ")
}
write_dataset <- function(x, name) {
  readr::write_csv(x, file.path(export_dir, paste0(name, ".csv")), na = "")
  message(name, ": ", nrow(x), " rows")
}

universe <- readr::read_csv(universe_path,
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::select(pin, pin10, municipality, township)
stopifnot(!anyDuplicated(universe$pin))

# Read only the fields needed for the 2024 snapshot and last paid bill.
history <- data.table::fread(history_path,
  select = c("pin", "year", "class", "fmv", "fmv_NA_flag",
    "incent_prop", "tax_bill_total", "tif_record_present"),
  colClasses = c(pin = "character"), showProgress = FALSE) |>
  tibble::as_tibble() |>
  dplyr::semi_join(universe, by = "pin")
last_bills <- history |>
  dplyr::filter(!is.na(year), !is.na(tax_bill_total), tax_bill_total > 0) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(last_positive_bill_year = max(year), .groups = "drop")
snapshot <- history |>
  dplyr::filter(year == 2024) |>
  dplyr::mutate(observed_2024 = TRUE,
    fmv = dplyr::if_else(fmv_NA_flag == 1, NA_real_, as.numeric(fmv)))
stopifnot(!anyDuplicated(snapshot$pin))
rm(history)

assignments <- readr::read_csv(assignments_path,
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::semi_join(universe, by = "pin") |>
  dplyr::transmute(pin, keypin = phase7_project_keypin,
    buyer = buyer_name, sale_document_number)
stopifnot(!anyDuplicated(assignments$pin))
sales <- data.table::fread(sales_path,
  select = c("pin", "year", "sale_document_num", "sale_buyer_name"),
  colClasses = "character", showProgress = FALSE) |>
  tibble::as_tibble() |>
  dplyr::transmute(pin = stringr::str_pad(gsub("-", "", trimws(pin)), 14, pad = "0"),
    sale_document_number = sale_document_num, buyer = sale_buyer_name,
    sale_year = year) |>
  dplyr::semi_join(assignments,
    by = c("pin", "sale_document_number", "buyer")) |>
  dplyr::group_by(pin, sale_document_number, buyer) |>
  dplyr::summarise(sale_year = collapse_values(sale_year), .groups = "drop")
assignments <- assignments |>
  dplyr::left_join(sales,
    by = c("pin", "sale_document_number", "buyer"),
    relationship = "many-to-one") |>
  dplyr::mutate(buyer_detail = dplyr::if_else(is.na(buyer) | buyer == "",
    NA_character_, paste0(buyer, " (purchase year: ",
      dplyr::coalesce(dplyr::na_if(sale_year, ""), "unavailable"), ")")))
rm(sales)

appeals <- read_csv(bor_paths) |>

  select("pin", "appealid", "appellant", "tax_year") |>
  dplyr::mutate(pin = stringr::str_pad(gsub("-", "", trimws(pin)), 14, pad = "0"),
    appellant = dplyr::na_if(trimws(appellant), ""),
    tax_year = suppressWarnings(as.integer(tax_year))) |>
  dplyr::semi_join(universe, by = "pin") |>
  dplyr::distinct() |>
  dplyr::group_by(pin) |>
  dplyr::filter(if (all(is.na(tax_year))) TRUE else
    tax_year == max(tax_year, na.rm = TRUE)) |>
  dplyr::summarise(appealid = collapse_values(appealid),
    appellant = collapse_values(appellant),
    appeal_detail = collapse_values(dplyr::if_else(is.na(appellant),
      NA_character_, paste0(appellant, " (appeal tax year: ",
        dplyr::coalesce(as.character(tax_year), "unavailable"), ")"))),
    .groups = "drop")

pins <- universe |>
  dplyr::left_join(snapshot, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(last_bills, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(assignments, by = "pin", relationship = "one-to-one") |>
  dplyr::left_join(appeals, by = "pin", relationship = "one-to-one") |>
  dplyr::mutate(status = dplyr::case_when(
    is.na(observed_2024) ~ "Not observed in 2024",
    is.na(class) ~ "Unknown 2024 class",
    class == 0 ~ "Exempt (class 0)",
    TRUE ~ "Other recorded class"))
stopifnot(nrow(pins) == nrow(universe), !anyDuplicated(pins$pin))
write_dataset(pins, "ci_pins")

# A key PIN can occur in more than one region; summarize separately within each.
make_projects <- function(region_type) {
  pins |>
    dplyr::filter(!is.na(.data[[region_type]]), .data[[region_type]] != "",
      !is.na(keypin), keypin != "") |>
    dplyr::group_by(region = .data[[region_type]], keypin) |>
    dplyr::summarise(region_type = region_type,
      pins_in_region = dplyr::n_distinct(pin),
      observed_2024 = sum(!is.na(observed_2024)),
      exempt_2024 = sum(status == "Exempt (class 0)"),
      incentive_class_pins_2024 = sum(class >= 600 & class < 900, na.rm = TRUE),
      tif_pins_2024 = dplyr::n_distinct(pin[!is.na(tif_record_present) &
        tif_record_present == 1]),
      last_positive_bill_years = collapse_values(dplyr::coalesce(
        as.character(last_positive_bill_year),
        "No positive bill observed (2006-2024)")),
      fmv_2024 = if (all(is.na(fmv))) NA_real_ else sum(fmv, na.rm = TRUE),
      appealid = collapse_values(appealid),
      appellant = collapse_values(appellant),
      buyer = collapse_values(buyer_detail), .groups = "drop") |>
    dplyr::mutate(project_identifier = dplyr::case_when(
      nzchar(appellant) & nzchar(appealid) ~ paste0(appellant, " - ", appealid),
      nzchar(appellant) ~ appellant, TRUE ~ keypin), .before = pins_in_region) |>
    dplyr::select(region_type, region, dplyr::everything()) |>
    dplyr::arrange(region, dplyr::desc(pins_in_region), keypin)
}
municipality_projects <- make_projects("municipality")
township_projects <- make_projects("township")
write_dataset(municipality_projects, "ci_municipality_projects")
write_dataset(township_projects, "ci_township_projects")

# One row per PIN. Project totals appear in prefixed columns for each region.
combined <- pins |>
  dplyr::left_join(municipality_projects |>
    dplyr::select(region, keypin, project_identifier, pins_in_region, fmv_2024,
      incentive_class_pins_2024, tif_pins_2024) |>
    dplyr::rename(municipality = region) |>
    dplyr::rename_with(~ paste0("municipality_project_", .x),
      -c(municipality, keypin)),
  by c("municipality", "keypin"), relationship = "many-to-one") |>
  dplyr::left_join(township_projects |>
    dplyr::select(region, keypin, project_identifier, pins_in_region, fmv_2024,
      incentive_class_pins_2024, tif_pins_2024) |>
    dplyr::rename(township = region) |>
    dplyr::rename_with(~ paste0("township_project_", .x),
      -c(township, keypin)),
  by =("township", "keypin"), relationship = "many-to-one")
stopifnot(nrow(combined) == nrow(pins), !anyDuplicated(combined$pin))
write_dataset(combined, "ci_combined")
