# Build auditable methodology and manually checked PIN-to-project inputs.
# Outputs are dated so earlier crosswalks remain intact.
# Run from the CookCounty-PropertyTaxes project root.

library(tidyverse)
library(readxl)
library(janitor)

output_dir <- "Output"
methodology_dir <- file.path(output_dir, "Combined Methodology Worksheets")
checked_dir <- file.path(output_dir, "Pin to Project Files")
output_date <- "2026_09_21"

write_csv_auditable <- function(data, path) {
  tryCatch(
    readr::write_csv(data, path),
    error = function(e) {
      if (file.exists(path)) {
        warning("Preserved existing output because it is locked or unavailable for writing: ", path)
        invisible(path)
      } else stop(e)
    }
  )
}

normalize_pin <- function(x) {
  cleaned <- str_remove_all(str_trim(as.character(x)), "-")
  cleaned <- if_else(str_detect(cleaned, "^[0-9]{13}$"), str_pad(cleaned, 14, side = "left", pad = "0"), cleaned)
  if_else(str_detect(cleaned, "^[0-9]{14}$"), cleaned, NA_character_)
}

infer_township <- function(file_name) {
  x <- str_to_lower(file_name)
  case_when(
    str_detect(x, "bloom") ~ "Bloom", str_detect(x, "bremen") ~ "Bremen",
    str_detect(x, "berwyn") ~ "Berwyn", str_detect(x, "calumet") ~ "Calumet",
    str_detect(x, "cicero") ~ "Cicero", str_detect(x, "lemont") ~ "Lemont",
    str_detect(x, "lyons") ~ "Lyons", str_detect(x, "oak[ _-]*park") ~ "Oak Park",
    str_detect(x, "orland") ~ "Orland", str_detect(x, "palos") ~ "Palos",
    str_detect(x, "proviso") ~ "Proviso", str_detect(x, "rich") ~ "Rich",
    str_detect(x, "river[ _-]*forest") ~ "River Forest",
    str_detect(x, "riverside|t34") ~ "Riverside",
    str_detect(x, "stickney") ~ "Stickney", str_detect(x, "thornton") ~ "Thornton",
    str_detect(x, "worth") ~ "Worth", TRUE ~ NA_character_
  )
}

read_methodology <- function(file_name, year, triad, pin_column) {
  path <- file.path(methodology_dir, file_name)
  data <- read_csv(path, col_types = cols(.default = col_character()),
                   show_col_types = FALSE, name_repair = "unique")
  required <- c("key_pin", pin_column, "file_name", "sheet_name")
  if (!all(required %in% names(data))) stop("Missing required fields in ", path)
  data |>
    transmute(
      methodology_keypin_raw = key_pin, pin_list_raw = .data[[pin_column]],
      methodology_year = as.integer(year), triad = triad,
      township = infer_township(file_name), source_file = file_name,
      source_sheet = sheet_name
    )
}

parse_methodology <- function(data) {
  staged <- data |>
    mutate(
      methodology_keypin = normalize_pin(methodology_keypin_raw),
      pin_text = str_remove_all(str_to_lower(coalesce(pin_list_raw, "")), "-"),
      has_unexpanded_range = str_detect(pin_text, "\\b(thru|through)\\b"),
      parsed_pins = str_extract_all(pin_text, "(?<![0-9])[0-9]{14}(?![0-9])"),
      parsed_pin_count = lengths(parsed_pins), invalid_keypin = is.na(methodology_keypin)
    )
  valid <- staged |>
    filter(!has_unexpanded_range, !invalid_keypin, parsed_pin_count > 0) |>
    select(-pin_text) |>
    unnest_longer(parsed_pins, values_to = "pin") |>
    transmute(pin, methodology_keypin, methodology_year, triad, township,
              source_file, source_sheet) |>
    distinct()
  exceptions <- staged |>
    filter(has_unexpanded_range | invalid_keypin | parsed_pin_count == 0) |>
    transmute(
      methodology_year, triad, township, source_file, source_sheet,
      methodology_keypin_raw, pin_list_raw,
      exception_type = case_when(
        has_unexpanded_range & invalid_keypin ~ "unexpanded_range_and_invalid_keypin",
        has_unexpanded_range ~ "unexpanded_range",
        invalid_keypin ~ "invalid_keypin",
        TRUE ~ "no_valid_14_digit_pin"
      )
    ) |>
    distinct()
  list(valid = valid, exceptions = exceptions)
}

snapshots <- list(
  chicago_2021 = read_methodology("combined_methodologyworksheets_CHICAGO.csv", 2021, "Chicago", "pi_ns"),
  chicago_2024 = read_methodology("combined_methodologyworksheets_chicago2024.csv", 2024, "Chicago", "ias_world_pi_ns"),
  north_2022 = read_methodology("combined_methodologyworksheets_NORTH2022.csv", 2022, "North", "pi_ns"),
  north_2025 = read_methodology("combined_methodologyworksheets_north2025.csv", 2025, "North", "pi_ns"),
  south_2023 = read_methodology("combined_methodologyworksheets_SOUTH.csv", 2023, "South", "ias_world_pi_ns"),
  south_2026 = read_methodology("combined_methodologyworksheets_south2026.csv", 2026, "South", "pi_ns")
)
parsed <- map(snapshots, parse_methodology)

methodology_exceptions <- map_dfr(parsed, "exceptions") |>
  arrange(triad, methodology_year, township, source_file, source_sheet)

# Phase 2: use the newest available snapshot. Retain South 2023 for every
# township whose workbook is absent from the 2026 directory.
south_2026_townships <- parsed$south_2026$valid |>
  filter(!is.na(township)) |> distinct(township) |> pull(township)
south_current <- bind_rows(
  parsed$south_2026$valid |> mutate(snapshot_rule = "south_2026_available"),
  parsed$south_2023$valid |>
    filter(is.na(township) | !township %in% south_2026_townships) |>
    mutate(snapshot_rule = if_else(
      township %in% c("Bloom", "Orland", "Rich", "Thornton"),
      "south_2023_fallback_2026_not_released_as_of_2026_09_21",
      "south_2023_fallback_2026_workbook_not_present"
    ))
)

amazon_path <- c(
  "inputs/amazonPINs.xlsx",
  file.path("..", "..", "Merriman RA", "ptax", "inputs", "amazonPINs.xlsx")
) |>
  keep(file.exists) |>
  first()
if (is.null(amazon_path)) stop("amazonPINs.xlsx was not found in the local or upstream inputs directory")

amazon <- read_excel(amazon_path, col_types = "text") |>
  transmute(
    pin = normalize_pin(PIN), methodology_keypin = "amazon", methodology_year = 2025L,
    triad = NA_character_, township = NA_character_, source_file = "amazonPINs.xlsx",
    source_sheet = "manual_group", snapshot_rule = "manual_amazon_group"
  ) |>
  filter(!is.na(pin))

current_methodology <- bind_rows(
  parsed$chicago_2024$valid |> mutate(snapshot_rule = "chicago_2024_current"),
  parsed$north_2025$valid |> mutate(snapshot_rule = "north_2025_current"),
  south_current, amazon
) |>
  group_by(pin, methodology_keypin, methodology_year, triad, township, snapshot_rule) |>
  summarise(
    source_files = paste(sort(unique(source_file)), collapse = " | "),
    source_sheets = paste(sort(unique(source_sheet)), collapse = " | "),
    .groups = "drop"
  ) |>
  group_by(pin) |>
  mutate(current_assignment_conflict = n_distinct(methodology_keypin) > 1) |>
  ungroup() |>
  arrange(pin, desc(methodology_year), methodology_keypin)

write_csv_auditable(current_methodology, file.path(output_dir, paste0("keypins_from_methodwkshts_", output_date, ".csv")))
write_csv_auditable(methodology_exceptions, file.path(output_dir, paste0("methodology_keypin_exceptions_", output_date, ".csv")))

# Phase 3: preserve all snapshots and derive one best-current row per PIN.
historical_methodology <- map_dfr(parsed, "valid") |>
  group_by(pin, methodology_keypin, methodology_year) |>
  summarise(
    triad = paste(sort(unique(na.omit(triad))), collapse = " | "),
    township = paste(sort(unique(na.omit(township))), collapse = " | "),
    source_files = paste(sort(unique(source_file)), collapse = " | "),
    source_sheets = paste(sort(unique(source_sheet)), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(pin, methodology_year, methodology_keypin)

best_current <- historical_methodology |>
  group_by(pin) |>
  filter(methodology_year == max(methodology_year, na.rm = TRUE)) |>
  summarise(
    best_methodology_year = first(methodology_year),
    best_methodology_keypin = if_else(n_distinct(methodology_keypin) == 1,
      first(methodology_keypin), paste(sort(unique(methodology_keypin)), collapse = " | ")),
    best_assignment_conflict = n_distinct(methodology_keypin) > 1,
    triad = first(triad), township = first(township),
    source_files = paste(sort(unique(source_files)), collapse = " | "),
    source_sheets = paste(sort(unique(source_sheets)), collapse = " | "), .groups = "drop"
  )

history_flags <- historical_methodology |>
  group_by(pin) |>
  summarise(
    methodology_keypin_changed = n_distinct(methodology_keypin) > 1,
    methodology_years = paste(sort(unique(methodology_year)), collapse = " | "),
    historical_keypins = paste(sort(unique(methodology_keypin)), collapse = " | "),
    .groups = "drop"
  )

project_lineage <- historical_methodology |>
  inner_join(best_current |> select(pin, best_methodology_keypin), by = "pin")
split_keypins <- project_lineage |>
  count(methodology_keypin, best_methodology_keypin) |>
  count(methodology_keypin, name = "n_later_projects") |>
  filter(n_later_projects > 1) |> pull(methodology_keypin)
merged_best_keypins <- project_lineage |>
  count(best_methodology_keypin, methodology_keypin) |>
  count(best_methodology_keypin, name = "n_historical_projects") |>
  filter(n_historical_projects > 1) |> pull(best_methodology_keypin)

best_current <- best_current |>
  left_join(history_flags, by = "pin") |>
  left_join(project_lineage |>
    group_by(pin) |>
    summarise(historical_project_split = any(methodology_keypin %in% split_keypins), .groups = "drop"),
    by = "pin") |>
  mutate(historical_projects_merged = best_methodology_keypin %in% merged_best_keypins) |>
  arrange(pin)

write_csv_auditable(historical_methodology, file.path(output_dir, paste0("methodology_keypin_history_", output_date, ".csv")))
write_csv_auditable(best_current, file.path(output_dir, paste0("methodology_best_current_assignment_", output_date, ".csv")))

# Phase 4: standardize manually checked township workbooks.
checked_files <- list.files(checked_dir, pattern = "_checked[.]xlsx$", full.names = TRUE, ignore.case = TRUE)
first_existing <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) rep(NA_character_, nrow(data)) else data[[found[[1]]]]
}
checked_township <- function(path) {
  stem <- basename(path) |>
    str_remove(regex("^projects_", ignore_case = TRUE)) |>
    str_remove(regex("_checked[.]xlsx$", ignore_case = TRUE))
  recode(stem,
    "bedfordpark" = "Bedford Park", "elkgrove" = "Elk Grove",
    "franklinpark" = "Franklin Park", "southholland" = "South Holland",
    .default = str_to_title(str_replace_all(stem, "_", " "))
  )
}

read_checked_sheet <- function(path, sheet) {
  data <- read_excel(path, sheet = sheet, col_types = "text") |> clean_names()
  pin_raw <- first_existing(data, "pin")
  keypin_raw <- first_existing(data, c("keypin", "main_keypin"))
  tibble(
    pin_raw = pin_raw, original_manual_keypin = keypin_raw,
    pin = normalize_pin(pin_raw), checked_keypin = normalize_pin(keypin_raw),
    pin10 = first_existing(data, "pin10"), pin7 = str_sub(normalize_pin(pin_raw), 1, 7),
    source_township = checked_township(path), source_file = basename(path), source_sheet = sheet,
    manually_checked = TRUE,
    manual_add_flag = first_existing(data, c("alea_added", "awm_added", "mvh_added")),
    group = first_existing(data, "group"), appellant = first_existing(data, "appellant"),
    project_appellant = first_existing(data, c("proj_appeallant", "project_appellant")),
    appeal_id = first_existing(data, c("appealid", "appeal_id")),
    appeal_year = first_existing(data, "year_appealed"),
    updated_owner = first_existing(data, c("updated_owner", "owner")),
    buyer_name = first_existing(data, c("sale_buyer_name", "buyer_name")),
    sale_document_number = first_existing(data, "sale_document_num"),
    sale_year = first_existing(data, "year_sold"),
    incentive_property = first_existing(data, "incent_prop"),
    review_notes = first_existing(data, c("review_notes", "decision_notes", "comments", "notes"))
  )
}

checked_inventory <- map_dfr(checked_files, function(path) map_dfr(excel_sheets(path), function(sheet) {
  data <- read_excel(path, sheet = sheet, col_types = "text") |> clean_names()
  tibble(source_file = basename(path), source_sheet = sheet, source_township = checked_township(path),
         rows = nrow(data), columns = ncol(data), column_names = paste(names(data), collapse = " | "))
}))

checked_assignments <- map_dfr(checked_files, function(path) {
  map_dfr(excel_sheets(path), ~ read_checked_sheet(path, .x))
}) |>
  mutate(invalid_pin = is.na(pin), invalid_checked_keypin = is.na(checked_keypin)) |>
  group_by(pin) |>
  mutate(checked_assignment_conflict = !is.na(pin) & n_distinct(checked_keypin, na.rm = TRUE) > 1) |>
  ungroup() |>
  arrange(source_township, pin)

checked_conflicts <- checked_assignments |>
  filter(invalid_pin | invalid_checked_keypin | checked_assignment_conflict) |>
  arrange(pin, source_file)

checked_inventory <- checked_inventory |>
  left_join(checked_assignments |>
    group_by(source_file, source_sheet) |>
    summarise(valid_pins = n_distinct(pin, na.rm = TRUE),
      duplicate_pin_rows = sum(duplicated(pin) & !is.na(pin)), invalid_pins = sum(invalid_pin),
      invalid_keypins = sum(invalid_checked_keypin),
      conflicting_pins = n_distinct(pin[checked_assignment_conflict], na.rm = TRUE), .groups = "drop"),
    by = c("source_file", "source_sheet"))

write_csv_auditable(checked_assignments, file.path(output_dir, paste0("checked_township_assignments_", output_date, ".csv")))
write_csv_auditable(checked_conflicts, file.path(output_dir, paste0("checked_township_conflicts_", output_date, ".csv")))
write_csv_auditable(checked_inventory, file.path(output_dir, paste0("checked_township_inventory_", output_date, ".csv")))

# Phase 5: construct one row per PIN in the complete ever-commercial/industrial
# universe. This phase intentionally reads the all-C&I-PINs-ever extract, not a
# balanced panel or a prepared time-series dataset.
first_nonblank <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  if (length(x) == 0) NA_character_ else first(x)
}

collapse_unique <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & str_trim(as.character(x)) != ""])))
  if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
}

normalize_name <- function(x) {
  x |>
    as.character() |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish() |>
    str_remove("( llc| inc| incorporated| corporation| corp| company| co| limited| ltd| lp| llp)+$") |>
    str_squish() |>
    na_if("")
}

universe_candidates <- c(
  file.path("..", "..", "Merriman RA", "ptax", "Output", "comm_ind_PINs_ever_2006to2024.csv"),
  file.path(output_dir, "comm_ind_PINs_ever_2006to2024.csv")
)
universe_path <- universe_candidates[file.exists(universe_candidates)][1]
if (is.na(universe_path)) stop("The all-C&I-PINs-ever CSV was not found")

universe_raw <- data.table::fread(
  universe_path,
  select = c("year", "pin", "class", "clean_name", "Triad", "Township", "incent_prop"),
  colClasses = "character", showProgress = FALSE
) |>
  as_tibble() |>
  mutate(pin = normalize_pin(pin), year = suppressWarnings(as.integer(year)),
         class = str_trim(class), incentive_numeric = suppressWarnings(as.numeric(incent_prop))) |>
  filter(!is.na(pin), !is.na(year))

latest_universe_year <- max(universe_raw$year, na.rm = TRUE)
pin_universe <- universe_raw |>
  arrange(pin, desc(year)) |>
  group_by(pin) |>
  summarise(
    classes_observed = collapse_unique(class),
    first_year_observed = min(year, na.rm = TRUE),
    last_year_observed = max(year, na.rm = TRUE),
    current_class = first_nonblank(class),
    municipality = first_nonblank(clean_name),
    township = first_nonblank(Township),
    triad = first_nonblank(Triad),
    ever_incentive_class = any(suppressWarnings(as.integer(class)) >= 600 &
                                 suppressWarnings(as.integer(class)) < 900, na.rm = TRUE) |
      any(incentive_numeric == 1, na.rm = TRUE),
    incentive_labels_observed = collapse_unique(incent_prop),
    observed_year_count = n_distinct(year),
    .groups = "drop"
  ) |>
  mutate(
    pin10 = str_sub(pin, 1, 10), pin7 = str_sub(pin, 1, 7),
    currently_exists_in_latest_extract = last_year_observed == latest_universe_year,
    universe_latest_year = latest_universe_year,
    universe_source_file = basename(universe_path)
  ) |>
  select(pin, pin10, pin7, township, municipality, triad, classes_observed,
         current_class, first_year_observed, last_year_observed,
         currently_exists_in_latest_extract, ever_incentive_class,
         incentive_labels_observed, observed_year_count, universe_latest_year,
         universe_source_file) |>
  arrange(pin)

write_csv_auditable(pin_universe, file.path(output_dir, paste0("commercial_industrial_pin_universe_", output_date, ".csv")))

# Phase 6: attach evidence without resolving a final project identifier. Each
# field remains attributable to its source so Phase 7 can apply an explicit
# authority hierarchy and route conflicts to manual review.
current_evidence <- current_methodology |>
  group_by(pin) |>
  summarise(
    current_methodology_keypins = collapse_unique(methodology_keypin),
    current_methodology_year = max(methodology_year, na.rm = TRUE),
    current_methodology_conflict = any(current_assignment_conflict),
    current_methodology_source_files = collapse_unique(source_files),
    current_methodology_source_sheets = collapse_unique(source_sheets),
    current_methodology_snapshot_rules = collapse_unique(snapshot_rule),
    .groups = "drop"
  )

checked_evidence <- checked_assignments |>
  filter(!invalid_pin) |>
  group_by(pin) |>
  summarise(
    checked_keypins = collapse_unique(checked_keypin),
    checked_assignment_conflict = any(checked_assignment_conflict | invalid_checked_keypin),
    checked_source_files = collapse_unique(source_file),
    checked_source_townships = collapse_unique(source_township),
    checked_appellant = first_nonblank(coalesce(project_appellant, appellant)),
    checked_owner = first_nonblank(updated_owner),
    checked_buyer = first_nonblank(buyer_name),
    checked_appeal_id = first_nonblank(appeal_id),
    checked_appeal_year = suppressWarnings(max(as.integer(appeal_year), na.rm = TRUE)),
    checked_sale_document_number = first_nonblank(sale_document_number),
    checked_sale_year = suppressWarnings(max(as.integer(sale_year), na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(across(c(checked_appeal_year, checked_sale_year), ~ if_else(is.infinite(.x), NA_integer_, .x)))

bor_candidates <- c(file.path(output_dir, "borappeals.csv"),
                    file.path("..", "..", "Merriman RA", "ptax", "Output", "borappeals.csv"))
bor_path <- bor_candidates[file.exists(bor_candidates)][1]
if (is.na(bor_path)) stop("borappeals.csv was not found")
bor_evidence <- data.table::fread(
  bor_path, select = c("pin", "tax_year", "class", "appealid", "appellant", "project_id"),
  colClasses = "character", showProgress = FALSE
) |>
  as_tibble() |>
  mutate(pin = normalize_pin(pin), tax_year = suppressWarnings(as.integer(tax_year))) |>
  filter(!is.na(pin)) |>
  arrange(pin, desc(tax_year)) |>
  group_by(pin) |>
  summarise(
    latest_appeal_id = first_nonblank(appealid), latest_appellant = first_nonblank(appellant),
    latest_appeal_year = suppressWarnings(max(tax_year, na.rm = TRUE)),
    latest_bor_class = first_nonblank(class), appeal_count = n(),
    appeal_ids_observed = collapse_unique(appealid),
    bor_project_ids_observed = collapse_unique(project_id), .groups = "drop"
  ) |>
  mutate(latest_appeal_year = if_else(is.infinite(latest_appeal_year), NA_integer_, latest_appeal_year))

sales_candidates <- c(
  file.path("..", "..", "dissertation", "data", "raw", "Assessor_Parcel_Sales_20250105.csv"),
  file.path("inputs", "Assessor_-_Parcel_Sales_20250704.csv")
)
sales_path <- sales_candidates[file.exists(sales_candidates)][1]
if (is.na(sales_path)) stop("Assessor parcel sales data was not found")
sales_evidence <- data.table::fread(
  sales_path,
  select = c("pin", "year", "class", "sale_document_num", "sale_seller_name",
             "num_parcels_sale", "sale_buyer_name"),
  colClasses = "character", showProgress = FALSE
) |>
  as_tibble() |>
  mutate(pin = normalize_pin(pin), year = suppressWarnings(as.integer(year)),
         class_numeric = suppressWarnings(as.integer(class))) |>
  filter(!is.na(pin), class_numeric >= 400, class_numeric < 900) |>
  arrange(pin, desc(year)) |>
  group_by(pin) |>
  summarise(
    latest_sale_year = suppressWarnings(max(year, na.rm = TRUE)),
    latest_sale_document_number = first_nonblank(sale_document_num),
    latest_buyer_name = first_nonblank(sale_buyer_name),
    latest_seller_name = first_nonblank(sale_seller_name),
    latest_sale_parcel_count = first_nonblank(num_parcels_sale),
    sale_documents_observed = collapse_unique(sale_document_num), .groups = "drop"
  ) |>
  mutate(latest_sale_year = if_else(is.infinite(latest_sale_year), NA_integer_, latest_sale_year))

prior_main_path <- file.path("..", "projects_checked_MAINFILE_REVIEWED_2.xlsx")
prior_main <- if (file.exists(prior_main_path)) {
  read_excel(prior_main_path, sheet = 1, col_types = "text") |>
    clean_names() |>
    transmute(
      pin = normalize_pin(pin),
      prior_main_keypin = normalize_pin(coalesce(reviewed_main_keypin, main_keypin)),
      prior_main_original_keypin = normalize_pin(main_keypin),
      prior_main_review_confidence = keypin_review_confidence,
      prior_main_review_reference = keypin_review_reference,
      prior_main_review_reason = keypin_review_reason
    ) |>
    filter(!is.na(pin)) |>
    distinct(pin, .keep_all = TRUE)
} else tibble(pin = character(), prior_main_keypin = character())

project_evidence <- pin_universe |>
  left_join(current_evidence, by = "pin") |>
  left_join(best_current |>
    select(pin, best_methodology_keypin, best_methodology_year,
           methodology_keypin_changed, methodology_years, historical_keypins,
           historical_project_split, historical_projects_merged), by = "pin") |>
  left_join(checked_evidence, by = "pin") |>
  left_join(bor_evidence, by = "pin") |>
  left_join(sales_evidence, by = "pin") |>
  left_join(prior_main, by = "pin") |>
  mutate(
    appellant_name = coalesce(checked_appellant, latest_appellant),
    owner_name = checked_owner,
    buyer_name = coalesce(checked_buyer, latest_buyer_name),
    normalized_appellant = normalize_name(appellant_name),
    normalized_owner = normalize_name(owner_name),
    normalized_buyer = normalize_name(buyer_name),
    normalized_seller = normalize_name(latest_seller_name),
    appeal_id = coalesce(checked_appeal_id, latest_appeal_id),
    sale_document_number = coalesce(checked_sale_document_number, latest_sale_document_number),
    evidence_year = pmax(best_methodology_year, checked_appeal_year, checked_sale_year,
                         latest_appeal_year, latest_sale_year, na.rm = TRUE),
    evidence_year = if_else(is.infinite(evidence_year), NA_real_, evidence_year),
    evidence_source_files = paste0(
      "universe:", universe_source_file,
      if_else(!is.na(current_methodology_source_files), paste0(" | methodology:", current_methodology_source_files), ""),
      if_else(!is.na(checked_source_files), paste0(" | checked:", checked_source_files), ""),
      if_else(!is.na(latest_appeal_id), paste0(" | appeals:", basename(bor_path)), ""),
      if_else(!is.na(latest_sale_document_number), paste0(" | sales:", basename(sales_path)), ""),
      if_else(!is.na(prior_main_keypin), paste0(" | prior_main:", basename(prior_main_path)), "")
    )
  ) |>
  group_by(best_methodology_keypin) |>
  mutate(same_methodology_keypin = !is.na(best_methodology_keypin) & n() > 1) |>
  ungroup() |>
  group_by(checked_keypins) |>
  mutate(same_checked_keypin = !is.na(checked_keypins) & n() > 1) |>
  ungroup() |>
  group_by(appeal_id) |>
  mutate(same_appeal = !is.na(appeal_id) & n() > 1) |>
  ungroup() |>
  group_by(normalized_appellant) |>
  mutate(similar_appellant = !is.na(normalized_appellant) & n() > 1) |>
  ungroup() |>
  group_by(normalized_owner) |>
  mutate(same_owner = !is.na(normalized_owner) & n() > 1) |>
  ungroup() |>
  group_by(normalized_buyer) |>
  mutate(same_buyer = !is.na(normalized_buyer) & n() > 1) |>
  ungroup() |>
  group_by(pin10) |>
  mutate(same_pin10 = n() > 1) |>
  ungroup() |>
  group_by(pin7) |>
  mutate(same_pin7 = n() > 1) |>
  ungroup() |>
  mutate(.evidence_row = row_number(), pin7_numeric = suppressWarnings(as.numeric(pin7))) |>
  group_by(normalized_appellant) |>
  mutate(
    shared_appellant_township_count = if_else(
      is.na(normalized_appellant), NA_integer_, n_distinct(township, na.rm = TRUE)
    ),
    shared_appellant_cross_township = !is.na(normalized_appellant) &
      shared_appellant_township_count > 1
  ) |>
  ungroup() |>
  arrange(normalized_appellant, township, pin7_numeric, pin, .by_group = FALSE) |>
  group_by(normalized_appellant, township) |>
  mutate(
    previous_pin7_gap = abs(pin7_numeric - lag(pin7_numeric)),
    next_pin7_gap = abs(lead(pin7_numeric) - pin7_numeric),
    nearest_shared_appellant_pin7_gap = pmin(previous_pin7_gap, next_pin7_gap, na.rm = TRUE),
    nearest_shared_appellant_pin7_gap = if_else(
      is.infinite(nearest_shared_appellant_pin7_gap), NA_real_, nearest_shared_appellant_pin7_gap
    ),
    shared_appellant_same_or_nearby_pin7 = !is.na(normalized_appellant) &
      !is.na(nearest_shared_appellant_pin7_gap) & nearest_shared_appellant_pin7_gap <= 2
  ) |>
  ungroup() |>
  mutate(
    possible_shared_project_from_pin_proximity = same_pin10 | same_pin7 |
      shared_appellant_same_or_nearby_pin7,
    shared_appellant_proximity_supports_project = similar_appellant &
      !shared_appellant_cross_township & possible_shared_project_from_pin_proximity,
    proximity_evidence_status = case_when(
      same_pin10 ~ "same_pin10_shared_parcel_polygon",
      same_pin7 ~ "same_pin7_shared_land_block",
      shared_appellant_same_or_nearby_pin7 ~ "shared_appellant_pin7_within_2_same_township",
      shared_appellant_cross_township ~ "shared_appellant_different_townships_less_likely",
      similar_appellant ~ "shared_appellant_different_land_blocks_less_likely",
      TRUE ~ "no_pin7_or_pin10_proximity_evidence"
    ),
    any_external_project_evidence = same_methodology_keypin | same_checked_keypin |
      same_appeal | shared_appellant_proximity_supports_project | same_owner | same_buyer |
      possible_shared_project_from_pin_proximity,
    evidence_conflict = coalesce(current_methodology_conflict, FALSE) |
      coalesce(checked_assignment_conflict, FALSE) |
      (!is.na(checked_keypins) & !is.na(best_methodology_keypin) &
         checked_keypins != best_methodology_keypin) |
      coalesce(methodology_keypin_changed, FALSE)
  ) |>
  select(-.evidence_row, -pin7_numeric, -previous_pin7_gap, -next_pin7_gap) |>
  arrange(pin)

write_csv_auditable(project_evidence, file.path(output_dir, paste0("project_linkage_evidence_", output_date, ".csv")))

message("Created Phase 2-6 methodology, checked-assignment, PIN-universe, and evidence outputs in ", output_dir)
