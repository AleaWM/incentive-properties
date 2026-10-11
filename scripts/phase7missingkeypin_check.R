library(tidyverse)


# list of pins that had blank phase7keypins
phase7checks <- read_csv("./CookCounty-PropertyTaxes/inputs/check_pin_existance_in2024.csv") |>
  mutate(pin = str_sub(pin, 1, 14),
    pin10 = str_sub(pin, 1, 10),
    pin7 = str_sub(pin, 1, 7))

# list of pins in 2024
pins_in2024 <- read_csv("../Merriman RA/ptax/Output/Dont_Upload/0_joined_PIN_data_2024.csv") |>
  mutate(pin = str_sub(pin, 1, 14),
    pin10 = str_sub(pin, 1, 10),
    pin7 = str_sub(pin, 1, 7))


# exempt pins in 2024
exempt_in2024 <- pins_in2024 |> filter(class == 0)
# 13,330 of the 17,476 are tax exempt in 2024

phase7missing_existin2024 <- phase7checks |> filter(pin %in% pins_in2024$pin)


# pins that do not exist in 2024 ptaxsim data
phase7missing_2024extinct <- phase7checks |> filter(!pin %in% pins_in2024$pin)
# 4856 pins do not exist in ptaxsim data that are missing keypins in phase7files for C&I projects

# > 12620+4856 = 17476    <= all pins in this phase7 check are either exempt or no longer exist.

write_csv(phase7missing_existin2024, "CookCounty-PropertyTaxes/Output/phase7pinswithoutkeypins_existin2024.csv")

phase7missing_2024taxexempt <- phase7checks |> filter(!pin %in% exempt_in2024$pin)





bor <- read_csv("../Merriman RA/ptax/Output/borappeals.csv") %>%
  mutate(project_appellant = paste(project_id, sep = "-", appellant)) |>  # note: project_id was just the appealid as a whole number, like AppealTrk in next dataset.
  arrange(pin)

bor_2024_2025 <- read_csv("./inputs/Board_of_Review_Appeal_Decision_History_20260924.csv") %>%
  rename(class = Class,
    appellant = Appellant,
    appealid = (AppealTrk)) |>
  mutate(
    project = trunc(AppealID),
    project_appellant = paste(project, sep = "-", appellant),
    majorclass = str_sub(class, 1, 1),
    project_id = appealid # only to have same columns as other dataset to allow for rowbinding
  ) |>
  arrange(pin)




##### Create updated BOR appeals dataset
new_bor <- bor_2024_2025 |> filter(pin %in% bor$pin) |>
  select(pin, township_code, tax_year, class, majorclass, changereason = ChangeReason,
    changereasondescription = ChangeReasonDescription, appealid, appellant, project_id, project_appellant)

old_bor <-  bor |> filter(pin %in% new_bor$pin) |> filter(tax_year > 2022)

new_appealled_pins <- new_bor |> filter(!pin %in% old_bor$pin)
## 12,200 New PINs that were not appealed after 2022 that should be in the same triad

old_bor |> group_by(project_appellant) |> summarize(appellant = first(appellant), firstpin_keypin = first(pin), pin_count = n()) |> View()

new_bor |> group_by(project_appellant) |> summarize(appellant = first(appellant), firstpin_keypin = first(pin), pin_count = n()) |> View()
# 63,094 becomes 28,194 based on appeal id and appellant name


new_bor |> filter(appellant %in% old_bor$appellant)
# 25,800 pins in new BOR PINs with same appellant name


bind_old <- bor |>
  select(pin, township_code, tax_year, class, majorclass, changereason,
    changereasondescription, appealid, appellant, project_id, project_appellant)

bind_new <- bor_2024_2025 |>
  select(pin, township_code, tax_year, class, majorclass, changereason = ChangeReason,
    changereasondescription = ChangeReasonDescription, appealid, appellant,
    project_id, project_appellant)

updated_appeals_data <- rbind(bind_old, bind_new)

write_csv(updated_appeals_data, "../Merriman RA/ptax/Output/borappeals_UPDATED.csv")
