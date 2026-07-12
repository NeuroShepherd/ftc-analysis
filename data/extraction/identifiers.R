library(readxl)
use("magrittr", "%<>%")
library(purrr)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)


downloaded_data_files <- list.files("data/downloads", full.names = T) %>%
  purrr::set_names(str_remove_all(., "data/downloads/|.xlsx"))


split_last_first_name <- function(name) {
  name_split <- stringr::str_split(name, ", ", simplify = T)

  return(c(name_split[2], name_split[1]))
}


# split_last_first_name("World, Hello")

excel_sheets(
  downloaded_data_files["2025_speed_cycle_2024_2025"]
)


all_excel_sheet_names <- purrr::map(
  downloaded_data_files,
  ~ excel_sheets(.x)
)

# all of the files up until 2019 are single-sheet workbooks, thereafter it's a mix

all_excel_sheet_names %>%
  unlist() %>%
  table() %>%
  sort()

# Useful sheets to gather information: Sprint Roster (3), BOUNDS (4),
# Vertical Jump (5), 40s and 10m flys (7), and then the sheets labeled for 10m flys
# Also saw Official Roster (1) and Roster (1) which I'll include
# No need to dig further than that IMO.

get_workbooks_with_relevant_sheets <- function(target_sheet) {
  Filter(
    function(sheet_names) any(sheet_names == target_sheet),
    all_excel_sheet_names
  )
}

target_sheets <- c(
  # "Sprint Roster",
  # "BOUNDS",
  # "Vertical Jump",
  "40s and 10m flys"
  # "Official Roster",
  # "Roster"
)

select_with_before <- function(data, pattern) {
  # quite a few workbooks with sheets I want but inconsistent column location
  col_pos <- which(str_detect(names(data), pattern))
  data |> select(all_of(c(col_pos - 1, col_pos)))
}


removable_rows <- c(
  "#DIV/0!",
  "AVERAGE OF TOP TEN",
  "AVERAGE OF TOP 20",
  "AVERAGE, NA",
  "Average, NA"
)
remove_misc_rows <- function(data, column, removable_rows) {
  data %>%
    dplyr::filter(!{{ column }} %in% removable_rows)
}

read_sheet_core <- function(path, sheet_name) {
  switch(
    sheet_name,
    "Sprint Roster" = read_excel(
      path,
      sheet_name,
      col_names = c("grade", "name")
    ),
    "BOUNDS" = read_excel(path, sheet_name) %>%
      select(1, 2) %>%
      rename(grade = 1, name = 2),
    "Vertical Jump" = read_excel(path, sheet_name) %>%
      select_with_before("Vertical Jump") %>%
      rename(grade = 1, name = 2),
    "40s and 10m flys" = read_excel(path, sheet_name) %>%
      # had to confirm all workbooks with these sheets have a 10-Meter Fly col
      select_with_before("10-Meter Fly") %>%
      rename(grade = 1, name = 2),
    read_excel(path, sheet_name, col_names = FALSE) %>%
      select(grade = 1, name = 2)
  )
}

targeted_workbooks <- target_sheets %>%
  set_names() %>%
  map(
    ~ {
      sheet_name <- .x
      notebook_names <- names(get_workbooks_with_relevant_sheets(.x))

      map(
        notebook_names,
        ~ read_sheet_core(
          paste0("data/downloads/", .x, ".xlsx"),
          sheet_name
        ) %>%
          remove_misc_rows(name, removable_rows) %>%
          dplyr::filter(!is.na(grade) | !is.na(name))
      ) %>%
        set_names(notebook_names)
    }
  ) %>%
  c(
    list(
      "10_meter" = downloaded_data_files %>%
        grep("10_meter", ., value = TRUE) %>%
        map(
          ~ read_excel(.x) %>%
            select(grade = 1, 2, 3) %>%
            unite(name, 2, 3, sep = ", ") %>%
            remove_misc_rows(name, removable_rows) %>%
            dplyr::filter(name != "NA, NA")
        ),
      "30_yard" = downloaded_data_files %>%
        grep("30_yard", ., value = TRUE) %>%
        map(
          ~ {
            read_excel(.x) %>%
              select(-matches("Best")) %>%
              select(grade = 1, 2, 3) %>%
              unite(name, 2, 3, sep = ", ") %>%
              remove_misc_rows(name, removable_rows) %>%
              dplyr::filter(name != "NA, NA")
          }
        )
    )
  )

# apparently no 10 meter fly data from 2016 through 2018 :(
# the only 3 years with explicit 30 yard spreadsheets are 2016-2019

# Probably do some additional cleaning or checks of the files here
# for comments that were entered in the rows

# remove notes and info from previous year
targeted_workbooks$`40s and 10m flys`$`2025_speed_cycle_2024_2025` %<>%
  dplyr::filter(row_number() <= 30)

targeted_workbooks$`40s and 10m flys`$`2024_speed_cycle_several_tabs` %<>%
  dplyr::filter(row_number() <= 47)

targeted_workbooks$`40s and 10m flys`$`2023_speed_cycle_several_tabs` %<>%
  dplyr::filter(row_number() <= 42)

targeted_workbooks$`40s and 10m flys`$`2022_speed_cycle_several_tabs` %<>%
  dplyr::filter(row_number() <= 52)

# no problems with the 40s and 10m flys sheet from 2021

targeted_workbooks$`40s and 10m flys`$`2020_speed_cycle_several_tabs` %<>%
  dplyr::filter(row_number() <= 37)


# # i don't know what strangeness happened here, but this person is listed
# # as a freshman in 2020 and then again in 2022. they do not appear in any
# # track results online or in the 2020 spreadsheet either
# targeted_workbooks$`Sprint Roster`$`2020_speed_cycle_several_tabs` %<>%
#   dplyr::filter(name != "Clark Amiel")

# # this actually appears to have happened many times. I think a roster was
# # simply copied and pasted from a later year into the 2020 roster without
# # editing the years. There are tons of errors originating there so I'm
# # just going to drop the 2020 Sprint Roster and BOUNDS as they seem to be
# # the sources of error and have no associated empirical data

# targeted_workbooks$`Sprint Roster`$`2020_speed_cycle_several_tabs` <- NULL
# targeted_workbooks$BOUNDS$`2020_speed_cycle_several_tabs` <- NULL

# # same thing for the 2023 spreadsheet where I've confirmed the BOUNDS
# # sheet has incorrect grades
# targeted_workbooks$BOUNDS$`2023_speed_cycle_several_tabs` <- NULL

# # all of the 2021 BOUNDS, Vertical Jump and Sprint Roster sheets
# # also appear to have incorrect years
# targeted_workbooks$`Sprint Roster`$`2021_speed_cycle_several_tabs` <- NULL
# targeted_workbooks$BOUNDS$`2021_speed_cycle_several_tabs` <- NULL
# targeted_workbooks$`Vertical Jump`$`2021_speed_cycle_several_tabs` <- NULL

targeted_workbooks$`40s and 10m flys`$`2019_speed_cycle_two_tabs` %<>%
  dplyr::filter(name != "Damhoff, Dr. Brian")


all_combined <- targeted_workbooks %>%
  enframe(name = "sheet_name", value = "workbooks") %>%
  mutate(
    workbooks = map(workbooks, enframe, name = "workbook_name", value = "data")
  ) %>%
  unnest(workbooks) %>%
  mutate(data = map(data, ~ mutate(.x, across(everything(), as.character)))) %>%
  unnest(data)


source("data/extraction/name-corrections.R")

incorrect <- unlist(name_corrections_list, use.names = FALSE)
correct <- rep(
  names(name_corrections_list),
  times = lengths(name_corrections_list)
)
correction_map <- set_names(correct, incorrect)

all_combined <- all_combined |>
  mutate(name = recode(name, !!!correction_map)) %>%
  mutate(grade = (as.numeric(grade))) %>%
  mutate(year = as.numeric(str_sub(workbook_name, 1, 4))) %>%
  arrange(name, year, as.numeric(grade))

# View(all_combined)

# add a UUID for anonymization to all sprinters
unique_names <- unique(all_combined$name)
name_to_uuid <- set_names(
  uuid::UUIDgenerate(n = length(unique_names)),
  unique_names
)

all_combined %<>%
  mutate(uuid = unname(name_to_uuid[name]))


# NEED TO UPDATE SO THAT GRADE **PLUS** A PARTICIPATION YEAR VARIABLE IS AVAILABLE

all_combined %<>%
  arrange(name, grade) %>%
  group_by(name) %>%
  mutate(participation_year = row_number())
