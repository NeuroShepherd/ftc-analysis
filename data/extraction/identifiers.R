library(readxl)
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
  "Sprint Roster",
  "BOUNDS",
  "Vertical Jump",
  "40s and 10m flys",
  "Official Roster",
  "Roster"
)

select_with_before <- function(data, pattern) {
  # quite a few workbooks with sheets I want but inconsistent column location
  col_pos <- which(str_detect(names(data), pattern))
  data |> select(all_of(c(col_pos - 1, col_pos)))
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
      print(notebook_names)

      map(
        notebook_names,
        ~ read_sheet_core(paste0("data/downloads/", .x, ".xlsx"), sheet_name)
      ) %>%
        set_names(notebook_names)
    }
  )


all_combined <- targeted_workbooks %>%
  enframe(name = "sheet_name", value = "workbooks") %>%
  mutate(
    workbooks = map(workbooks, enframe, name = "workbook_name", value = "data")
  ) %>%
  unnest(workbooks) %>%
  mutate(data = map(data, ~ mutate(.x, across(everything(), as.character)))) %>%
  unnest(data)


temp <- all_combined %>%
  mutate(grade = as.character(as.numeric(grade))) %>%
  distinct(grade, name, .keep_all = TRUE) %>%
  arrange(name)


# apparently no 10 meter fly data from 2016 through 2018 :(

downloaded_data_files %>%
  grep("10_meter", ., value = TRUE) %>%
  map(
    ~ read_excel(.x) %>%
      select(grade = 1, 2, 3) %>%
      unite(name, 2, 3, sep = ", ")
  )


# the only 3 years with explicit 30 year spreadsheets are 2016-2019
# which makes sense given previous comment
downloaded_data_files %>%
  grep("30_yard", ., value = TRUE) %>%
  map(
    ~ read_excel(.x) %>%
      select(-matches("Best")) %>%
      select(grade = 1, 2, 3) %>%
      unite(name, 2, 3, sep = ", ")
  )
