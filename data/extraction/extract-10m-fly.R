library(purrr)
library(tidyr)
library(dplyr)
source("data/extraction/identifiers.R")
source("data/extraction/rename-date-cols.R")

source("data/extraction/name-corrections.R")
incorrect <- unlist(name_corrections_list, use.names = FALSE)
correct <- rep(
    names(name_corrections_list),
    times = lengths(name_corrections_list)
)
correction_map <- set_names(correct, incorrect)

data_10m_flys <- list()
data_10m_flys$original <- list()


# Combine both batches of workbooks into a single named list of data tables
data_10m_flys$original <- c(
    targeted_workbooks %>%
        magrittr::extract2(1) %>%
        names() %>%
        set_names() %>%
        map(
            ~ {
                readxl::read_excel(
                    paste0("data/downloads/", .x, ".xlsx"),
                    sheet = "40s and 10m flys"
                ) %>%
                    mutate(year = as.numeric(str_sub(.x, 1, 4)))
            }
        ),
    targeted_workbooks %>%
        magrittr::extract2(2) %>%
        names() %>%
        set_names() %>%
        map(
            ~ readxl::read_excel(paste0("data/downloads/", .x, ".xlsx")) %>%
                mutate(year = as.numeric(str_sub(.x, 1, 4)))
        )
)


remove_trailing_numbers <- function(data) {
    data %>%
        rename_with(
            ~ str_remove(.x, "\\.\\.\\.\\d+$"),
            .cols = matches(".+\\.\\.\\.\\d+$")
        )
}

long_data <- function(data) {
    pivot_longer(
        data,
        cols = -c(name, year, uuid),
        names_to = "date",
        values_to = "dash_time"
    )
}


#######################

# Begin reading in the 10m fly data

data_10m_flys$wide$`2019_speed_cycle_two_tabs` <- data_10m_flys$original$`2019_speed_cycle_two_tabs` %>%
    select(21, 24:last_col()) %>%
    select(-...31) %>%
    rename(
        "Nov 22" = ...24,
        "Dec 4" = ...25,
        "Dec 11" = ...26
    ) %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 41) %>%
    rename_date_columns() %>%
    mutate(`2018-11-22` = as.numeric(`2018-11-22`))


data_10m_flys$wide$`2020_speed_cycle_several_tabs` <- data_10m_flys$original$`2020_speed_cycle_several_tabs` %>%
    select(21, 24:last_col()) %>%
    select(-c(...31:...33, MPH)) %>%
    rename(name = `10-Meter Fly`) %>%
    remove_trailing_numbers() %>%
    dplyr::filter(row_number() <= 37) %>%
    rename_date_columns()


data_10m_flys$wide$`2021_speed_cycle_several_tabs` <- data_10m_flys$original$`2021_speed_cycle_several_tabs` %>%
    select(24, 27:last_col()) %>%
    select(-MPH, -...43) %>%
    remove_trailing_numbers() %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 27) %>%
    rename_date_columns() %>%
    mutate(`2020-09-11` = as.numeric(`2020-09-11`))


# Feb 24 has two columns in this data frame so presuming the first
# one should actually be Feb 14th which also matches the pattern
# for the following years

data_10m_flys$wide$`2022_speed_cycle_several_tabs` <- data_10m_flys$original$`2022_speed_cycle_several_tabs` %>%
    select(30, 33:last_col()) %>%
    # names()
    select(-MPH, -...50, -"22.37 divided by 10m fly time") %>%
    rename(
        "Feb 14" = "Feb 24...45"
    ) %>%
    remove_trailing_numbers() %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 52) %>%
    rename_date_columns() %>%
    mutate(`2021-11-15` = as.numeric(`2021-11-15`))

data_10m_flys$wide$`2023_speed_cycle_several_tabs` <- data_10m_flys$original$`2023_speed_cycle_several_tabs` %>%
    select(30, 33:last_col()) %>%
    select(-MPH, -c(...45:...50), -"22.37 divided by 10m fly time") %>%
    remove_trailing_numbers() %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 42) %>%
    rename_date_columns() %>%
    mutate(`2022-12-12` = as.numeric(`2022-12-12`))


data_10m_flys$wide$`2024_speed_cycle_several_tabs` <- data_10m_flys$original$`2024_speed_cycle_several_tabs` %>%
    select(30, 33:last_col()) %>%
    select(-c(...43:"22.37 divided by 10m fly time")) %>%
    remove_trailing_numbers() %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 47) %>%
    rename_date_columns() %>%
    mutate(`2023-11-14` = as.numeric(`2023-11-14`))


data_10m_flys$wide$`2025_speed_cycle_2024_2025` <- data_10m_flys$original$`2025_speed_cycle_2024_2025` %>%
    select(30, 33:last_col()) %>%
    select(-c(...44:"22.37 divided by 10m fly time")) %>%
    remove_trailing_numbers() %>%
    rename(name = `10-Meter Fly`) %>%
    dplyr::filter(row_number() <= 28) %>%
    rename_date_columns() %>%
    mutate(
        `2024-11-11` = as.numeric(`2024-11-11`),
        `2025-02-19` = as.numeric(`2025-02-19`)
    )

data_10m_flys$wide$`2012_fat_10_meter_fly` <- data_10m_flys$original$`2012_fat_10_meter_fly` %>%
    select(2, 3, 6:12, year) %>%
    unite(name, 1, 2, sep = ", ") %>%
    dplyr::filter(row_number() <= 42) %>%
    rename_date_columns()

data_10m_flys$wide$`2013_fat_10_meter_fly` <- data_10m_flys$original$`2013_fat_10_meter_fly` %>%
    select(2, 3, 6:13, year) %>%
    unite(name, 1, 2, sep = ", ") %>%
    dplyr::filter(row_number() <= 41) %>%
    rename_date_columns()

data_10m_flys$wide$`2014_fat_10_meter_fly` <- data_10m_flys$original$`2014_fat_10_meter_fly` %>%
    select(2, 3, 7:11, year) %>%
    unite(name, 1, 2, sep = ", ") %>%
    dplyr::filter(row_number() <= 39) %>%
    rename_date_columns()

data_10m_flys$wide$`2015_fat_10_meter_fly` <- data_10m_flys$original$`2015_fat_10_meter_fly` %>%
    select(2, 3, 8:10, year) %>%
    unite(name, 1, 2, sep = ", ") %>%
    dplyr::filter(row_number() <= 45) %>%
    rename_date_columns()

# Choose to not trust the grade values in the worksheets since I have created
# a clean list associated to each individual.
# Instead, read in the names and the columns with 10m fly data only. Then,
# run the name cleaner and can join to the year-over-year ID table I have.

# run name corrections and UUID anonymization on names, and then sort by UUID
uuids <- select(all_combined, name, uuid, participation_year) %>%
    distinct()

data_10m_flys$wide %<>%
    map(
        ~ {
            mutate(.x, name = recode(name, !!!correction_map)) %>%
                left_join(uuids, by = c("name" = "name")) %>%
                relocate(uuid, .after = name)
        }
    )


# confirmed that no one is missing a UUID after joining the all_combined table
# data_10m_flys$wide %>%
#     map(
#         ~ {
#             sum(is.na(.x[["uuid"]]))
#         }
#     )

data_10m_flys$long <- data_10m_flys$wide %>%
    map(
        ~ {
            long_data(.x) %>%
                rename(season_year = year)
        }
    )


data_10m_flys$long_combined <- data_10m_flys$long %>%
    bind_rows()


# data_10m_flys$long_combined

# create a local copy for myself with original data if needed
saveRDS(data_10m_flys, "data/extraction/data_10m_flys.Rds")

# create an anonymized-only version which means dropping the original
# data files
data_10m_flys_anonymized <- data_10m_flys
data_10m_flys_anonymized$original <- NULL

data_10m_flys_anonymized$wide %<>%
    map(~ select(.x, -name))

data_10m_flys_anonymized$long %<>%
    map(~ select(.x, -name))

data_10m_flys_anonymized$long_combined %<>%
    select(-name)

saveRDS(
    data_10m_flys_anonymized,
    "data/extraction/data_10m_flys_anonymized.Rds"
)
