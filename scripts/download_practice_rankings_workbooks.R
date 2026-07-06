library(curl)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

extract_practice_rankings_links <- function(md_path) {
  lines <- readLines(md_path, warn = FALSE)

  current_year <- NA_integer_
  records <- vector("list", length(lines))
  idx <- 0L

  for (line in lines) {
    line <- trimws(line)

    if (startsWith(line, "**PRACTICE RANKINGS ")) {
      current_year <- as.integer(str_extract(line, "\\d{4}"))
    }

    link_match <- str_match(line, '^\\[(.+?)\\]\\((.+?)\\)$')
    if (!is.na(link_match[1, 1])) {
      idx <- idx + 1L
      records[[idx]] <- tibble(
        year = current_year,
        text = str_squish(gsub("\\*", "", link_match[1, 2])),
        href = link_match[1, 3]
      )
    }
  }

  bind_rows(records[seq_len(idx)])
}

extract_google_file_id <- function(url) {
  patterns <- c(
    "/d/([A-Za-z0-9_-]+)",
    "[?&]key=([A-Za-z0-9_-]+)"
  )

  for (pattern in patterns) {
    match <- str_match(url, pattern)
    if (!is.na(match[1, 2])) {
      return(match[1, 2])
    }
  }

  NA_character_
}

safe_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
}

make_export_url <- function(file_id) {
  sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?format=xlsx",
    file_id
  )
}

download_active_workbooks <- function(
  md_path = "data/google-sheets/Practice Rankings.md",
  status_path = "data/google-sheets/practice_rankings_link_status.csv",
  download_dir = "data/downloads",
  overwrite = FALSE
) {
  links <- extract_practice_rankings_links(md_path)

  status <- if (file.exists(status_path)) {
    read.csv(status_path, stringsAsFactors = FALSE) |>
      as_tibble()
  } else {
    stop("Link status file not found: ", status_path)
  }

  active_links <- links |>
    left_join(
      status |> select(year, href, final_url, status_code, active, reason),
      by = c("year", "href")
    ) |>
    filter(active %in% TRUE) |>
    mutate(
      google_file_id = vapply(href, extract_google_file_id, character(1)),
      export_url = if_else(
        str_detect(href, "/spreadsheets/d/"),
        sub("/edit.*$", "/export?format=xlsx", href),
        if_else(
          !is.na(google_file_id),
          make_export_url(google_file_id),
          NA_character_
        )
      ),
      file_name = sprintf(
        "%04d_%s_%s.xlsx",
        year,
        safe_name(text),
        google_file_id
      ),
      local_path = file.path(download_dir, file_name)
    )

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  downloads <- vector("list", nrow(active_links))

  for (i in seq_len(nrow(active_links))) {
    row <- active_links[i, ]

    if (!overwrite && file.exists(row$local_path[[1]])) {
      downloads[[i]] <- tibble(
        year = row$year[[1]],
        text = row$text[[1]],
        href = row$href[[1]],
        google_file_id = row$google_file_id[[1]],
        source_url = row$href[[1]],
        export_url = row$export_url[[1]],
        local_path = row$local_path[[1]],
        file_size_bytes = as.integer(file.info(row$local_path[[1]])$size),
        download_status = "skipped_exists"
      )
      next
    }

    curl::curl_download(
      row$export_url[[1]],
      row$local_path[[1]],
      handle = curl::new_handle(
        followlocation = TRUE,
        maxredirs = 10L,
        useragent = "Posit Assistant workbook downloader"
      ),
      quiet = TRUE
    )

    downloads[[i]] <- tibble(
      year = row$year[[1]],
      text = row$text[[1]],
      href = row$href[[1]],
      google_file_id = row$google_file_id[[1]],
      source_url = row$href[[1]],
      export_url = row$export_url[[1]],
      local_path = row$local_path[[1]],
      file_size_bytes = as.integer(file.info(row$local_path[[1]])$size),
      download_status = "downloaded"
    )
  }

  bind_rows(downloads) |>
    arrange(year, text)
}

inventory_workbook_tabs <- function(
  download_manifest,
  tab_inventory_path = NULL
) {
  tab_rows <- vector("list", nrow(download_manifest))
  idx <- 0L

  for (i in seq_len(nrow(download_manifest))) {
    wb <- download_manifest[i, ]

    if (!file.exists(wb$local_path[[1]])) {
      next
    }

    sheets <- readxl::excel_sheets(wb$local_path[[1]])

    for (j in seq_along(sheets)) {
      sheet_name <- sheets[[j]]
      dat <- suppressMessages(
        readxl::read_excel(
          wb$local_path[[1]],
          sheet = sheet_name,
          col_names = FALSE,
          col_types = "text"
        )
      )

      idx <- idx + 1L
      tab_rows[[idx]] <- tibble(
        year = wb$year[[1]],
        workbook_title = wb$text[[1]],
        google_file_id = wb$google_file_id[[1]],
        source_url = wb$source_url[[1]],
        local_path = wb$local_path[[1]],
        sheet_index = j,
        sheet_name = sheet_name,
        row_count = nrow(dat),
        col_count = ncol(dat)
      )
    }
  }

  tabs <- bind_rows(tab_rows)

  if (!is.null(tab_inventory_path)) {
    dir.create(
      dirname(tab_inventory_path),
      recursive = TRUE,
      showWarnings = FALSE
    )
    write_csv(tabs, tab_inventory_path)
  }

  tabs
}

build_practice_rankings_downloads <- function(
  md_path = "data/google-sheets/Practice Rankings.md",
  status_path = "data/google-sheets/practice_rankings_link_status.csv",
  download_dir = "data/downloads",
  manifest_dir = "data/manifests",
  manifest_path = file.path(
    manifest_dir,
    "practice_rankings_download_manifest.csv"
  ),
  tab_inventory_path = file.path(
    manifest_dir,
    "practice_rankings_tab_inventory.csv"
  ),
  overwrite = FALSE
) {
  downloads <- download_active_workbooks(
    md_path = md_path,
    status_path = status_path,
    download_dir = download_dir,
    overwrite = overwrite
  )

  write_csv(downloads, manifest_path)

  tabs <- inventory_workbook_tabs(
    downloads,
    tab_inventory_path = tab_inventory_path
  )

  invisible(list(
    manifest = downloads,
    tabs = tabs,
    manifest_path = manifest_path,
    tab_inventory_path = tab_inventory_path
  ))
}

if (sys.nframe() == 0L) {
  result <- build_practice_rankings_downloads()
  message(sprintf(
    "Wrote manifest to %s and tab inventory to %s.",
    result$manifest_path,
    result$tab_inventory_path
  ))
}
