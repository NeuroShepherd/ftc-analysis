library(dplyr)
library(stringr)
library(tibble)


extraction_step <- function(
  md_path = "data/google-sheets/Practice Rankings.md"
) {
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


extract_additional_links <- function(
  md_path = "data/google-sheets/additional-links.md"
) {
  lines <- readLines(md_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0 & grepl("^\\*\\s+", lines)]
  lines <- sub("^\\*\\s+", "", lines)

  tibble(
    year = 2025L,
    text = sub(":\\s+(https?://.*)$", "", lines),
    href = sub("^[^:]+:\\s+(https?://.*)$", "\\1", lines)
  )
}
