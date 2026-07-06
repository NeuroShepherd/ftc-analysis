library(dplyr)
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

md_path <- "data/google-sheets/Practice Rankings.md"
link_rows <- extract_practice_rankings_links(md_path)
