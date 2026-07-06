library(curl)
library(dplyr)
library(stringr)
library(tibble)

extract_additional_links <- function(md_path) {
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

check_link_active <- function(url) {
  response <- tryCatch(
    curl_fetch_memory(
      url,
      handle = new_handle(
        followlocation = TRUE,
        maxredirs = 10L,
        useragent = "Posit Assistant link checker"
      )
    ),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(tibble(
      final_url = NA_character_,
      status_code = NA_integer_,
      active = FALSE,
      reason = conditionMessage(response)
    ))
  }

  body <- tryCatch(rawToChar(response$content), error = function(e) "")
  body_lc <- str_to_lower(body)

  inactive_phrases <- c(
    "sorry, the file you have requested does not exist",
    "you need access",
    "permission denied",
    "file not found",
    "page not found"
  )

  matched_phrase <- inactive_phrases[vapply(
    inactive_phrases,
    function(phrase) str_detect(body_lc, fixed(phrase)),
    logical(1)
  )]

  status_ok <- !is.na(response$status_code) &&
    response$status_code >= 200 &&
    response$status_code < 300
  active <- status_ok && length(matched_phrase) == 0

  reason <- if (active) {
    NA_character_
  } else if (length(matched_phrase) > 0) {
    matched_phrase[[1]]
  } else {
    paste0("HTTP ", response$status_code)
  }

  tibble(
    final_url = response$url,
    status_code = response$status_code,
    active = active,
    reason = reason
  )
}

md_path <- "data/google-sheets/Practice Rankings.md"
addl_path <- "data/google-sheets/additional-links.md"
out_path <- "data/google-sheets/practice_rankings_link_status.csv"

links <- bind_rows(
  extract_practice_rankings_links(md_path),
  extract_additional_links(addl_path)
)

link_status <- links |>
  rowwise() |>
  mutate(check = list(check_link_active(href))) |>
  unnest(check) |>
  ungroup()

write.csv(link_status, out_path, row.names = FALSE)
