library(curl)
library(dplyr)
library(stringr)
library(tibble)


check_links_active_step <- function(
  links,
  out_path = "data/google-sheets/practice_rankings_link_status.csv"
) {
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

  link_status <- links |>
    rowwise() |>
    mutate(check = list(check_link_active(href))) |>
    unnest(check) |>
    ungroup()

  write.csv(link_status, out_path, row.names = FALSE)
  link_status
}
