library(targets)

tar_option_set(
  packages = c(
    "curl",
    "dplyr",
    "readr",
    "readxl",
    "stringr",
    "tibble",
    "tidyr"
  )
)

tar_source("scripts/")

list(
  # ── Step 1: Extract links from both markdown sources ────────
  tar_target(
    link_rows,
    extraction_step()
  ),
  tar_target(
    additional_link_rows,
    extract_additional_links()
  ),
  tar_target(
    all_links,
    bind_rows(link_rows, additional_link_rows)
  ),

  # ── Step 2: Check which links are still active ──────────────
  # Returns the status data and writes a CSV for inspection.
  tar_target(
    link_status,
    check_links_active_step(all_links)
  ),

  # ── Step 3: Download active workbooks & inventory tabs ──────
  tar_target(
    download_result,
    download_data_files(status = link_status)
  ),
  tar_target(
    download_manifest_file,
    download_result$manifest_path,
    format = "file"
  ),
  tar_target(
    tab_inventory_file,
    download_result$tab_inventory_path,
    format = "file"
  )
)
