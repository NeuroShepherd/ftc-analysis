rename_date_columns <- function(data, year = NULL, year_col = "year") {
    if (is.null(year)) {
        year <- unique(data[[year_col]])
    }

    # Case-insensitive month match
    date_cols <- grep("^[A-Za-z]{3,4} \\d+$", names(data), value = TRUE)

    new_names <- vapply(
        strsplit(date_cols, " "),
        function(parts) {
            m <- match(
                sub("^SEPT$", "SEP", toupper(parts[1])),
                toupper(month.abb)
            )
            cal_year <- if (m >= 9) year - 1 else year
            sprintf("%d-%02d-%02d", cal_year, m, as.numeric(parts[2]))
        },
        character(1)
    )

    rename_with(
        data,
        ~ if_else(.x %in% date_cols, new_names[match(.x, date_cols)], .x)
    )
}
