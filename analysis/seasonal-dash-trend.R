# Seasonal Trend in 10m Fly Dash Times
# Looking at changes in dash_time from September through May/June, disregarding year

library(ggplot2)

library(dplyr)
library(lubridate)
library(lme4)
library(broom.mixed)
source("data/extraction/extract-10m-fly.R")

# --- Prepare data ---
season_df <- data_10m_flys$long_combined |>
    mutate(
        date_obj = ymd(date),
        month = month(date_obj),
        day = day(date_obj),
        # Create a "season date" — use a common year (2000) so Sep-Jun all plot in order
        season_date = make_date(
            year = if_else(month >= 9, 2000, 2001),
            month = month,
            day = day
        ),
        # Days since start of season (Sep 1)
        season_day = as.numeric(difftime(
            season_date,
            ymd("2000-09-01"),
            units = "days"
        )),
        month_label = factor(
            month,
            levels = c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6),
            labels = c(
                "Sep",
                "Oct",
                "Nov",
                "Dec",
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "May",
                "Jun"
            )
        )
    ) |>
    filter(month >= 9 | month <= 6, !is.na(dash_time))

# --- Quick descriptive stats ---
month_summary <- season_df |>
    group_by(month_label) |>
    summarise(
        n = n(),
        mean_time = mean(dash_time),
        sd_time = sd(dash_time),
        .groups = "drop"
    )

print("Monthly summary of 10m fly dash times:")
print(month_summary, n = Inf)

# --- Naive linear model (ignoring athlete clustering) ---
naive_lm <- lm(dash_time ~ season_day, data = season_df)
print("Naive linear model (ignoring athlete):")
print(summary(naive_lm))

# --- Create season label ---
season_df <- season_df |>
    mutate(
        year = year(date_obj),
        season_label = if_else(
            month >= 9,
            paste0(year, "-", year + 1),
            paste0(year - 1, "-", year)
        )
    )

# --- Mixed model: athlete nested within season ---
# This allows each athlete's baseline to vary from season to season
library(nlme)

mixed_model_original <- lme(
    dash_time ~ season_day,
    random = ~ 1 | name,
    data = season_df,
    na.action = na.omit
)

mixed_model <- lme(
    dash_time ~ season_day,
    random = ~ 1 | season_label / name,
    data = season_df,
    na.action = na.omit
)
print("Nested mixed model (athlete within season):")
print(summary(mixed_model))

# --- Nested model with random slope for season_day ---
# Allows each athlete's rate of improvement to vary
mixed_model_slope <- lme(
    dash_time ~ season_day,
    random = ~ 1 + season_day | season_label / name,
    data = season_df,
    na.action = na.omit
)
print("Nested mixed model with random slope for season_day:")
print(summary(mixed_model_slope))

cat("\n--- Model comparison ---\n")
cat(sprintf(
    "Original (name only)        AIC: %.1f  BIC: %.1f\n",
    AIC(mixed_model_original),
    BIC(mixed_model_original)
))
cat(sprintf(
    "Nested intercept-only       AIC: %.1f  BIC: %.1f\n",
    AIC(mixed_model),
    BIC(mixed_model)
))
cat(sprintf(
    "Nested + random slope       AIC: %.1f  BIC: %.1f\n",
    AIC(mixed_model_slope),
    BIC(mixed_model_slope)
))
cat(sprintf(
    "ΔAIC (slope vs intercept-only) = %.1f\n",
    AIC(mixed_model) - AIC(mixed_model_slope)
))

cat("\n--- Variance components (random slope model) ---\n")
print(VarCorr(mixed_model_slope))

cat("\n--- Slope variation across athletes ---\n")
athlete_slope_sd <- as.numeric(VarCorr(mixed_model_slope)[, "StdDev"])[4]
pop_slope <- fixef(mixed_model_slope)[["season_day"]]
cat(sprintf("Population avg slope:   %.5f sec/day\n", pop_slope))
cat(sprintf("Athlete slope SD:       %.5f sec/day\n", athlete_slope_sd))
cat(sprintf(
    "95%% of athletes have slopes between %.5f and %.5f\n",
    pop_slope - 1.96 * athlete_slope_sd,
    pop_slope + 1.96 * athlete_slope_sd
))
cat(sprintf(
    "~%.1f%% improve, ~%.1f%% get slower over the season\n",
    pnorm(0, pop_slope, athlete_slope_sd, lower.tail = FALSE) * 100,
    pnorm(0, pop_slope, athlete_slope_sd) * 100
))

# Extract athlete intercept-slope correlation
summ <- summary(mixed_model_slope)
athlete_corr <- summ$modelStruct$reStruct[[2]] |>
    nlme::pdMatrix() |>
    cov2cor() |>
    (\(m) m[1, 2])()
cat(sprintf("Athlete intercept-slope correlation: r = %.3f\n", athlete_corr))
cat("(Athletes with faster baselines tend to improve more)\n")

# ---- Visualizations ----

# 1. Overall scatter with mixed model fixed effect line ------------------
season_day_range <- tibble(
    season_day = seq(
        min(season_df$season_day),
        max(season_df$season_day),
        length.out = 100
    )
)
season_day_range$predicted <- predict(
    mixed_model_slope,
    newdata = season_day_range,
    level = 0
)

set.seed(7291)
ggplot(season_df, aes(x = season_day, y = dash_time)) +
    geom_point(
        alpha = 0.15,
        size = 1.5,
        position = position_jitter(width = 1.5, height = 0)
    ) +
    geom_line(
        data = season_day_range,
        aes(y = predicted),
        color = "#D55E00",
        linewidth = 1.3
    ) +
    scale_x_continuous(
        breaks = c(0, 30, 61, 91, 122, 153, 181, 212, 243, 273, 304),
        labels = c(
            "Sep 1",
            "Oct 1",
            "Nov 1",
            "Dec 1",
            "Jan 1",
            "Feb 1",
            "Mar 1",
            "Apr 1",
            "May 1",
            "Jun 1",
            "Jul 1"
        )
    ) +
    labs(
        title = "10m Fly Dash Times Across the Season",
        subtitle = sprintf(
            "Random slope model: %.4f sec/day (%.3f sec over full season)",
            fixef(mixed_model_slope)[["season_day"]],
            fixef(mixed_model_slope)[["season_day"]] *
                diff(range(season_df$season_day))
        ),
        x = "Date",
        y = "Dash Time (seconds)"
    ) +
    theme_minimal(base_size = 11)

# 2. Monthly boxplot -----------------------------------------------------
ggplot(season_df, aes(x = month_label, y = dash_time)) +
    geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.size = 0.7) +
    stat_summary(
        fun = mean,
        geom = "point",
        shape = 18,
        size = 3,
        color = "#D55E00"
    ) +
    labs(
        title = "Distribution of 10m Fly Dash Times by Month",
        subtitle = "Points show monthly means; boxes span Q1-Q3",
        x = "Month",
        y = "Dash Time (seconds)"
    ) +
    theme_minimal(base_size = 11)

# 3. Per-individual trends (top 4 athletes by data volume) ---------------
top_athletes <- season_df |>
    count(name, sort = TRUE) |>
    slice_head(n = 4) |>
    pull(name)

season_df |>
    filter(name %in% top_athletes) |>
    ggplot(aes(x = season_day, y = dash_time)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linewidth = 0.9) +
    facet_wrap(~name, scales = "free_x") +
    labs(
        title = "Individual 10m Fly Dash Trends (Top 4 by Data Volume)",
        x = "Days Since Sep 1",
        y = "Dash Time (seconds)"
    ) +
    theme_minimal(base_size = 11)
