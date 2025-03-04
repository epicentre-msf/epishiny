# function that takes a df and a list of dates and calculates delays between them for each row

library(tidyverse)
library(highcharter)

# ARGS --------------------------------------------------------------------

#data
dat <- episimdata::moissala_linelist_clean_EN

# select dates
date_vars <- c(
  "Symptom onset" = "date_onset",
  "Notification" = "date_consultation",
  "Hospitalisation" = "date_admission",
  "Outcome" = "date_outcome"
)

#minimum observation required
min_obs <-  20

# DELAY DF  ======================================================================================================================

get_delay_df <- function(dat, date_vars){

  # Get all unique pairs of date variables
  date_combinations <- combn(date_vars, 2, simplify = FALSE)

  dat <- dat |>

    # Ensure dates are in Date format - or ask for it to be date ?
    mutate(across(all_of(date_vars), as.Date))

  # Function to calculate delay between two date variables
  calculate_delay <- function(x, dates) {

    var_name <-  paste0(dates[[1]], "__", dates[[2]] )

    x |>

      mutate(across(all_of(dates), as.Date)) |>

      transmute(
        !!var_name := as.integer( .data[[dates[[2]]]] - .data[[dates[[1]]]])
      )
  }

  # apply delay function to all combination and get df
  final_df <- purrr::map(
    date_combinations,
    ~ calculate_delay(dat, .x)
  ) |>
    bind_cols()

  return(final_df)

}

# SUMMARY DELAYS  ======================================================================================================================
# from a delay df it calculates the summary for all possible delays

get_delay_summary <- function(delay_df,
                              co_value = 30,
                              group_var = NULL){

  # Pivot delay long
  delay_long <- delay_df |>
    pivot_longer(contains("__"),
                 names_to = "dates",
                 values_to = "timespan")

  # get invalid DF
  invalid_df <- delay_long |>
    summarise(
      .by = dates,
      N = n(),
      n_na = sum(is.na(timespan)),
      n_invalid = sum(timespan < 0, na.rm = TRUE),
      n_co = sum(timespan >= co_value, na.rm = TRUE),
      n_valid = N - (n_na + n_invalid + n_co)
    )

  #clean the timespan data
  delay_long <- delay_long |>
    # remove NAs
    drop_na(timespan) |>
    # keep valid values
    filter(between(timespan, 0, co_value))

  # calculate summary
  delay_summary <- delay_long |>
    summarise(
      .by = dates,
      min = round(digits = 2, suppressWarnings(min(timespan, na.rm = TRUE))),
      mean = round(digits = 2, suppressWarnings(mean(timespan, na.rm = TRUE))),
      median = round(digits = 2, suppressWarnings(median(timespan, na.rm = TRUE))
      ),
      max = round(digits = 2, suppressWarnings(max(timespan, na.rm = TRUE))),
      n_valid = sum(!is.na(timespan))

    ) |>

    pivot_longer(
      c(min, mean, median, max),
      names_to = "stat",
      values_to = "days"
    ) |>

    # create the labels and graph value
    mutate(
      label = ifelse(
        days == 0,
        "< 1 day",
        ifelse(days == 1, paste0(days, " day"), paste0(days, " days"))
      ),
      graph_value = days + 1
    ) |>

    separate(dates, c("first", "second"), sep = "__", remove = FALSE) |>

    mutate(
      first = factor(first, levels = date_vars, label = labels(date_vars)),

      second = factor(second, levels = date_vars, label = labels(date_vars))
    ) |>

    arrange(first, .by_group = TRUE) |>

    mutate(
      .by = stat,
      end = cumsum(graph_value),
      start = lag(end),
      start = if_else(is.na(start), 0, start),
      #default position will be overide if group_var
      position = 0
    )

  outputs <- list(invalid_delay = invalid_df,
                  summary_delay = delay_summary)

  return(outputs)

}
# PLOT DELAYS DISTRIBUTIONS ======================================================================================================================
delay_df <- get_delay_df(dat, date_vars)

delay_summary <- get_delay_summary(delay_df)

plot_delay_bar <- function(delay_df, delay_summary, var ){

  # filter invalid and delay based on var
  delay_fil <- purrr::map(delay_summary, ~filter(.x, dates == var))

  # function to retrieve stats from summary
  get_stat <- function(delay_summary, dates_name, stat_name = c("mean", "median")){

    delay_fil$summary_delay |> filter(stat == stat_name) |> pull(days)

  }

  hc <- highchart() |>
    hc_chart(type = "column", zoomtype = "x") |>
    hc_add_series(
      data = as.numeric(table(delay_df[[var]])),
      name = "count",
      type = "column"
    ) |>
    hc_yAxis(title = list(text = "Count")) |>
    hc_xAxis(
      title = list(text = "Delay (days)"),
      allowDecimals = FALSE,
      plotLines = list(
        list(
          color = "red", zIndex = 1, value = get_stat(delay_fil, var, "mean"),
          label = list(text = "Mean", verticalAlign = "top", textAlign = "left")
        ),
        list(
          color = "red", zIndex = 1, value = get_stat(delay_summary$summary_delay, var, "median"),
          label = list(text = "Median", verticalAlign = "top", textAlign = "left")
        )
      )
    ) |>
    hc_tooltip(pointFormat = "<b>{point.y}</b> occurrences") |>
    hc_colors(blue) |>
    hc_credits(enabled = TRUE, text = glue::glue("Using {delay_fil$invalid_delay$n_valid} valid delays ({delay_fil$invalid_delay$n_na} missing, {delay_fil$invalid_delay$n_co + delay_fil$invalid_delay$n_invalid} invalid)"))

return(hc)
}

plot_delay_bar(delay_df, delay_summary, var = "date_onset__date_outcome")

# PLOT TIMELINE  ======================================================================================================================
















timespan_long <- timespan_ls |>
  pivot_longer(contains("__"),
               names_to = "date_var",
               values_to = "timespan")

if (!is.null(group_var)) {
  timespan_long <- timespan_long |>
    group_by(!!group_var_sym)
}
# get invalid DF
invalid_df <- timespan_long |>
  group_by(date_var, .add = TRUE) |>
  summarise(
    N = n(),
    n_na = sum(is.na(timespan)),
    n_invalid = sum(timespan < 0, na.rm = TRUE),
    n_co = sum(timespan >= co_value, na.rm = TRUE),
    .groups = "drop"
  )

#clean the timespan data
timespan_long <- timespan_long |>
  drop_na(timespan) |>
  filter(between(timespan, 0, co_value)) |>

  group_by(date_var, .add = TRUE) |>

  filter(n() > min_obs)

