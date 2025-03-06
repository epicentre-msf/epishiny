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

# DELAY DF  ======================================================================================================================

get_delay_df <- function(dat, date_vars, group_var = NULL) {
  # Get all unique pairs of date variables
  date_combinations <- combn(date_vars, 2, simplify = FALSE)

  dat <- dat |>

    # Ensure dates are in Date format - or ask for it to be date ?
    mutate(across(all_of(date_vars), as.Date))

  # Function to calculate delay between two date variables
  calculate_delay <- function(x, dates) {
    var_name <- paste0(dates[[1]], "__", dates[[2]])

    x |>

      transmute(
        !!var_name := as.integer(.data[[dates[[2]]]] - .data[[dates[[1]]]])
      )
  }

  # apply delay function to all combination and get df
  final_df <- purrr::map(
    date_combinations,
    ~ calculate_delay(dat, .x)
  ) |>
    bind_cols() |>

    bind_cols(select(dat, all_of(group_var)))

  return(final_df)
}

# PLOT DELAYS DISTRIBUTIONS ======================================================================================================================
plot_delay_bar <- function(
  delay_df,
  delay,
  co_value = 30,
  group_var = NULL
) {
  if (length(group_var) > 1) {
    stop("Please provide only one group variable")
  }

  if (length(group_var) && !any(names(delay_df) %in% group_var)) {
    stop("Please provide a valid group variable")
  }

  #make group_var useable as variable name
  if (!is.null(group_var)) {
    group_var_sym <- sym(group_var)
  }

  # Pivot delay_df
  df <- delay_df |>
    select(all_of(c(delay, group_var))) |>
    # rename the delay to timespan
    rename("timespan" = 1)

  # get invalid df
  invalid_df <- df |>
    summarise(
      N = n(),
      n_na = sum(is.na(timespan)),
      n_invalid = sum(timespan < 0, na.rm = TRUE),
      n_co = sum(timespan >= co_value, na.rm = TRUE),
      n_valid = N - (n_na + n_invalid + n_co)
    )

  # filter only valid ranges
  df_clean <- filter(
    df,
    between(timespan, 0, co_value)
  )

  # get summary stat
  stat_df <- df_clean |>
    summarise(
      min = min(timespan, na.rm = TRUE),
      mean = mean(timespan),
      median = median(timespan),
      max = max(timespan)
    )

  # get hc_df
  if (is.null(group_var)) {
    hc_df <- df_clean |> count(timespan)
  } else {
    hc_df <- df_clean |> count(.data[[group_var]], timespan)
  }

  if (is.null(group_var)) {
    hc <- highchart() |>
      hc_chart(type = "column", zoomtype = "x") |>
      hc_add_series(
        data = hc_df,
        hcaes(x = timespan, y = n),
        name = "count",
        type = "column"
      ) |>
      hc_colors("#2E4473") |>

      hc_tooltip(pointFormat = "<b>{point.}</b> occurrences")
  } else {
    hc <- highchart() |>
      hc_chart(type = "column", zoomtype = "x") |>
      hc_add_series(
        data = hc_df,
        hcaes(x = timespan, y = n, group = !!group_var_sym),
        type = "column",
        stacking = "normal"
      ) |>
      hc_tooltip(
        pointFormat = "<b>{series.name}</b><br><b>{point.y}</b> occurrences"
      )
  }

  hc <- hc |>

    hc_yAxis(title = list(text = "Count")) |>
    hc_xAxis(
      title = list(text = "Delay (days)"),
      allowDecimals = FALSE,
      plotLines = list(
        list(
          color = "red",
          zIndex = 1,
          value = stat_df$mean,
          label = list(text = "Mean", verticalAlign = "top", textAlign = "left")
        ),
        list(
          color = "red",
          zIndex = 1,
          value = stat_df$median,
          label = list(
            text = "Median",
            verticalAlign = "top",
            textAlign = "left"
          )
        )
      )
    ) |>

    hc_credits(
      enabled = TRUE,
      text = glue::glue(
        "Using {invalid_df$n_valid} valid delays ({invalid_df$n_na} missing, {invalid_df$n_co + invalid_df$n_invalid} invalid)"
      )
    )

  return(hc)
}

delay_df <- get_delay_df(dat, date_vars, group_var = "sex")

plot_delay_bar(
  delay_df,
  delay = "date_onset__date_consultation",
  co_value = 30,
  group_var = "sex"
)

# PLOT TIMELINE  ======================================================================================================================

plot_timeline <- function(
  delay_df,
  statistic,
  date_var_seq,
  co_value,
  group_var = NULL,
  color_pal = c("#FFEDA0", "#FEB24C", "#F03B20")
) {
  # use the delay_summary to plot the timeline
  max <- length(date_var_seq)

  if (max == 1) {
    stop("Please provide at least two date variables")
  }

  steps <- paste0(date_var_seq[-max], "__", date_var_seq[-1])

  # Pivot delay_df
  delay_long <- delay_df |>
    select(all_of(steps)) |>
    pivot_longer(all_of(steps), names_to = "dates", values_to = "timespan") |>
    mutate(timespan = as.integer(timespan))

  # get invalid df
  invalid_df <- delay_long |>
    summarise(
      .by = dates,
      N = n(),
      n_na = sum(is.na(timespan)),
      n_invalid = sum(timespan < 0, na.rm = TRUE),
      n_co = sum(timespan >= co_value, na.rm = TRUE),
      n_valid = N - (n_na + n_invalid + n_co)
    )

  # filter only valid ranges
  delay_fil <- filter(
    delay_long,
    between(timespan, 0, co_value)
  )

  # get Summary stat and process for plot
  xrange_df <-
    delay_fil |>
    summarise(
      .by = dates,
      n_valid = n(),
      min = min(timespan, na.rm = TRUE),
      mean = mean(timespan),
      median = median(timespan),
      max = max(timespan)
    ) |>
    pivot_longer(
      c(mean, median),
      names_to = "stat",
      values_to = "days"
    ) |>
    mutate(days = as.integer(days)) |>

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
      first = factor(
        first,
        levels = date_var_seq,
        label = labels(date_var_seq)
      ),

      second = factor(
        second,
        levels = date_var_seq,
        label = labels(date_var_seq)
      )
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

  #tooltip df + filter xrange based on stat
  xrange_final <- xrange_df |>

    mutate(
      .by = c(first, second, min, max, n_valid),
      tooltip_label = paste0(
        "<br><b>",
        first,
        " - ",
        second,
        "</b><br> Range: ",
        min,
        " - ",
        max,
        "<br>Median: ",
        label[stat == "median"],
        "<br>Mean: ",
        label[stat == "mean"],
        "<br><hr><i>based on ",
        n_valid,
        " valid intervals"
      )
    ) |>
    filter(stat == statistic)

  # create a df for the events to be plotted
  events_df <- tibble(
    event = date_var_seq,
    event_lab = names(date_var_seq),
    row.names = NULL
  ) |>

    left_join(
      xrange_df[c(
        "dates",
        "first",
        "start",
        "end"
      )],
      by = c("event_lab" = "first")
    ) |>

    mutate(
      start = if_else(
        event_lab == names(tail(date_var_seq, 1)),
        max(end, na.rm = TRUE),
        start
      ),
      end = if_else(event_lab == names(tail(date_var_seq, 1)), start, end)
    )

  if (length(color_pal) != length(steps)) {
    stop(
      "color_pal must have the same length as the number of intervals (ie: if 3 dates provided, number of interval is 2"
    )
  }

  # color palette
  val_col <- data.frame(value = unique(xrange_final$first), col = color_pal)

  xrange_final$color <- val_col$col[match(xrange_final$first, val_col$value)]

  # PLOT TIMELINE ======================================================================================================================
  highchart() |>

    hc_add_series(
      "xrange",
      name = "timespan",
      data = xrange_final,
      hcaes(x = start, x2 = end, y = position, color = color),

      colorByPoint = TRUE,
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) |>

    hc_add_series(
      "scatter",
      name = "event",
      data = events_df,
      hcaes(x = start, y = 0),
      color = "darkred",
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) |>

    hc_xAxis(
      visible = FALSE,

      #extend the limit of the x axis to display the last event label
      max = max(events_df$start) + 1
    ) |>

    hc_yAxis(
      visible = if (is.null(group_var)) {
        FALSE
      },

      categories = if (!is.null(group_var)) {
        group_cat
      }
    ) |>

    hc_plotOptions(
      scatter = list(
        dataLabels = list(
          enabled = is.null(group_var),
          y = -15,
          align = "left",
          allowOverlap = TRUE,
          rotation = -20,
          style = list(fontSize = "10px", color = "black"),

          formatter = JS(
            "function(){
          outHTML =  this.point.event_lab
          return(outHTML) }"
          ),

          marker = list(radius = 10)
        )
      ),
      xrange = list(
        pointWidth = 15,
        opacity = .7,

        dataLabels = list(
          enabled = TRUE,
          y = +15,
          style = list(fontSize = "9px", color = "#34495E"),
          formatter = JS(
            "function(){
            outHTML =  this.point.label
            return(outHTML)}"
          )
        )
      )
    ) |>

    hc_tooltip(
      useHTML = T,
      formatter = JS(
        "
           function(){

           outHTML = this.point.tooltip_label

           return(outHTML)
           }"
      )
    ) |>

    hc_exporting(enabled = TRUE) |>

    highcharter::hc_credits(
      enabled = TRUE,
      text = glue::glue(
        "{sum(c(invalid_df$n_invalid, invalid_df$n_co), na.rm = TRUE)} ranges removed ({sum(na.rm = TRUE, invalid_df$n_invalid)} negatives, {sum(na.rm = TRUE, invalid_df$n_co)} over cut-Off)"
      )
    )
}

plot_timeline(
  delay_df,
  date_var_seq = c(
    "Onset" = "date_onset",
    "Consultation" = "date_consultation",
    "Outcome" = "date_outcome"
  ),
  co_value = 30,
  statistic = "mean",
  group_var = NULL,
  color_pal = c("green", "red")
)
