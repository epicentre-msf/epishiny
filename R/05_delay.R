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
min_obs <- 20

# DELAY DF  ======================================================================================================================

get_delay_df <- function(dat, date_vars) {
  # Get all unique pairs of date variables
  date_combinations <- combn(date_vars, 2, simplify = FALSE)

  dat <- dat |>

    # Ensure dates are in Date format - or ask for it to be date ?
    mutate(across(all_of(date_vars), as.Date))

  # Function to calculate delay between two date variables
  calculate_delay <- function(x, dates) {
    var_name <- paste0(dates[[1]], "__", dates[[2]])

    x |>

      mutate(across(all_of(dates), as.Date)) |>

      transmute(
        !!var_name := as.integer(.data[[dates[[2]]]] - .data[[dates[[1]]]])
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
#
# get_delay_summary <- function(delay_df, co_value = 30, group_var = NULL) {
#   # Pivot delay long
#   delay_long <- delay_df |>
#     pivot_longer(contains("__"), names_to = "dates", values_to = "timespan")
#
#   # get invalid DF
#   invalid_df <- delay_long |>
#     summarise(
#       .by = dates,
#       N = n(),
#       n_na = sum(is.na(timespan)),
#       n_invalid = sum(timespan < 0, na.rm = TRUE),
#       n_co = sum(timespan >= co_value, na.rm = TRUE),
#       n_valid = N - (n_na + n_invalid + n_co)
#     )
#
#   #clean the timespan data
#   delay_long <- delay_long |>
#     # remove NAs
#     drop_na(timespan) |>
#     # keep valid values
#     filter(between(timespan, 0, co_value))
#
#   # calculate summary
#   delay_summary <- delay_long |>
#     summarise(
#       .by = dates,
#       min = round(digits = 2, suppressWarnings(min(timespan, na.rm = TRUE))),
#       mean = round(digits = 2, suppressWarnings(mean(timespan, na.rm = TRUE))),
#       median = round(
#         digits = 2,
#         suppressWarnings(median(timespan, na.rm = TRUE))
#       ),
#       max = round(digits = 2, suppressWarnings(max(timespan, na.rm = TRUE))),
#       n_valid = sum(!is.na(timespan))
#     ) |>
#
#     pivot_longer(
#       c(min, mean, median, max),
#       names_to = "stat",
#       values_to = "days"
#     )
#
#   outputs <- list(invalid_delay = invalid_df, summary_delay = delay_summary)
#
#   return(outputs)
# }

# PLOT DELAYS DISTRIBUTIONS ======================================================================================================================
delay_df <- get_delay_df(dat, date_vars)

plot_delay_bar <- function(delay_df, delay, co_value = 30) {
  # Pivot delay_df
  delay_long <- delay_df |>
    select(delay) |>
    pivot_longer(delay, names_to = "dates", values_to = "timespan")

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

  # filter only valide ranges
  delay_fil <- filter(
    delay_long,
    between(timespan, 0, co_value)
  )

  # get summary stat
  stat_df <- delay_fil |>
    summarise(
      min = min(timespan, na.rm = TRUE),
      mean = mean(timespan),
      median = median(timespan),
      max = max(timespan)
    )

  # get hc_df
  hc_df <- delay_fil |> count(timespan)

  hc <- highchart() |>
    hc_chart(type = "column", zoomtype = "x") |>
    hc_add_series(
      data = hc_df,
      hcaes(x = timespan, y = n),
      name = "count",
      type = "column"
    ) |>
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
    #hc_tooltip(pointFormat = "<b>{point.}</b> occurrences") |>
    hc_colors("#2E4473") |>
    hc_credits(
      enabled = TRUE,
      text = glue::glue(
        "Using {invalid_df$n_valid} valid delays ({invalid_df$n_na} missing, {invalid_df$n_co + invalid_df$n_invalid} invalid)"
      )
    )

  return(hc)
}

plot_delay_bar(delay_df, delay = "date_admission__date_outcome")

# PLOT TIMELINE  ======================================================================================================================

# use the delay_summary to plot the timeline
statistic <- "mean"
max <- length(date_vars)

steps <- paste0(date_vars[-max], "__", date_vars[-1])

stat <- delay_summary$summary_delay |>
  filter(stat == statistic, dates %in% steps)


stat_df <- stat |>

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

events_df <- tibble(
  event = date_vars,
  event_lab = names(date_vars),
  row.names = NULL
) |>

  left_join(
    stat_df[c(
      "dates",
      "first",
      "start",
      "end"
    )],
    by = c("event_lab" = "first")
  ) |>

  mutate(
    start = if_else(
      event_lab == names(tail(date_vars, 1)),
      max(end, na.rm = TRUE),
      start
    ),
    end = if_else(event_lab == names(tail(date_vars, 1)), start, end)
  )

#tooltip df
tooltip_df <- stat_df |>

  group_by(first, second) |>

  reframe(
    tooltip_label = paste0(
      "<br><b>",
      unique(first),
      " - ",
      unique(second),
      "</b><br> Range: ",
      label[stat == "min"],
      " - ",
      label[stat == "max"],
      "<br>Median: ",
      label[stat == "median"],
      "<br>Mean: ",
      label[stat == "mean"],
      "<br><hr><i>based on ",
      unique(n_valid),
      " valid intervals"
    )
  )

stat_df <- left_join(stat_df, tooltip_df, relationship = "many-to-many")

# color palette
pal <- RColorBrewer::brewer.pal("YlOrRd", n = length(date_vars) - 1)

val_col <- data.frame(value = unique(stat_df$first), col = pal)

stat_df$color <- val_col$col[match(stat_df$first, val_col$value)]

# PLOT TIMELINE ======================================================================================================================
highchart() |>

  hc_add_series(
    "xrange",
    name = "timespan",
    data = stat_df[stat_df$stat == statistic, ],
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
