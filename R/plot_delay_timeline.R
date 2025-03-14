#' Plot Timeline of Delay Intervals
#'
#' This function creates a timeline plot to visualize the delays between successive date variables in a dataset. It generates a highcharter plot where each date pair is represented as a horizontal bar, and the events are shown as scatter points. The function supports optional grouping of data and displays various statistics related to the delays.
#'
#' @param delay_df A data frame containing the delay data, with at least the date variables specified in `date_var_seq`.
#' @param statistic A string indicating the statistic to display. It must be one of "mean" or "median" for the timespans between date intervals.
#' @param date_var_seq A character vector containing the sequence of date variables. The number of date variables must be at least 2.
#' @param co_value A numeric value representing the cut-off for valid delays. Delays exceeding this value will be considered invalid.
#' @param group_var An optional string specifying the name of a grouping variable in the data. If provided, the data will be grouped by this variable for visualization.
#' @param color_pal A character vector of colors to be used for the date intervals. The length of this vector should match the number of intervals in `date_var_seq` (i.e., `length(date_var_seq) - 1`).
#' @param display_lab A logical value indicating whether to display labels for the events in the plot. Defaults to `TRUE`.
#'
#' @return A highcharter object representing the timeline plot.
#'
#' @details The function processes the input data frame to calculate delay timespans between each pair of date variables in `date_var_seq`. It then creates a horizontal bar plot for each valid interval, with optional grouping and statistical labels. Tooltips are included to show detailed information about each event.
#'
#' The plot also includes scatter points to represent the events at each stage in the timeline. The `color_pal` argument allows users to specify a custom color palette for the intervals, while the `group_var` argument enables grouping by a specific variable.
#'
#' The timeline plot helps in visualizing the distribution and relationships of delay intervals across different stages in a process.
#'
#' @importFrom dplyr select mutate filter summarise count arrange pivot_longer
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_c
#' @importFrom forcats fct_reorder
#' @importFrom highcharter highchart hc_add_series hc_tooltip hc_xAxis hc_yAxis hc_plotOptions hc_credits
#' @importFrom glue glue
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_delay_timeline(
#'   delay_df = my_data,
#'   statistic = "mean",
#'   date_var_seq = c("start_date", "mid_date", "end_date"),
#'   co_value = 30,
#'   group_var = "group",
#'   color_pal = c("#FFEDA0", "#FEB24C", "#F03B20"),
#'   display_lab = TRUE
#' )
#' }
#'
#' @export
plot_delay_timeline <- function(
  delay_df,
  statistic,
  date_var_seq,
  co_value,
  group_var = NULL,
  display_lab = TRUE
) {
  max <- length(date_var_seq)

  steps <- paste0(date_var_seq[-max], "__", date_var_seq[-1])

  if (max == 1) {
    stop("Please provide at least two date variables")
  }

  #make group_var useable as variable name
  if (!is.null(group_var)) {
    group_var_sym <- sym(group_var)
  }

  # Pivot delay_df
  delay_long <- delay_df |>
    select(all_of(c(steps, group_var))) |>
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

  # get summary stat and process for plot
  xrange_df <- delay_fil |>
    summarise(
      .by = c(dates, group_var),
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
      .by = c(stat, group_var),
      end = cumsum(graph_value),
      start = lag(end),
      start = if_else(is.na(start), 0, start),
      #default position will be overide if group_var
      position = 0
    )

  # make the group variable as factor
  if (!is.null(group_var)) {
    xrange_final <- xrange_df |>
      mutate(group = factor(!!group_var_sym)) |>
      mutate(
        .by = group,
        sum = sum(graph_value[stat == statistic], na.rm = TRUE)
      ) |>
      arrange(sum) |>
      mutate(group = fct_reorder(group, sum)) |>
      mutate(
        .by = group,
        position = cur_group_id() - 1,
        group = fct_reorder(group, position)
      ) |>

      mutate(
        .by = c(first, second, min, max, n_valid, group),
        tooltip_label = paste0(
          "<b>",
          group,
          "</b><br><b>",
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
  } else {
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
  }

  # Create a df of the events to be plotted as scatter and labels
  last_event <- names(tail(date_var_seq, 1))

  events_df <- xrange_final |>
    select(any_of(c(
      "first",
      "second",
      "start",
      "end",
      "graph_value",
      "group",
      "position"
    ))) |>
    mutate(
      event = map(first, ~.x),
      event_2 = map2(first, second, ~ c(.x, .y)),
      event_final = if_else(second == last_event, event_2, event)
    ) |>
    unnest(event_final) |>

    mutate(x = if_else(event_final == last_event, end, start)) |>
    select(event_final, x, position)

  if (!is.null(group_var)) {
    group_cat <- levels(xrange_final[xrange_final$stat == statistic, ]$group)
  }

  # PLOT TIMELINE ======================================================================================================================

  # color palette
  generate_palette <- function(n) {
    # Choose a palette from RColorBrewer
    palette <- RColorBrewer::brewer.pal(n, "Set1") # You can choose from other palettes like "Set3", "Paired", etc.
    return(palette)
  }

  color_pal <- suppressWarnings(generate_palette(length(steps)))

  if (length(steps) == 2) {
    color_pal <- color_pal[-1]
  }

  val_col <- data.frame(value = unique(xrange_final$first), col = color_pal)

  xrange_final$color <- val_col$col[match(xrange_final$first, val_col$value)]

  hc <- highchart() |>

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
      hcaes(x = x, y = position),
      color = "darkred",
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) |>

    hc_xAxis(
      visible = FALSE,
      #extend the limit of the x axis to display the last event label
      max = max(events_df$x) + 1
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
          enabled = display_lab,
          y = -15,
          align = "left",
          allowOverlap = TRUE,
          rotation = -20,
          style = list(fontSize = "10px", color = "black"),

          formatter = JS(
            "function(){
          outHTML =  this.point.event_final
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
        "function(){
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
  return(hc)
}

# TEST ZONE ========================================================================================================================
# linelist <- episimdata::moissala_linelist_clean_EN
#
# date_vars <- c(
#   "Onset" = "date_onset",
#   "Consultation" = "date_consultation",
#   "Admission" = "date_admission",
#   "Outcome" = "date_outcome"
# )
#
# # calculate delays
# delay_df <- get_delay_df(linelist, date_vars, "age_group")
#
# plot_delay_timeline(
#   delay_df,
#   date_var_seq = date_vars,
#   co_value = 30,
#   statistic = "mean",
#   group_var = "age_group"
# )
