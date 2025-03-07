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
  group_var = NULL,
  fit_dist = FALSE,
  which_dist = c("gamma", "weibull", "lnorm")
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

  # Fit a distribution to data
  if (fit_dist) {
    #extract values used for fitting, remove 0s
    values <- df_clean |> filter(timespan > 0) |> pull(timespan)

    fit_results <- list()

    for (i in which_dist) {
      fit <- fitdistrplus::fitdist(values, distr = i)

      fit_results[[i]] <- fit
    }

    # structure the results
    results_list <- lapply(names(fit_results), function(d) {
      fit <- fit_results[[d]]
      param_df <- as.data.frame(t(fit$estimate)) # Convert parameter estimates into a dataframe
      param_df$distribution <- d
      param_df$loglik <- fit$loglik
      return(param_df)
    })

    # Bind all results into a single data frame
    results_df <- bind_rows(results_list) %>%
      relocate(distribution, loglik) # Ensure distribution name is the first column

    #function to compute density
    compute_density <- function(dist_name, params) {
      if (dist_name == "gamma") {
        return(dgamma(
          value_seq,
          shape = params["shape"],
          rate = params["rate"]
        ))
      } else if (dist_name == "weibull") {
        return(dweibull(
          value_seq,
          shape = params["shape"],
          scale = params["scale"]
        ))
      } else if (dist_name == "lnorm") {
        return(dlnorm(
          value_seq,
          meanlog = params["meanlog"],
          sdlog = params["sdlog"]
        ))
      } else {
        return(rep(NA, length(value_seq))) # Return NA if the distribution is not recognized
      }
    }
    # Create a sequence of values to compute densities
    value_seq <- seq(min(values), max(values), length.out = 100)

    # Compute densities
    density_list <- lapply(1:nrow(results_df), function(i) {
      dist_name <- results_df$distribution[i]
      params <- results_df[i, -1, drop = FALSE] # Extract only parameter values
      params <- as.numeric(params) # Convert to numeric
      names(params) <- names(results_df)[-1] # Assign parameter names

      tooltip_text <- paste0(
        "<b>Distribution:</b> ",
        dist_name,
        "<br>",
        if (!is.na(params["shape"]))
          paste0("<b>Shape:</b> ", round(params["shape"], 4), "<br>") else "",
        if (!is.na(params["rate"]))
          paste0("<b>Rate:</b> ", round(params["rate"], 4), "<br>") else "",
        if (!is.na(params["scale"]))
          paste0("<b>Scale:</b> ", round(params["scale"], 4), "<br>") else "",
        if (!is.na(params["meanlog"]))
          paste0("<b>Meanlog:</b> ", round(params["meanlog"], 4), "<br>") else
          "",
        if (!is.na(params["sdlog"]))
          paste0("<b>Sdlog:</b> ", round(params["sdlog"], 4), "<br>") else "",
        "<br>",
        "<b>Log-likelihood:</b> ",
        round(results_df$loglik[i], 4)
      )

      data.frame(
        values = value_seq,
        density = compute_density(dist_name, params),
        distribution = dist_name,
        tooltip = tooltip_text
      )
    })

    dist_df <- bind_rows(density_list)
  }

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
    hc_df <- df_clean |>
      count(timespan) |>
      mutate(
        tooltip = paste0(
          "<b>Delay:</b> ",
          timespan,
          " days",
          "<br>",
          n,
          " occurences"
        )
      )
  } else {
    hc_df <- df_clean |>
      count(.data[[group_var]], timespan) |>
      mutate(
        tooltip = paste0(
          "<b>",
          .data[[group_var]],
          "</b><br>",
          "<b>Delay:</b> ",
          timespan,
          " days",
          "<br>",
          n,
          " occurences"
        )
      )
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
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
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
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
  }

  # Add fitted distribution
  if (fit_dist) {
    hc <- hc |>
      hc_yAxis_multiples(
        list(title = list(text = "Counts"), opposite = FALSE),
        list(title = list(text = "Density"), opposite = TRUE)
      ) |>

      # Add Gamma Density Curve
      hc_add_series(
        type = "spline",
        data = dist_df,
        marker = list(enabled = FALSE),
        hcaes(x = values, y = density, group = distribution, tooltip = tooltip),
        lineWidth = 2,
        yAxis = 1
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
  }

  hc <- hc |>

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
    hc_plotOptions(
      column = list(
        pointWidth = 45
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
  delay = "date_onset__date_outcome",
  co_value = 30,
  group_var = "sex",
  fit_dist = TRUE
)

# PLOT TIMELINE  ======================================================================================================================

plot_timeline <- function(
  delay_df,
  statistic,
  date_var_seq,
  co_value,
  group_var = NULL,
  color_pal = c("#FFEDA0", "#FEB24C", "#F03B20"),
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

  # Create a Df of event points to be plotted
  last_event <- names(tail(date_var_seq, 1))

  events_df <- tibble(
    event = date_var_seq,
    event_lab = names(date_var_seq),
    row.names = NULL
  ) |>
    left_join(
      distinct(xrange_final, first, end, start, position),
      by = c("event_lab" = "first")
    ) |>

    mutate(
      start = if_else(
        event_lab == last_event,
        max(end, na.rm = TRUE),
        start
      )
    ) |>
    filter(event_lab != last_event) |>
    select(event, event_lab, start, position)

  # special case for the last event

  n_position <- length(unique(xrange_final$position))

  last_event_df <- tibble(
    event = tail(date_var_seq, 1),
    event_lab = last_event,
    start = max(xrange_final$end, na.rm = TRUE)
  ) |>
    slice(rep(1, n_position)) |>
    mutate(position = c(seq(0, to = max(xrange_final$position))))

  # bind last event back
  events_df <- events_df |> bind_rows(last_event_df)

  if (!is.null(group_var)) {
    group_cat <- levels(xrange_final[xrange_final$stat == statistic, ]$group)
  }

  # PLOT TIMELINE ======================================================================================================================
  if (length(color_pal) != length(steps)) {
    stop(
      "color_pal must have the same length as the number of intervals (ie: if 3 dates provided, number of interval is 2"
    )
  }

  # color palette
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
      hcaes(x = start, y = position),
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
          enabled = display_lab,
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

delay_df <- get_delay_df(dat, date_vars, group_var = "sex")

plot_timeline(
  delay_df,
  date_var_seq = c(
    "Onset" = "date_onset",
    "Consultation" = "date_consultation",
    "Admission" = "date_admission",
    "Outcome" = "date_outcome"
  ),
  co_value = 30,
  statistic = "mean",
  group_var = "sex",
  display_lab = TRUE
)
