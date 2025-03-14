#' Plot Delay Distributions
#'
#' This function generates a bar plot of delay distributions from a provided data frame containing delay data.
#' The plot shows the counts of delays within specified ranges, and optionally fits and overlays a distribution (e.g., Gamma, Weibull, Lognormal).
#' It can also display the distribution parameters and log-likelihood values in tooltips for the fitted distributions.
#'
#' @param delay_df A data frame containing the delay data and optionally a grouping variable.
#' @param delay A character string specifying the name of the column in `delay_df` that contains the delay values.
#' @param co_value A numeric value specifying the threshold for counting delays as outliers (default is 30).
#' @param group_var (Optional) A character string specifying the name of a grouping variable in `delay_df`. The function will produce separate bars for each group.
#' @param fit_dist A logical value indicating whether to fit and display a distribution curve (default is `FALSE`).
#' @param which_dist A character vector specifying which distributions to fit. Options are "gamma", "weibull", and "lnorm" (default is `c("gamma", "weibull", "lnorm")`).
#'
#' @return A highchart object showing the distribution of delays, with optional overlays for fitted distributions.
#' If `fit_dist` is `TRUE`, the plot includes density curves based on the fitted distributions.
#' Tooltips display distribution parameters and log-likelihood values if fitted distributions are included.
#'
#' @examples
#' # Example data frame
#' dat <- data.frame(
#'   id = rep(1:3, each = 5),
#'   delay = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Plot delay distributions without fitting a distribution
#' plot_delay_bar(dat, delay = "delay")
#'
#' # Plot delay distributions with fitted distributions (Gamma, Weibull, Lognormal)
#' plot_delay_bar(dat, delay = "delay", fit_dist = TRUE)
#'
#' # Plot with grouping variable
#' plot_delay_bar(dat, delay = "delay", group_var = "id", fit_dist = TRUE)
#'
#' @importFrom dplyr select rename summarise filter count mutate
#' @importFrom fitdistrplus fitdist
#' @importFrom highcharter highchart hc_chart hc_add_series hc_tooltip hc_yAxis_multiples hc_xAxis hc_plotOptions hc_credits
#' @importFrom rlang sym
#' @importFrom glue glue
#' @export
plot_delay_bar <- function(
  delay_df,
  delay,
  co_value = 30,
  group_var = NULL,
  fit_dist = FALSE,
  which_dist = c("gamma", "weibull", "lnorm"),
  show_stat = c("mean", "median")
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

    if (length(values) < 10) {
      message("Not enough data to fit a distribution")
      fit_dist <- FALSE
    }
  }

  if (fit_dist) {
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
        hcaes(x = as.factor(timespan), y = n),
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
        hcaes(x = as.factor(timespan), y = n, group = !!group_var_sym),
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

  # Stat line ?
  stat_line <- list()

  if ("mean" %in% show_stat) {
    stat_line <- append(
      stat_line,
      list(
        list(
          color = "red",
          zIndex = 1,
          value = stat_df$mean,
          label = list(
            text = "Mean",
            verticalAlign = "top",
            textAlign = "left"
          )
        )
      )
    )
  }

  if ("median" %in% show_stat) {
    stat_line <- append(
      stat_line,
      list(
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
    )
  }

  hc <- hc |>

    hc_xAxis(type = "category") |> # Ensures bars align properly

    hc_xAxis(
      title = list(text = "Delay (days)"),
      allowDecimals = FALSE,
      # Show stat line
      plotLines = stat_line
    ) |>

    hc_plotOptions(
      column = list(
        pointPadding = 0, # No spacing within individual bars
        groupPadding = 0, # No spacing between different bars
        borderWidth = 0, # Keeps visible separation between bars
        shadow = FALSE
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

# TEST ZONE ---------------------------------------------------------------
#
# linelist <- episimdata::moissala_linelist_clean_EN
# date_vars <- c(
#   "date_onset",
#   "date_consultation",
#   "date_admission",
#   "date_outcome"
# )
#
# # calculate delays
# delay_df <- get_delay_df(linelist, date_vars, "sex")
#
# plot_delay_bar(
#   delay_df,
#   "date_onset__date_outcome",
#   fit_dist = TRUE
# )
