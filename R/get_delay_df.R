#' Get Delay DataFrame
#'
#' This function calculates the delay between all combinations of two date variables in a data frame.
#' It returns a new data frame containing the delay for each unique pair of dates. The delay is calculated
#' as the difference (in days) between the two dates. Additionally, it optionally includes a grouping variable
#' from the input data frame.
#'
#' @param dat A data frame containing the date variables and optionally a grouping variable.
#' @param date_vars A character vector of the names of the date variables for which the delays should be calculated.
#' @param group_var (Optional) A character string specifying the name of a grouping variable to include in the result.
#' If not provided, no grouping variable will be included in the output.
#'
#' @return A data frame containing the delay for each unique pair of date variables in `date_vars` as columns.
#' If `group_var` is provided, it is also included as an additional column in the result.
#'
#' @examples
#' # Example data frame
#' dat <- data.frame(
#'   id = 1:5,
#'   start_date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")),
#'   end_date = as.Date(c("2025-01-05", "2025-01-06", "2025-01-07", "2025-01-08", "2025-01-09"))
#' )
#'
#' # Example usage
#' get_delay_df(dat, date_vars = c("start_date", "end_date"))
#'
#' # With a grouping variable
#' get_delay_df(dat, date_vars = c("start_date", "end_date"), group_var = "id")
#'
#' @importFrom dplyr mutate across transmute select bind_cols
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
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
