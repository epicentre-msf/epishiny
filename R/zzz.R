.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "epishiny",
    system.file("assets", package = "epishiny")
  )
}

.onAttach <- function(libname, pkgname) {
  # set epishiny default options =============
  options(
    epishiny.na.label = "(Missing)",
    epishiny.count.label = "Patients",
    epishiny.week.letter = "W",
    epishiny.week.start = 1
  )

  # default highcharter theming options =============
  fntfmly <- '"Roboto Mono",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'

  hc_opts <- getOption("highcharter.chart")

  hc_opts$colors <- scales::alpha(epi_pals()$d310, .8)

  hc_opts$plotOptions$column <- list(
    zIndex = 2,
    stacking = "normal",
    groupPadding = 0.05,
    pointPadding = 0.05,
    borderWidth = 1,
    borderColor = "white"
  )

  hc_opts$plotOptions$bar <- list(
    zIndex = 2,
    stacking = "normal",
    groupPadding = 0.05,
    pointPadding = 0.05,
    borderWidth = 0.05
  )

  hc_opts$plotOptions$line <- list(
    zIndex = 10,
    marker = list(enabled = TRUE)
  )

  hc_opts$credits <- list(
    enabled = FALSE,
    href = "",
    style = list(fontFamily = fntfmly, fontSize = "10px", fontStyle = "italic", cursor = "default")
  )

  hc_opts$caption <- list(
    enabled = FALSE,
    href = "",
    style = list(fontFamily = fntfmly, fontSize = "10px", cursor = "default")
  )

  hc_opts$exporting <- list(enabled = FALSE)

  options(
    highcharter.chart = hc_opts,
    highcharter.theme = highcharter::hc_theme_smpl(
      chart = list(style = list(fontFamily = fntfmly)),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly))
    )
  )
}

#' Example Linelist Data
#'
#' A 'linelist' is a (tidy) data format used in public health data collection 
#' with each row representing an individual (patient, participant, etc) and 
#' each column representing a variable associated with said individual.
#'
#' `df_ll` is an example linelist dataset containing data for a fake measles
#' outbreak in Yemen. The data contains temporal, demographic, and geographic
#' information for each patient, as well as other medical indicators.
#'
#' @format a tibble dataframe
#' @examples
#' df_ll
"df_ll"

#' Yemen Governorate (adm1) and District (adm2) Administrative Boundaries
#'
#' A list of length 2 containing geographic administrative boundary data
#' for Yemen, stored as simple features (sf) objects. 
#' 
#' Each admin level can be joined to the example [`df_ll`] dataset with a join by
#' specification of `c("pcode" = "adm1_pcode")` and `c("pcode" = "adm2_pcode")`
#' respectively. These should be passed as the `join_by` field in each `geo_data`
#' specification passed to [place_ui] and [place_server].
#'
#' @format named list of sf objects
#' @examples
#' sf_yem$adm1
#' sf_yem$adm2
"sf_yem"
