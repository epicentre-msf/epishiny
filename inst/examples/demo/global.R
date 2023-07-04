library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(leaflet.minicharts)
library(highcharter)
pkgload::load_all()

app_title = "epishiny modules"
app_font <- "Roboto Mono"

date_range <- range(df_ll$date_notification, na.rm = TRUE)

date_vars <- c(
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_start",
  "Date of hospitalisation" = "date_hospitalisation_start",
  "Date of outcome" = "date_hospitalisation_end"
)

group_vars <- c(
  "Governorate" = "adm1_origin",
  "Sex" = "sex_id",
  "Hospitalised" = "hospitalised_yn",
  "Vaccinated measles" = "vacci_measles_yn",
  "Outcome" = "outcome"
)

geo_data <- list(
  "adm1" = list(
    level_name = "Governorate",
    sf = sf_yem$adm1,
    name_var = "adm1_name",
    join_by = c("pcode" = "adm1_pcode")
  ),
  "adm2" = list(
    level_name = "District",
    sf = sf_yem$adm2,
    name_var = "adm2_name",
    join_by = c("pcode" = "adm2_pcode")
  )
)

epi_pal <- list(
  PRIMARY = "#2E569E",
  SECONDARY = "#D37331",
  SUCCESS = "#94BA3B"
)
