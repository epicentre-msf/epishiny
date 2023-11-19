library(shiny)
library(bslib)
library(epishiny)

options(
  "epishiny.na.label" = "(Missing)",
  "epishiny.count.label" = "Patients",
  "epishiny.week.letter" = "W",
  "epishiny.week.start" = 1
)

app_title = "epishiny modules"
app_font <- "Roboto Mono"

# example package data
data("df_ll") # linelist
data("sf_yem") # sf geo boundaries for Yemen admin 1 & 2

# setup geo data for adm1 and adm2 in the format
# required for epishiny map module
geo_data <- list(
  geo_layer(
    layer_name = "Governorate",
    sf = sf_yem$adm1,
    name_var = "adm1_name",
    pop_var = "adm1_pop",
    join_by = c("pcode" = "adm1_pcode")
  ),
  geo_layer(
    layer_name = "District",
    sf = sf_yem$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode")
  )
)

# range of dates used in filter module to filter time period
date_range <- range(df_ll$date_notification, na.rm = TRUE)

# define date variables in data as named list to be used in app
date_vars <- c(
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_start",
  "Date of hospitalisation" = "date_hospitalisation_start",
  "Date of outcome" = "date_hospitalisation_end"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Governorate" = "adm1_origin",
  "Sex" = "sex_id",
  "Hospitalised" = "hospitalised_yn",
  "Vaccinated measles" = "vacci_measles_yn",
  "Outcome" = "outcome"
)

