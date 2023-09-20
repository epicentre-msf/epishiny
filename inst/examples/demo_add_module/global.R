library(shiny)
library(bslib)
library(epishiny)

# Additional packages
library(EpiEstim)
library(epiparameter)

# options(
#   "epishiny.na.label" = "(Manquant)",
#   "epishiny.week.letter" = "S"
# )

app_title = "epishiny modules"
app_font <- "Roboto Mono"

# example package data
data("df_ll") # linelist
data("sf_yem") # sf geo boundaries for Yemen admin 1 & 2

# setup geo data for adm1 and adm2 in the format
# required for epishiny map module
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

epi_pal <- list(
  PRIMARY = "#2E569E",
  SECONDARY = "#D37331",
  SUCCESS = "#94BA3B"
)

# R estimation function

estimate_func <- function(x){
  output_R <- estimate_R(incid = x,
                         method = "parametric_si",
                         config = make_config(list(
                           mean_si = 5, std_si = 1)))
  
  out_R <- c(rep(NA,7),output_R$R$`Mean(R)`) # Add buffer so length is the same
  out_R
}


