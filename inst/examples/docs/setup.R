library(epishiny)

# example package data
data("df_ll")
data("sf_yem")

# setup geo data for adm1 and adm2 in the format
# required for epishiny map module
geo_data <- list(
  "adm1" = list(
    level_name = "Governorate", # name of the boundary level
    sf = sf_yem$adm1, # sf object with boundary polygons
    name_var = "adm1_name", # variable with place names
    pop_var = "adm1_pop", # variable with population data (optional)
    join_by = c("pcode" = "adm1_pcode") # geo to data join vars: LHS = sf, RHS = data
  ),
  # same for admin 2 level
  "adm2" = list(
    level_name = "District",
    sf = sf_yem$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode")
  )
)

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
