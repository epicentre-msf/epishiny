
# epishiny

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Interactive web-based data visualisations and dashboards are an increasingly common method of monitoring infectious disease outbreaks. Whilst R is now a commonly used tool for analysis and data visualisation in epidemiology, epidemiologists will often lack the knowledge and training required to be able to produce interactive dashboards directly from within their R workflows, resulting in external propriatory software being used instead.

`epishiny` aims to bridge this gap by providing simple functions that produce engaging interactive visualisations and dashboards of epidemiological linelist data using R's [`shiny`](https://shiny.posit.co/) web-framework. 

## Installation

You can install the development version of epishiny from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/epishiny")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(epishiny)

# example package data
data("df_ll")
data("sf_yem")

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

# define date variable in data as named list
date_vars <- c(
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_start",
  "Date of hospitalisation" = "date_hospitalisation_start",
  "Date of outcome" = "date_hospitalisation_end"
)

# define categorical grouping variables in data as named list
group_vars <- c(
  "Governorate" = "adm1_origin",
  "Sex" = "sex_id",
  "Hospitalised" = "hospitalised_yn",
  "Vaccinated measles" = "vacci_measles_yn",
  "Outcome" = "outcome"
)

# launch epicurve module
launch_module(
  module = "epicurve",
  df_ll = df_ll,
  date_vars = date_vars,
  group_vars = group_vars,
  ratio_line_lab = "Show CFR line?",
  ratio_var = "outcome",
  ratio_lab = "CFR",
  ratio_numer = "Deceased",
  ratio_denom = c("Deceased", "Healed", "Abandonment")
)

# launch map module
launch_module(
  module = "map",
  df_ll = df_ll,
  geo_data = geo_data,
  group_vars = group_vars
)

# launch age/sex pyramid module
launch_module(
  module = "pyramid",
  df_ll = df_ll,
  age_var = "age_years",
  sex_var = "sex_id",
  male_level = "Male",
  female_level = "Female"
)

```

