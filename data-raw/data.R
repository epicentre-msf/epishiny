# code to prepare package example data
library(tidyverse)
library(fs)

# Yemen admin 1 and 2
dir_geo <- max(dir_ls("~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/OutbreakTools - GeoBase/YEM"))

sf_yem <-
  dir_ls(path(dir_geo, "sf"), regexp = "adm[1-2]") %>%
  set_names(c("adm1", "adm2")) %>% 
  # remove coords to show epishiny will add them for you if missing
  map(~ read_rds(.x) %>% select(-any_of(c("adm0_sub", "lon", "lat"))))

# path to example Outbreak Tools linelist export: a measles dataset with linelist "Linelist patients"
path_ll <- here::here("data-raw", "linelist-example.xlsx")
df_ll_raw <- qxl::qread(
  here::here("data-raw", "linelist-example.xlsx"),
  sheet = "Linelist patients"
)

bin_ages <- function(
    df,
    age_var,
    age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
    age_labs = c("<5", "5-17", "18-24", "25-34", "35-49", "50+")
) {
  dplyr::mutate(
    df,
    age_group = cut(
      .data[[age_var]],
      breaks = age_breaks,
      labels = age_labs,
      include.lowest = TRUE,
      right = FALSE
    ),
    .after = dplyr::any_of(age_var)
  )
}

df_ll <- df_ll_raw %>%
  left_join(
    sf::st_drop_geometry(sf_yem$adm1) %>% select(adm1_name, adm1_pcode = pcode),
    by = c("adm1_origin" = "adm1_name")
  ) %>%
  left_join(
    sf::st_drop_geometry(sf_yem$adm2) %>% select(adm1_name, adm2_name, adm2_pcode = pcode),
    by = c(
      "adm1_origin" = "adm1_name",
      "adm2_origin" = "adm2_name"
    )
  ) %>%
  mutate(
    across(starts_with("date_"), lubridate::as_date),
    age_num = as.numeric(age_num),
    age_years = case_when(
      age_unit == "Year" ~ age_num,
      age_unit == "Months" ~ age_num / 12,
      age_unit == "Day" ~ age_num / 365,
      .default = NA_real_
    )
  ) %>%
  bin_ages(age_var = "age_years") %>% 
  select(
    case_id,
    starts_with("date"),
    sex_id,
    age_years,
    age_group,
    starts_with("adm"),
    fever,
    rash,
    cough,
    oral_lesions,
    muac,
    oedema,
    hospitalised_yn,
    measles_stage,
    oxygen,
    vacci_measles_yn,
    vacci_measles_doses,
    TDR_malaria,
    outcome,
    death_cause
  )

usethis::use_data(df_ll, sf_yem, overwrite = TRUE)
