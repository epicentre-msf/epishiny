## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(obtdata)
library(rmapshaper)
library(fs)

# Yemen admin 1 and 2
dir_geo <- max(dir_ls("~/MSF/OutbreakTools - GeoBase/YEM"))
ref <- read_rds(path(dir_geo, "adm_reference_YEM.rds")) %>%
  filter(level %in% 1:2) %>%
  select(-adm3_name, -adm4_name)

dir_shps <- path(max(dir_ls("~/MSF/OutbreakTools - GeoBase/YEM")), "sf")
sf_yem <-
  dir_ls(dir_shps, regexp = "adm[1-2]") %>%
  set_names(path_file(.) %>% path_ext_remove() %>% str_remove("YEM_")) %>%
  map(read_rds) %>%
  map(rmapshaper::ms_simplify, keep = 0.01, keep_shapes = TRUE, explode = FALSE, sys = FALSE)

geo_yem <- c(
  list(ref = ref),
  sf_yem
)

# path to example Outbreak Tools linelist export: a measles dataset with linelist "Linelist patients"
# path_export <- system.file("extdata", "ll_export.xlsx", package = "obtdata")
# ll_export <- ll_read(path_export) %>% ll_translate(from = "FR", to = "EN")

path_ll <- here::here("data-raw", "ll_export_measles_Yemen_20230127 English 20230127-1520.xlsx")
df_ll_raw <- qxl::qread(
  here::here("data-raw", "ll_export_measles_Yemen_20230127 English 20230127-1520.xlsx"),
  sheet = "Linelist patients"
)

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
  select(
    case_id,
    starts_with("date"),
    sex_id,
    age_years,
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
