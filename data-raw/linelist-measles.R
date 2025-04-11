# code to prepare package example data
library(tidyverse)
library(fs)
library(sf)

# Geo boundary data Chad admin 1 + 2 + 4
iso <- "TCD"
lvls <- c(1:2)
shp_dir <- path(Sys.getenv("SHAREPOINT_PATH"), "OutbreakTools - GeoBase", iso, "TCD__ALL__20240507__123")
shp_paths <- str_glue("{shp_dir}/sf/{iso}_adm{lvls}.rds")
sf_tcd <- set_names(shp_paths, str_glue("adm{lvls}")) |>
  map(read_rds) |>
  map(\(x) filter(x, adm1_name == "Mandoul"))

shp_dir_adm4 <- path(Sys.getenv("SHAREPOINT_PATH"), "OutbreakTools - GeoBase", iso, "TCD__ALL__20231122__124H")
sf_tcd$adm4 <- read_rds(str_glue("{shp_dir_adm4}/sf/{iso}_adm4.rds")) |> filter(adm1_name == "Mandoul")

geo_ref <- sf_tcd$adm4 |>
  sf::st_drop_geometry() |>
  select(adm1_name, adm2_name, adm4_name, adm4_pcode = pcode) |>
  left_join(
    sf_tcd$adm2 |>
      sf::st_drop_geometry() |>
      select(adm1_name, adm2_name, adm2_pcode = pcode),
    by = join_by(adm1_name, adm2_name)
  ) |>
  left_join(
    sf_tcd$adm1 |>
      sf::st_drop_geometry() |>
      select(adm1_name, adm1_pcode = pcode),
    by = join_by(adm1_name)
  ) |>
  select(adm1_name, adm2_name, adm4_name, adm1_pcode, adm2_pcode, adm4_pcode)

# simulated measles ll data
if (!rlang::is_installed("episimdata")) pak::pak("epicentre-msf/episimdata")

df_ll_measles <- episimdata::moissala_linelist_clean_EN |>
  # join pcode to ll
  left_join(
    geo_ref,
    by = join_by(region == adm1_name, sub_prefecture == adm2_name, village_commune == adm4_name)
  )

usethis::use_data(df_ll_measles, sf_tcd, overwrite = TRUE)
